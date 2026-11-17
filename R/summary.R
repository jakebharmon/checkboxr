#' Summarize checkbox sets and other variables in one gtsummary table
#'
#' A thin wrapper around [gtsummary::tbl_summary()]. Each set named in
#' `include` appears as one question: a header row with the question label,
#' then one indented row per option. Ordinary variables are passed through
#' untouched. The data stay wide, with one row per respondent, so the table
#' N is always `nrow(data)`.
#'
#' @details
#' `data` must have been through [cb_clean()]. Because the set columns are
#' `NA` for respondents with no usable answer, the denominator of every
#' option row is the number of respondents with status `answered`. That
#' count is appended to the header, as in `Transport used (n = 5)`. With
#' `by`, the count differs by column, so it is left out of the header; use
#' `show_status = TRUE` to show it instead.
#'
#' "Don't know" and "prefer not to answer" columns are never shown as
#' option rows. `show_status = TRUE` adds the status column below the set
#' as a categorical variable, which reports those rates against the full
#' sample.
#'
#' The result is a regular gtsummary table, so `gtsummary::add_p()`,
#' `gtsummary::add_overall()`, `gtsummary::add_n()`, `gtsummary::as_gt()`
#' and `gtsummary::as_flex_table()` work on it. Two cautions:
#'
#' * `add_p()` tests each option row separately, as an independent
#'   dichotomous variable. This ignores the dependence between options of
#'   the same question. For a joint test of a multiple-response question
#'   see the MRCV package.
#' * Functions that style rows by position after the fact, such as
#'   `gtsummary::bold_labels()`, do not know about the header row. Bold the
#'   header with `gtsummary::modify_bold()` if needed.
#'
#' @inheritParams cb_clean
#' @param data A data frame returned by [cb_clean()].
#' @param include <[`tidy-select`][dplyr::dplyr_tidy_select]> What to show,
#'   in order: set names and ordinary columns. The columns of a set cannot
#'   be selected one by one; select the set by its name. The default shows
#'   every set and every ordinary column.
#' @param by <[`tidy-select`][dplyr::dplyr_tidy_select]> A single column to
#'   split the table by, or `NULL`.
#' @param show_none If `TRUE` (default), show the "none of the above" row.
#' @param show_status If `TRUE`, add the status column as a categorical
#'   variable directly below each set.
#' @param ... Passed to [gtsummary::tbl_summary()], for example `statistic`,
#'   `digits` or `missing`. `label`, `type` and `value` are combined with
#'   the ones generated for the set columns.
#'
#' @return A gtsummary table of class
#'   `c("cb_tbl_summary", "tbl_summary", "gtsummary")`.
#' @export
#' @examplesIf rlang::is_installed("gtsummary", version = "2.1.0")
#' sets <- cb_sets(cb_define(
#'   "q5", "Transport used", starts_with("q5_"),
#'   option_labels = c(q5_car = "Car", q5_bus = "Bus", q5_bike = "Bike",
#'                     q5_none = "None of the above"),
#'   none = q5_none, dk = q5_dk, pna = q5_pna
#' ))
#' cleaned <- cb_clean(cb_example, sets, quiet = TRUE)
#'
#' cb_tbl_summary(cleaned, sets, include = c(age, q5), type = age ~ "continuous")
#'
#' cb_tbl_summary(cleaned, sets, include = q5, by = group, show_status = TRUE)
cb_tbl_summary <- function(data, sets, include = tidyselect::everything(), by = NULL,
                           show_none = TRUE, show_status = FALSE, ...,
                           status_suffix = "_status") {
  rlang::check_installed("gtsummary", version = "2.1.0",
                         reason = "to build summary tables.")
  check_data(data)
  sets <- as_sets(sets)
  data <- dplyr::ungroup(data)
  dots <- rlang::list2(...)
  clash <- intersect(names(dots), c("data", "include", "by"))
  if (length(clash) > 0) {
    cli::cli_abort("{.arg {clash}} cannot be passed through {.arg ...}.")
  }

  by_name <- names(tidyselect::eval_select(rlang::enquo(by), data))
  if (length(by_name) > 1) {
    cli::cli_abort("{.arg by} must select a single column, not {length(by_name)}.")
  }

  # Resolve every set and confirm it was cleaned
  call <- rlang::current_env()
  resolved <- lapply(sets, function(set) {
    rs <- resolve_set(set, data, status_col = paste0(set$name, status_suffix),
                      call = call)
    status <- if (rs$status_col %in% names(data)) as_status(data[[rs$status_col]])
    if (is.null(status)) {
      cli::cli_abort(c(
        "Set {.val {rs$name}} has no status column {.field {rs$status_col}}.",
        "i" = "Run {.fn cb_clean} on the data before {.fn cb_tbl_summary}."
      ), call = call)
    }
    shown <- c(rs$sub, if (show_none) rs$none)
    not_clean <- shown[!vapply(shown, function(col) {
      all(data[[col]] %in% c(0, 1, NA))
    }, logical(1))]
    if (length(not_clean) > 0) {
      cli::cli_abort(c(
        "Set {.val {rs$name}}: {cli::qty(length(not_clean))}column{?s} {.field {not_clean}} {?is/are} not 0/1.",
        "i" = "Run {.fn cb_clean} on the data before {.fn cb_tbl_summary}."
      ), call = call)
    }
    rs$shown <- shown
    rs$n_answered <- sum(status == "answered", na.rm = TRUE)
    rs
  })

  # Resolve `include` against a stand-in where each set is a single column
  # named after the set, sitting where the set's first column is.
  proxy <- data
  for (rs in resolved) {
    others <- setdiff(names(proxy), c(rs$cols, rs$status_col))
    if (rs$name %in% others) {
      cli::cli_abort(c(
        "Set name {.val {rs$name}} is also a column of {.arg data}.",
        "i" = "Rename the set or the column so {.arg include} is unambiguous."
      ))
    }
    names(proxy)[match(rs$cols[1], names(proxy))] <- rs$name
    proxy <- proxy[setdiff(names(proxy), c(rs$cols, rs$status_col))]
  }
  selected <- names(tidyselect::eval_select(rlang::enquo(include), proxy))
  selected <- setdiff(selected, by_name)

  vars <- character()
  for (item in selected) {
    if (item %in% names(resolved)) {
      rs <- resolved[[item]]
      vars <- c(vars, rs$shown, if (show_status) rs$status_col)
    } else {
      vars <- c(vars, item)
    }
  }
  if (length(vars) == 0) {
    cli::cli_abort("{.arg include} selects no sets or columns.")
  }
  used <- resolved[intersect(names(resolved), selected)]
  set_cols <- unlist(lapply(used, function(rs) rs$shown), use.names = FALSE)

  # Arguments generated for the set columns, followed by the user's own
  per_col <- function(values) rlang::set_names(as.list(values), set_cols)
  user_list <- function(x) {
    if (is.null(x)) list() else if (rlang::is_formula(x)) list(x) else as.list(x)
  }
  label <- per_col(unlist(lapply(used, function(rs) rs$labels[rs$shown]), use.names = FALSE))
  if (show_status) {
    for (rs in used) {
      label[[rs$status_col]] <- paste0(rs$label, ": response status")
    }
  }
  user_missing <- dots$missing %||% "ifany"
  if (rlang::is_string(user_missing)) {
    user_missing <- rlang::new_formula(quote(tidyselect::everything()), user_missing)
  }
  missing <- c(
    user_list(user_missing),
    if (length(set_cols) > 0) list(tidyselect::all_of(set_cols) ~ "no")
  )
  dots$label <- c(label, user_list(dots$label))
  dots$type <- c(per_col(rep("dichotomous", length(set_cols))), user_list(dots$type))
  dots$value <- c(per_col(rep(1L, length(set_cols))), user_list(dots$value))
  dots$missing <- missing

  tbl <- rlang::inject(gtsummary::tbl_summary(
    data = data,
    by = tidyselect::all_of(by_name),
    include = tidyselect::all_of(vars),
    !!!dots
  ))

  for (rs in used) {
    header <- if (length(by_name) == 0) {
      sprintf("%s (n = %d)", rs$label, rs$n_answered)
    } else {
      rs$label
    }
    tbl <- gtsummary::add_variable_group_header(
      tbl, header = header, variables = tidyselect::all_of(rs$shown)
    )
  }
  class(tbl) <- union("cb_tbl_summary", class(tbl))
  tbl
}

# gtsummary functions that rebuild the table and merge it back by row
# (add_overall(), add_n(), ...) fail when they meet the group header rows.
# These methods lift the headers out, run the gtsummary method, and put the
# headers back above the first row of each set.
without_headers <- function(x, fn, ...) {
  body <- x$table_body
  is_header <- body$row_type %in% "variable_group"
  if (!any(is_header)) {
    return(fn(x, ...))
  }
  headers <- body[is_header, , drop = FALSE]
  # the variable each header sits above
  next_var <- body$variable[which(is_header) + 1L]

  x$table_body <- body[!is_header, , drop = FALSE]
  class(x) <- setdiff(class(x), "cb_tbl_summary")
  x <- fn(x, ...)

  body <- x$table_body
  for (i in seq_len(nrow(headers))) {
    at <- match(next_var[i], body$variable)
    if (is.na(at)) {
      next
    }
    body <- dplyr::bind_rows(
      body[seq_len(at - 1L), , drop = FALSE],
      headers[i, , drop = FALSE],
      body[seq(at, nrow(body)), , drop = FALSE]
    )
  }
  x$table_body <- body
  class(x) <- union("cb_tbl_summary", class(x))
  x
}

#' @exportS3Method gtsummary::add_overall
add_overall.cb_tbl_summary <- function(x, ...) {
  without_headers(x, gtsummary::add_overall, ...)
}

#' @exportS3Method gtsummary::add_n
add_n.cb_tbl_summary <- function(x, ...) {
  without_headers(x, gtsummary::add_n, ...)
}

#' @exportS3Method gtsummary::add_p
add_p.cb_tbl_summary <- function(x, ...) {
  without_headers(x, gtsummary::add_p, ...)
}

#' @exportS3Method gtsummary::add_ci
add_ci.cb_tbl_summary <- function(x, ...) {
  without_headers(x, gtsummary::add_ci, ...)
}

#' @exportS3Method gtsummary::add_difference
add_difference.cb_tbl_summary <- function(x, ...) {
  without_headers(x, gtsummary::add_difference, ...)
}
