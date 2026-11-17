# Internal status classifier. A pure function of a 0/1/NA integer matrix
# (one row per respondent, one column per set column) and one role per
# column. Rules are evaluated top to bottom; the first match wins.
#
# Returns a list with
#   pattern: what the raw row looks like, one of "not_shown", "conflict",
#            "dk", "pna", "none", "substantive", "empty"
#   status:  factor with levels cb_status_levels, after applying the
#            `empty` and `conflict` policies
classify_status <- function(mat, roles, empty = c("na", "zero"),
                            conflict = c("keep", "na")) {
  empty <- rlang::arg_match(empty)
  conflict <- rlang::arg_match(conflict)
  stopifnot(is.matrix(mat), length(roles) == ncol(mat))

  ticked <- function(role) {
    rowSums(mat[, roles == role, drop = FALSE] == 1L, na.rm = TRUE) > 0
  }
  sub <- ticked("substantive")
  none <- ticked("none")
  dk <- ticked("dk")
  pna <- ticked("pna")

  pattern <- rep("empty", nrow(mat))
  pattern[sub] <- "substantive"
  pattern[none] <- "none"
  pattern[pna] <- "pna"
  pattern[dk] <- "dk"
  pattern[sub & (dk | pna | none)] <- "conflict"
  pattern[rowSums(is.na(mat)) > 0] <- "not_shown"

  to_status <- c(
    not_shown = "not_shown",
    conflict = if (conflict == "keep") "answered" else "conflict",
    dk = "dk",
    pna = "pna",
    none = "answered",
    substantive = "answered",
    empty = if (empty == "na") "skipped" else "answered"
  )
  list(
    pattern = pattern,
    status = factor(unname(to_status[pattern]), levels = cb_status_levels)
  )
}

#' Set checkbox sets to NA where there is no usable answer
#'
#' Assigns each respondent one status per set, then sets the substantive and
#' "none" columns to `NA` for every status except `answered`. This removes
#' respondents who said "don't know", preferred not to answer or skipped the
#' question from the denominator of every option.
#'
#' @details
#' Rules are evaluated top to bottom; the first match wins.
#'
#' | Pattern in the raw row | Status | Substantive and none columns |
#' |---|---|---|
#' | Any set column is `NA` | `not_shown` | `NA` |
#' | Substantive option ticked with dk, pna or none | see `conflict` | see `conflict` |
#' | dk ticked, no substantive option | `dk` | `NA` |
#' | pna ticked, no substantive option | `pna` | `NA` |
#' | none ticked, no substantive option | `answered` | kept |
#' | At least one substantive option ticked | `answered` | kept |
#' | Every box unticked | `skipped` or `answered`, see `empty` | `NA` or kept |
#'
#' If both dk and pna are ticked with nothing else, the status is `dk`.
#'
#' The substantive and none columns are returned as integer 0/1/`NA`
#' whatever their input type. The dk and pna columns are never changed, so
#' they stay available for reporting non-response.
#'
#' `cb_clean()` is idempotent: running it again on its own output changes
#' nothing. Rows that were already set to `NA` keep their status, because
#' the ticks needed to classify them again are gone. To clean with different
#' `empty` or `conflict` settings, start again from the raw data.
#'
#' @param data A data frame with one row per respondent.
#' @param sets A `cb_sets` from [cb_sets()] or [cb_detect()], or a single set
#'   from [cb_define()].
#' @param empty What an all-unticked row means. `"na"` (default) treats it
#'   as a skip: status `skipped`, columns set to `NA`. `"zero"` treats it as
#'   a real answer of "none of these": status `answered`, zeros kept.
#' @param conflict What to do when a substantive option is ticked together
#'   with dk, pna or none. `"keep"` (default) trusts the substantive ticks:
#'   status `answered`, substantive columns kept, none column set to 0.
#'   `"na"` gives status `conflict` and sets the columns to `NA`. `"error"`
#'   stops and lists the offending rows.
#' @param status_suffix Suffix for the status column, which is named
#'   `<set name><status_suffix>`.
#' @param quiet If `TRUE`, do not print the one-line summary per set.
#'
#' @return `data` with the substantive and none columns of each set recoded,
#'   and one status column per set placed after the set's last column. The
#'   status column is a factor with levels `answered`, `dk`, `pna`,
#'   `skipped`, `not_shown`, `conflict`.
#' @export
#' @examples
#' sets <- cb_sets(cb_define(
#'   "q5", "Transport used", starts_with("q5_"),
#'   none = q5_none, dk = q5_dk, pna = q5_pna
#' ))
#' cb_clean(cb_example, sets)
#'
#' # Discard rows where "don't know" is ticked alongside a real option
#' cb_clean(cb_example, sets, conflict = "na")
cb_clean <- function(data, sets, empty = c("na", "zero"),
                     conflict = c("keep", "na", "error"),
                     status_suffix = "_status", quiet = FALSE) {
  check_data(data)
  sets <- as_sets(sets)
  empty <- rlang::arg_match(empty)
  conflict <- rlang::arg_match(conflict)
  if (!rlang::is_string(status_suffix) || !nzchar(status_suffix)) {
    cli::cli_abort("{.arg status_suffix} must be a single non-empty string.")
  }

  for (set in sets) {
    rs <- resolve_set(set, data, status_col = paste0(set$name, status_suffix))
    st <- set_state(data, rs, set, empty = empty,
                    conflict = if (conflict == "error") "keep" else conflict)

    in_conflict <- st$pattern == "conflict" & !st$locked
    if (conflict == "error" && any(in_conflict)) {
      rows <- which(in_conflict)
      cli::cli_abort(c(
        "Set {.val {rs$name}}: a substantive option is ticked together with dk, pna or none in {cli::qty(length(rows))}row{?s} {rows}.",
        "i" = "Columns involved: {.field {rs$cols}}.",
        "i" = "Fix the rows, or use {.code conflict = \"keep\"} or {.code conflict = \"na\"}."
      ))
    }

    usable <- st$status == "answered"
    for (col in c(rs$sub, rs$none)) {
      v <- st$mat[, col]
      v[!usable] <- NA_integer_
      if (col %in% rs$none) {
        # kept conflicts trust the substantive ticks over "none"
        v[usable & in_conflict] <- 0L
      }
      attr(v, "label") <- attr(data[[col]], "label", exact = TRUE)
      data[[col]] <- v
    }

    is_new <- !rs$status_col %in% names(data)
    data[[rs$status_col]] <- st$status
    if (is_new) {
      data <- dplyr::relocate(
        data, tidyselect::all_of(rs$status_col),
        .after = tidyselect::all_of(rs$cols[length(rs$cols)])
      )
    }

    if (!quiet) {
      inform_clean(rs, st$status, sum(in_conflict & usable))
    }
  }
  data
}

inform_clean <- function(rs, status, n_kept_conflict) {
  n <- length(status)
  counts <- table(status)[c("dk", "pna", "skipped", "not_shown", "conflict")]
  counts <- counts[counts > 0]
  n_na <- sum(counts)
  why <- if (n_na > 0) {
    paste0(" (", paste(counts, names(counts), collapse = ", "), ")")
  } else {
    ""
  }
  kept <- if (n_kept_conflict > 0) {
    paste0("; ", n_kept_conflict, " conflict", if (n_kept_conflict > 1) "s",
           " kept as answered")
  } else {
    ""
  }
  cli::cli_inform(
    c("i" = "{.strong {rs$name}}: {n_na} of {n} respondent{?s} set to {.code NA}{why}{kept}."),
    class = "checkboxr_clean_summary"
  )
}
