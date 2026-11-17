#' Count the substantive options each respondent ticked
#'
#' @inheritParams cb_clean
#' @param which Name of the set to use. Needed only when `sets` holds more
#'   than one set.
#'
#' @return An integer vector with one element per row of `data`. `NA` where
#'   any substantive column is `NA`, which on cleaned data means the
#'   respondent gave no usable answer.
#' @export
#' @examples
#' sets <- cb_sets(cb_define(
#'   "q5", "Transport used", starts_with("q5_"),
#'   none = q5_none, dk = q5_dk, pna = q5_pna
#' ))
#' cleaned <- cb_clean(cb_example, sets, quiet = TRUE)
#' cb_count(cleaned, sets)
cb_count <- function(data, sets, which = NULL) {
  check_data(data)
  set <- single_set(sets, which)
  rs <- resolve_set(set, data)
  st <- set_state(data, rs, set)
  as.integer(rowSums(st$mat[, rs$sub, drop = FALSE]))
}

#' Paste the ticked option labels into one string per respondent
#'
#' @inheritParams cb_count
#' @param sep Separator between labels.
#' @param none If `TRUE` (default), a ticked "none of the above" column
#'   contributes its label. If `FALSE`, only substantive options are used.
#'
#' @return A character vector with one element per row of `data`. `NA` where
#'   the set is `NA`, and `""` where the respondent answered but ticked
#'   nothing.
#' @export
#' @examples
#' sets <- cb_sets(cb_define(
#'   "q5", "Transport used", starts_with("q5_"),
#'   option_labels = c(q5_car = "Car", q5_bus = "Bus", q5_bike = "Bike",
#'                     q5_none = "None of the above"),
#'   none = q5_none, dk = q5_dk, pna = q5_pna
#' ))
#' cleaned <- cb_clean(cb_example, sets, quiet = TRUE)
#' cb_collapse(cleaned, sets)
cb_collapse <- function(data, sets, which = NULL, sep = ", ", none = TRUE) {
  check_data(data)
  set <- single_set(sets, which)
  rs <- resolve_set(set, data)
  st <- set_state(data, rs, set)
  cols <- c(rs$sub, if (none) rs$none)
  mat <- st$mat[, cols, drop = FALSE]
  labels <- unname(rs$labels[cols])
  out <- vapply(seq_len(nrow(mat)), function(i) {
    paste(labels[mat[i, ] == 1L], collapse = sep)
  }, character(1))
  out[rowSums(is.na(mat)) > 0] <- NA_character_
  out
}

#' Reshape checkbox sets to one row per tick, for plotting only
#'
#' Returns one row for every ticked option, with the other columns of `data`
#' repeated. This is convenient for `ggplot2::geom_bar()` and similar.
#'
#' @section Warning:
#' **Do not pass the output to `gtsummary::tbl_summary()`** or use it to
#' compute percentages. Respondents who ticked several options appear several
#' times and respondents who ticked nothing disappear, so the row count is
#' the number of ticks, not the number of respondents, and every other
#' variable is distorted. Use [cb_tbl_summary()] on the wide data instead.
#'
#' @inheritParams cb_clean
#' @param none If `TRUE` (default), ticks of "none of the above" columns are
#'   included. "Don't know" and "prefer not to answer" are never included.
#'
#' @return A tibble with the columns of `data` that are not part of any set,
#'   followed by `set` (set name), `option` (column name) and `label`
#'   (display label, a factor in option order). Rows are ordered by
#'   respondent, then option.
#' @export
#' @examples
#' sets <- cb_sets(cb_define(
#'   "q5", "Transport used", starts_with("q5_"),
#'   none = q5_none, dk = q5_dk, pna = q5_pna
#' ))
#' cleaned <- cb_clean(cb_example, sets, quiet = TRUE)
#' cb_long(cleaned, sets)
cb_long <- function(data, sets, none = TRUE) {
  check_data(data)
  sets <- as_sets(sets)
  data <- dplyr::ungroup(data)

  call <- rlang::current_env()
  resolved <- lapply(sets, function(set) {
    rs <- resolve_set(set, data, call = call)
    rs$mat <- set_state(data, rs, set, call = call)$mat
    rs
  })
  set_cols <- unlist(lapply(resolved, function(rs) rs$cols), use.names = FALSE)
  keep <- setdiff(names(data), set_cols)
  clash <- intersect(keep, c("set", "option", "label"))
  if (length(clash) > 0) {
    cli::cli_abort(
      "{.fn cb_long} adds columns {.field set}, {.field option} and {.field label}, but {.arg data} already has {.field {clash}}."
    )
  }

  pieces <- list()
  labels <- character()
  for (rs in resolved) {
    cols <- c(rs$sub, if (none) rs$none)
    labels <- c(labels, unname(rs$labels[cols]))
    for (j in seq_along(cols)) {
      rows <- which(rs$mat[, cols[j]] == 1L)
      pieces[[length(pieces) + 1]] <- tibble::tibble(
        .row = rows, .set = match(rs$name, names(sets)), .opt = j,
        set = rs$name, option = cols[j], label = unname(rs$labels[cols[j]])
      )
    }
  }
  ticks <- dplyr::bind_rows(pieces)
  ticks <- ticks[order(ticks$.row, ticks$.set, ticks$.opt), ]
  ticks$label <- factor(ticks$label, levels = unique(labels))

  out <- tibble::as_tibble(data[ticks$.row, keep, drop = FALSE])
  dplyr::bind_cols(out, ticks[c("set", "option", "label")])
}
