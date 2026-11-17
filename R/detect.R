#' Guess checkbox sets from column names
#'
#' Groups columns that share a prefix before `sep` (for example REDCap's
#' `transport___1`, `transport___2`) into draft sets and guesses the "none",
#' "don't know" and "prefer not to answer" columns from the text after
#' `sep` and from variable labels.
#'
#' The result is a draft. It is printed so the roles can be reviewed, and it
#' should be corrected by hand (usually by copying it into [cb_define()]
#' calls) before it is used. `cb_detect()` never changes the data.
#'
#' @details
#' A group of columns becomes a set when at least two columns share the
#' prefix and every one of them looks like a checkbox column: logical, or
#' holding only the codings [cb_define()] recognises on its own (0/1,
#' `"Yes"`/`"No"`, `"Checked"`/`"Unchecked"` and so on) or blanks.
#'
#' If the columns of a set contain blanks but no unticked value at all, the
#' draft assumes blanks mean unticked (`blank = "unticked"`), as in exports
#' that write `1` for a ticked box and leave the rest empty.
#' Column names are split at the first occurrence of `sep`.
#'
#' Option labels come from variable labels where they exist. REDCap labels
#' of the form `"Question (choice=Car)"` are shortened to `"Car"`.
#'
#' @param data A data frame.
#' @param sep Text separating the question prefix from the option suffix in
#'   column names. Fixed text, not a regular expression.
#' @param dk_pattern,pna_pattern,none_pattern Regular expressions, matched
#'   without regard to case against the option suffix and the option label,
#'   that mark the "don't know", "prefer not to answer" and "none of the
#'   above" columns.
#'
#' @return A draft `cb_sets`, printed as a side effect. If no sets are
#'   found, `NULL`, invisibly, with a message.
#' @export
#' @examples
#' draft <- cb_detect(cb_example, sep = "_")
cb_detect <- function(data, sep = "___", dk_pattern = "dk|dont_know|unknown",
                      pna_pattern = "pna|prefer_not|refuse",
                      none_pattern = "none") {
  check_data(data)
  if (!rlang::is_string(sep) || !nzchar(sep)) {
    cli::cli_abort("{.arg sep} must be a single non-empty string.")
  }

  nms <- names(data)
  at <- regexpr(sep, nms, fixed = TRUE)
  has_sep <- at > 1 & at + nchar(sep) <= nchar(nms)
  prefix <- ifelse(has_sep, substr(nms, 1, at - 1), NA_character_)
  suffix <- ifelse(has_sep, substr(nms, at + nchar(sep), nchar(nms)), NA_character_)

  looks_checkbox <- vapply(data, function(x) {
    if (is_status_col(x)) {
      return(FALSE)
    }
    is.null(attr(normalize_col(x), "bad"))
  }, logical(1))
  has_values <- vapply(data, function(x) {
    any(!is.na(x) & trimws(as.character(x)) != "")
  }, logical(1))

  found <- list()
  for (p in unique(prefix[has_sep & looks_checkbox])) {
    idx <- which(has_sep & looks_checkbox & prefix == p)
    if (length(idx) < 2 || !any(has_values[idx])) {
      next
    }
    cols <- nms[idx]
    var_labels <- vapply(cols, function(col) {
      lab <- attr(data[[col]], "label", exact = TRUE)
      if (rlang::is_string(lab) && !is.na(lab)) lab else NA_character_
    }, character(1))
    option_labels <- sub("^.*\\(choice=(.*)\\)\\s*$", "\\1", var_labels)

    hit <- function(pattern) {
      found <- grepl(pattern, suffix[idx], ignore.case = TRUE) |
        (!is.na(option_labels) & grepl(pattern, option_labels, ignore.case = TRUE))
      cols[found]
    }
    dk <- hit(dk_pattern)
    pna <- setdiff(hit(pna_pattern), dk)
    none <- setdiff(hit(none_pattern), c(dk, pna))

    # blanks but never an unticked value: blanks are the unticked boxes
    read <- unlist(lapply(data[cols], normalize_col), use.names = FALSE)
    blank <- if (anyNA(read) && !any(read %in% 0L)) "unticked" else "not_shown"

    labelled <- !is.na(option_labels)
    found[[length(found) + 1]] <- structure(
      list(
        name = p,
        label = p,
        cols = rlang::quo(!!cols),
        option_labels = if (any(labelled)) option_labels[labelled] else NULL,
        none = rlang::quo(!!(if (length(none)) none)),
        dk = rlang::quo(!!(if (length(dk)) dk)),
        pna = rlang::quo(!!(if (length(pna)) pna)),
        checked = NULL,
        unchecked = NULL,
        blank = blank
      ),
      class = "cb_set"
    )
  }

  if (length(found) == 0) {
    cli::cli_inform(c(
      "!" = "No checkbox sets found with separator {.val {sep}}.",
      "i" = "A set needs at least two 0/1-like columns sharing a prefix before {.arg sep}."
    ))
    return(invisible(NULL))
  }

  sets <- do.call(cb_sets, found)
  cli::cli_alert_info(
    "Draft with {length(sets)} set{?s}. Review the roles and labels before using it."
  )
  print(sets)
  invisible(sets)
}
