#' Declare one checkbox set
#'
#' A checkbox set is the metadata for one select-all-that-apply question:
#' which columns belong to it, how each option is labelled, and which columns
#' are "none of the above", "don't know" and "prefer not to answer".
#'
#' @details
#' Each column in `cols` gets exactly one role:
#'
#' * `substantive`: a real option, shown in tables. This is every column not
#'   named in `none`, `dk` or `pna`.
#' * `none`: "none of the above". A real answer; the respondent stays in the
#'   denominator.
#' * `dk`: don't know. Not a usable answer.
#' * `pna`: prefer not to answer. Not a usable answer.
#'
#' `cols`, `none`, `dk` and `pna` are tidyselect expressions. They are stored
#' unevaluated and resolved against the data each time the set is used, so a
#' set can be defined before the data exists. Status columns added by
#' [cb_clean()] are always excluded from the selection, so
#' `starts_with("q5_")` keeps working on cleaned data.
#'
#' @section How ticks are read:
#' With `checked` and `unchecked` left at `NULL`, the usual codings are
#' recognised without being told, ignoring case and surrounding spaces:
#'
#' | Ticked | Unticked |
#' |---|---|
#' | `1` | `0` |
#' | `TRUE`, `"TRUE"`, `"T"` | `FALSE`, `"FALSE"`, `"F"` |
#' | `"Yes"`, `"Y"` | `"No"`, `"N"` |
#' | `"Checked"` | `"Unchecked"` |
#' | `"Selected"` | `"Not selected"` |
#'
#' Columns of one set may mix these. Any other coding, such as 1 = yes and
#' 2 = no, must be spelled out with `checked` and `unchecked`, which are then
#' matched exactly. A value that is neither ticked, unticked nor blank is an
#' error, so a miscoded column is never read silently.
#'
#' A blank cell (`NA` or an empty string) normally means the question was
#' not shown. Some exports instead leave unticked boxes blank and write `1`
#' or the option text in ticked ones. For those use `blank = "unticked"`:
#' blanks count as unticked and, unless `checked` is given, any other value
#' counts as ticked. A row that is blank throughout is then an all-unticked
#' row and follows `empty` in [cb_clean()].
#'
#' @param name Short id for the set, such as `"q5"`. Used as the prefix for
#'   the status column and to refer to the set in [cb_tbl_summary()].
#' @param label Question text shown as the table header. Defaults to `name`.
#' @param cols <[`tidy-select`][dplyr::dplyr_tidy_select]> The columns in the
#'   set, or a character vector of column names.
#' @param option_labels Named character vector of display labels,
#'   `c(column = "Label")`. Columns left out use their variable label
#'   (the `"label"` attribute) if they have one, then the column name.
#' @param none,dk,pna <[`tidy-select`][dplyr::dplyr_tidy_select]> Columns
#'   within `cols` that hold those roles. Each may be `NULL`.
#' @param checked,unchecked Value or values meaning ticked and unticked.
#'   Leave both `NULL` to recognise the usual codings automatically; see
#'   "How ticks are read".
#' @param blank What a blank cell (`NA` or `""`) means: `"not_shown"`
#'   (default) or `"unticked"`.
#'
#' @return A `cb_set` object. Combine sets with [cb_sets()].
#' @export
#' @examples
#' q5 <- cb_define(
#'   name = "q5", label = "Transport used",
#'   cols = starts_with("q5_"),
#'   option_labels = c(q5_car = "Car", q5_bus = "Bus", q5_bike = "Bike",
#'                     q5_none = "None of the above"),
#'   none = q5_none, dk = q5_dk, pna = q5_pna
#' )
#' q5
cb_define <- function(name, label = name, cols, option_labels = NULL,
                      none = NULL, dk = NULL, pna = NULL,
                      checked = NULL, unchecked = NULL,
                      blank = c("not_shown", "unticked")) {
  if (!rlang::is_string(name) || is.na(name) || !nzchar(name)) {
    cli::cli_abort("{.arg name} must be a single non-empty string.")
  }
  if (!rlang::is_string(label) || is.na(label)) {
    cli::cli_abort("{.arg label} must be a single string, in set {.val {name}}.")
  }
  cols <- rlang::enquo(cols)
  if (rlang::quo_is_missing(cols) || rlang::quo_is_null(cols)) {
    cli::cli_abort("{.arg cols} must select the columns of set {.val {name}}.")
  }
  if (!is.null(option_labels)) {
    ok <- is.character(option_labels) && rlang::is_named(option_labels) &&
      !anyNA(option_labels) && !anyDuplicated(names(option_labels))
    if (!ok) {
      cli::cli_abort(c(
        "{.arg option_labels} must be a named character vector, in set {.val {name}}.",
        "i" = "Use the form {.code c(column = \"Label\")} with each column named once."
      ))
    }
  }
  blank <- rlang::arg_match(blank)
  if (!is.null(checked)) check_values(checked, "checked", name)
  if (!is.null(unchecked)) check_values(unchecked, "unchecked", name)
  if (!is.null(checked) && !is.null(unchecked)) {
    both <- intersect(as.character(checked), as.character(unchecked))
    if (length(both) > 0) {
      cli::cli_abort(
        "{.val {both}} cannot be both {.arg checked} and {.arg unchecked}, in set {.val {name}}."
      )
    }
  }

  structure(
    list(
      name = name,
      label = label,
      cols = cols,
      option_labels = option_labels,
      none = rlang::enquo(none),
      dk = rlang::enquo(dk),
      pna = rlang::enquo(pna),
      checked = checked,
      unchecked = unchecked,
      blank = blank
    ),
    class = "cb_set"
  )
}

check_values <- function(x, arg, name) {
  if (!is.atomic(x) || is.null(x) || length(x) == 0 || anyNA(x)) {
    cli::cli_abort(
      "{.arg {arg}} must be a non-empty vector with no missing values, in set {.val {name}}."
    )
  }
  invisible(x)
}

#' Combine checkbox sets into one object
#'
#' A `cb_sets` is a named list of sets made with [cb_define()]. Every other
#' function in the package takes the data plus this object. It is passed
#' explicitly; it is never stored on the data frame.
#'
#' @param ... Sets made with [cb_define()], or existing `cb_sets` objects to merge.
#'
#' @return A `cb_sets` object: a list of sets named by set name.
#' @export
#' @examples
#' sets <- cb_sets(
#'   cb_define("q5", "Transport used", starts_with("q5_"),
#'             none = q5_none, dk = q5_dk, pna = q5_pna)
#' )
#' sets
cb_sets <- function(...) {
  sets <- list()
  for (x in rlang::list2(...)) {
    if (inherits(x, "cb_set")) {
      sets <- c(sets, list(x))
    } else if (inherits(x, "cb_sets")) {
      sets <- c(sets, unclass(x))
    } else {
      cli::cli_abort(c(
        "Every input to {.fn cb_sets} must be a set made with {.fn cb_define} or a {.cls cb_sets}.",
        "x" = "Got an object of class {.cls {class(x)}}."
      ))
    }
  }
  if (length(sets) == 0) {
    cli::cli_abort("{.fn cb_sets} needs at least one set.")
  }
  nms <- vapply(sets, function(s) s$name, character(1))
  if (anyDuplicated(nms)) {
    cli::cli_abort("Set names must be unique. Duplicated: {.val {unique(nms[duplicated(nms)])}}.")
  }
  structure(rlang::set_names(sets, nms), class = "cb_sets")
}

as_sets <- function(sets, call = rlang::caller_env()) {
  if (inherits(sets, "cb_sets")) {
    return(sets)
  }
  if (inherits(sets, "cb_set")) {
    return(cb_sets(sets))
  }
  cli::cli_abort(
    "{.arg sets} must be a {.cls cb_sets} made with {.fn cb_sets}, not {.obj_type_friendly {sets}}.",
    call = call
  )
}

#' @export
`[.cb_sets` <- function(x, i) {
  structure(unclass(x)[i], class = "cb_sets")
}

#' @export
c.cb_sets <- function(...) {
  cb_sets(...)
}

# Text shown for a stored selection when printing
sel_text <- function(quo) {
  if (rlang::quo_is_null(quo)) {
    return(NULL)
  }
  expr <- rlang::quo_get_expr(quo)
  if (is.character(expr)) {
    return(paste(expr, collapse = ", "))
  }
  paste(rlang::expr_deparse(expr, width = 500L), collapse = " ")
}

set_bullets <- function(x) {
  roles <- c(cols = sel_text(x$cols), none = sel_text(x$none),
             dk = sel_text(x$dk), pna = sel_text(x$pna))
  out <- sprintf("%s: %s", names(roles), roles)
  if (!is.null(x$checked)) {
    out <- c(out, sprintf("checked: %s", paste(as.character(x$checked), collapse = ", ")))
  }
  if (!is.null(x$unchecked)) {
    out <- c(out, sprintf("unchecked: %s", paste(as.character(x$unchecked), collapse = ", ")))
  }
  if (identical(x$blank, "unticked")) {
    out <- c(out, "blank: unticked")
  }
  if (!is.null(x$option_labels)) {
    out <- c(out, sprintf(
      "labels: %s",
      paste0(names(x$option_labels), " = \"", x$option_labels, "\"", collapse = ", ")
    ))
  }
  out
}

#' @export
print.cb_set <- function(x, ...) {
  cli::cli_text("{.cls cb_set} {.val {x$name}}: {x$label}")
  cli::cli_bullets(rlang::set_names(set_bullets(x), "*"))
  invisible(x)
}

#' @export
print.cb_sets <- function(x, ...) {
  cli::cli_text("{.cls cb_sets} with {length(x)} set{?s}")
  for (set in x) {
    cli::cli_text("{.strong {set$name}}: {set$label}")
    cli::cli_bullets(rlang::set_names(set_bullets(set), "*"))
  }
  invisible(x)
}
