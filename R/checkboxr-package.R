#' checkboxr: clean and summarize select-all-that-apply survey questions
#'
#' The intended pipeline is define, check, clean, summarize:
#'
#' 1. [cb_define()] and [cb_sets()] (or [cb_detect()] for a first draft)
#'    declare which columns form one checkbox question and what each means.
#' 2. [cb_check()] reports problems before anything is changed.
#' 3. [cb_clean()] sets a question to `NA` for respondents with no usable
#'    answer and records why in a status column.
#' 4. [cb_tbl_summary()] builds a [gtsummary::tbl_summary()] table with one
#'    header per question and one row per option.
#'
#' [cb_count()], [cb_collapse()] and [cb_long()] are small helpers.
#'
#' @keywords internal
#' @importFrom rlang %||%
"_PACKAGE"

#' Toy checkbox survey data
#'
#' Eight respondents answering one select-all-that-apply question, "Transport
#' used". Each row exercises one cleaning rule of [cb_clean()].
#'
#' @format A tibble with 8 rows and 9 columns:
#' \describe{
#'   \item{id}{Respondent id.}
#'   \item{age}{Age in years.}
#'   \item{group}{Group, `"A"` or `"B"`.}
#'   \item{q5_car, q5_bus, q5_bike}{Substantive options, 0/1.}
#'   \item{q5_none}{"None of the above", 0/1.}
#'   \item{q5_dk}{"Don't know", 0/1.}
#'   \item{q5_pna}{"Prefer not to answer", 0/1.}
#' }
#'
#' Rows 1, 2 and 8 are ordinary answers, row 3 ticked "none of the above",
#' row 4 "don't know", row 5 "prefer not to answer", row 6 ticked nothing and
#' row 7 ticked both "car" and "don't know".
#'
#' @examples
#' cb_example
"cb_example"
