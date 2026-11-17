#' Report problems in checkbox sets before cleaning
#'
#' Counts, for each set, how many respondents fall under each cleaning rule.
#' Nothing is changed. Run it on the raw data to see what [cb_clean()] will
#' do and to find rows that need a decision.
#'
#' `cb_check()` stops with an error if a set column holds a value that is
#' neither ticked, unticked nor `NA`. It prints a note for every set without
#' a "none of the above" column, because in such a set an all-unticked row
#' cannot be told apart from a skipped question.
#'
#' @inheritParams cb_clean
#'
#' @return A tibble with one row per set and the columns
#' \describe{
#'   \item{set, label}{Set name and question label.}
#'   \item{n}{Number of respondents.}
#'   \item{answered}{A substantive option or "none" ticked, with no conflict.}
#'   \item{dk, pna}{"Don't know" or "prefer not to answer" ticked with no
#'     substantive option.}
#'   \item{all_unticked}{Every box unticked.}
#'   \item{conflict}{A substantive option ticked together with dk, pna or
#'     none.}
#'   \item{not_shown}{At least one set column is `NA`.}
#'   \item{has_none}{Whether the set has a "none of the above" column.}
#' }
#' The seven counts are mutually exclusive and add up to `n`.
#' @export
#' @examples
#' sets <- cb_sets(cb_define(
#'   "q5", "Transport used", starts_with("q5_"),
#'   none = q5_none, dk = q5_dk, pna = q5_pna
#' ))
#' cb_check(cb_example, sets)
cb_check <- function(data, sets, status_suffix = "_status") {
  check_data(data)
  sets <- as_sets(sets)

  call <- rlang::current_env()
  rows <- lapply(sets, function(set) {
    rs <- resolve_set(set, data, status_col = paste0(set$name, status_suffix),
                      call = call)
    st <- set_state(data, rs, set, call = call)
    count <- function(p) sum(st$pattern %in% p)
    tibble::tibble(
      set = rs$name,
      label = rs$label,
      n = nrow(data),
      answered = count(c("substantive", "none")),
      dk = count("dk"),
      pna = count("pna"),
      all_unticked = count("empty"),
      conflict = count("conflict"),
      not_shown = count("not_shown"),
      has_none = length(rs$none) > 0
    )
  })
  out <- dplyr::bind_rows(rows)

  no_none <- out$set[!out$has_none]
  if (length(no_none) > 0) {
    cli::cli_inform(
      c(
        "!" = "Set{?s} {.val {no_none}} {?has/have} no {.arg none} column.",
        "i" = "An all-unticked row may be a skip or a true \"none of these\"; see {.arg empty} in {.fn cb_clean}."
      ),
      class = "checkboxr_no_none"
    )
  }
  out
}
