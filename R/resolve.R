# Internal: turn a stored set into concrete columns and 0/1 values.

cb_status_levels <- c("answered", "dk", "pna", "skipped", "not_shown", "conflict")

# Values recognised without being told, compared in lower case
auto_checked <- c("1", "true", "t", "yes", "y", "checked", "selected")
auto_unchecked <- c("0", "false", "f", "no", "n", "unchecked", "not selected")

check_data <- function(data, call = rlang::caller_env()) {
  if (!is.data.frame(data)) {
    cli::cli_abort(
      "{.arg data} must be a data frame, not {.obj_type_friendly {data}}.",
      call = call
    )
  }
  invisible(data)
}

# A status column written by cb_clean(): a factor with exactly the status
# levels. Recognised by its levels, so it is found whatever its name is.
is_status_col <- function(x) {
  is.factor(x) && identical(levels(x), cb_status_levels)
}

# Read a status column back. Also accepts a character column holding only
# status values, as happens after a round trip through a CSV file.
as_status <- function(x) {
  if (is_status_col(x)) {
    return(x)
  }
  if ((is.character(x) || is.factor(x)) && all(as.character(x) %in% c(cb_status_levels, NA))) {
    return(factor(as.character(x), levels = cb_status_levels))
  }
  NULL
}

# Resolve one stored selection against the data.
eval_sel <- function(quo, data, set_name, arg, call) {
  if (rlang::quo_is_null(quo)) {
    return(character())
  }
  expr <- rlang::quo_get_expr(quo)
  if (is.character(expr)) {
    quo <- rlang::quo(tidyselect::all_of(!!expr))
  } else if (rlang::is_symbol(expr) && !rlang::as_string(expr) %in% names(data)) {
    # A bare name that is not a column: allow a character vector of names
    val <- tryCatch(rlang::eval_tidy(quo), error = function(e) NULL)
    if (is.character(val)) {
      quo <- rlang::quo(tidyselect::all_of(!!val))
    }
  }
  pos <- tryCatch(
    tidyselect::eval_select(quo, data, allow_rename = FALSE),
    error = function(e) {
      cli::cli_abort(
        "Can't resolve {.arg {arg}} for set {.val {set_name}} against the data.",
        parent = e, call = call
      )
    }
  )
  names(pos)
}

# Resolve a set against the data: columns, one role per column, labels.
# `status_col` is the set's own status column; it and any other status
# column are dropped from the selection so that cleaned data resolves to
# the same columns as raw data.
resolve_set <- function(set, data, status_col = paste0(set$name, "_status"),
                        call = rlang::caller_env()) {
  name <- set$name
  cols <- eval_sel(set$cols, data, name, "cols", call)
  derived <- vapply(cols, function(col) is_status_col(data[[col]]), logical(1))
  cols <- cols[!derived & cols != status_col]
  if (length(cols) == 0) {
    cli::cli_abort("Set {.val {name}} selects no columns in the data.", call = call)
  }

  role_cols <- list(
    none = eval_sel(set$none, data, name, "none", call),
    dk = eval_sel(set$dk, data, name, "dk", call),
    pna = eval_sel(set$pna, data, name, "pna", call)
  )
  outside <- setdiff(unlist(role_cols), cols)
  if (length(outside) > 0) {
    cli::cli_abort(
      "In set {.val {name}}, {cli::qty(length(outside))}column{?s} {.field {outside}} {?has/have} a role but {?is/are} not in {.arg cols}.",
      call = call
    )
  }
  twice <- unique(unlist(role_cols)[duplicated(unlist(role_cols))])
  if (length(twice) > 0) {
    cli::cli_abort(
      "In set {.val {name}}, {cli::qty(length(twice))}column{?s} {.field {twice}} {?has/have} more than one role.",
      call = call
    )
  }

  roles <- rlang::set_names(rep("substantive", length(cols)), cols)
  for (role in names(role_cols)) {
    roles[role_cols[[role]]] <- role
  }
  if (!any(roles == "substantive")) {
    cli::cli_abort("Set {.val {name}} has no substantive columns.", call = call)
  }

  unknown <- setdiff(names(set$option_labels), cols)
  if (length(unknown) > 0) {
    cli::cli_abort(
      "In set {.val {name}}, {.arg option_labels} names {cli::qty(length(unknown))}column{?s} not in the set: {.field {unknown}}.",
      call = call
    )
  }
  labels <- vapply(cols, function(col) {
    if (col %in% names(set$option_labels)) {
      return(set$option_labels[[col]])
    }
    var_label <- attr(data[[col]], "label", exact = TRUE)
    if (rlang::is_string(var_label) && !is.na(var_label)) var_label else col
  }, character(1))

  list(
    name = name,
    label = set$label,
    cols = cols,
    roles = roles,
    labels = labels,
    sub = cols[roles == "substantive"],
    none = cols[roles == "none"],
    dk = cols[roles == "dk"],
    pna = cols[roles == "pna"],
    status_col = status_col
  )
}

# Convert one column to integer 0/1/NA. Returns the values that are neither
# ticked, unticked nor blank in attribute "bad".
#
# `checked` / `unchecked` given: matched exactly. Left NULL: the common
# codings in auto_checked / auto_unchecked are recognised, ignoring case.
# A blank is NA or an empty string. With `blank = "not_shown"` blanks stay
# NA; with `blank = "unticked"` they are 0 and, unless `checked` is given,
# every other value that is not unticked counts as ticked.
normalize_col <- function(x, checked = NULL, unchecked = NULL,
                          blank = "not_shown") {
  if (is.logical(x)) {
    out <- as.integer(x)
    if (blank == "unticked") out[is.na(out)] <- 0L
    return(out)
  }
  chr <- trimws(as.character(x))
  key <- tolower(chr)
  is_blank <- is.na(chr) | chr == ""

  is_checked <- if (is.null(checked)) {
    key %in% setdiff(auto_checked, tolower(as.character(unchecked)))
  } else {
    chr %in% as.character(checked)
  }
  is_unchecked <- if (is.null(unchecked)) {
    key %in% setdiff(auto_unchecked, tolower(as.character(checked)))
  } else {
    chr %in% as.character(unchecked)
  }
  if (blank == "unticked" && is.null(checked)) {
    is_checked <- !is_blank & !is_unchecked
  }

  out <- rep(NA_integer_, length(chr))
  out[is_unchecked] <- 0L
  out[is_checked] <- 1L
  if (blank == "unticked") out[is_blank] <- 0L
  bad <- unique(chr[!is_blank & is.na(out)])
  if (length(bad) > 0) {
    attr(out, "bad") <- bad
  }
  out
}

# The set as an integer 0/1/NA matrix, one column per set column.
# With `cleaned = TRUE` the substantive and none columns were already
# normalized by cb_clean() and hold 0/1; dk and pna columns are never
# rewritten, so they are always read with the set's own `checked` values.
set_matrix <- function(data, rs, set, cleaned = FALSE, call = rlang::caller_env()) {
  n <- nrow(data)
  mat <- matrix(NA_integer_, nrow = n, ncol = length(rs$cols),
                dimnames = list(NULL, rs$cols))
  bad <- list()
  for (col in rs$cols) {
    normalized <- cleaned && rs$roles[[col]] %in% c("substantive", "none")
    v <- if (normalized) {
      normalize_col(data[[col]], 1, 0)
    } else {
      normalize_col(data[[col]], set$checked, set$unchecked, set$blank %||% "not_shown")
    }
    if (!is.null(attr(v, "bad"))) {
      bad[[col]] <- attr(v, "bad")
    }
    mat[, col] <- v
  }
  if (length(bad) > 0) {
    detail <- vapply(names(bad), function(col) {
      vals <- bad[[col]]
      shown <- paste0("\"", vals[seq_len(min(5, length(vals)))], "\"", collapse = ", ")
      if (length(vals) > 5) shown <- paste0(shown, ", ...")
      paste0(col, ": ", shown)
    }, character(1))
    cli::cli_abort(
      c(
        "Set {.val {rs$name}}: {cli::qty(length(bad))}column{?s} {.field {names(bad)}} hold{?s/} values other than ticked, unticked or {.code NA}.",
        rlang::set_names(detail, "x"),
        "i" = "Say what the values mean with {.arg checked} and {.arg unchecked} in {.fn cb_define}, or drop the {cli::qty(length(bad))}column{?s} from the set."
      ),
      call = call
    )
  }
  mat
}

# Matrix, raw pattern and status for one set, honouring an existing status
# column. Rows that an earlier cb_clean() already set to NA keep that
# status: the information needed to classify them again is gone.
set_state <- function(data, rs, set, empty = "na", conflict = "keep",
                      call = rlang::caller_env()) {
  existing <- NULL
  if (rs$status_col %in% names(data)) {
    existing <- as_status(data[[rs$status_col]])
    if (is.null(existing)) {
      cli::cli_abort(
        c(
          "Column {.field {rs$status_col}} already exists and is not a status column for set {.val {rs$name}}.",
          "i" = "Rename it or choose another {.arg status_suffix}."
        ),
        call = call
      )
    }
  }
  mat <- set_matrix(data, rs, set, cleaned = !is.null(existing), call = call)
  cl <- classify_status(mat, rs$roles, empty = empty, conflict = conflict)
  locked <- rep(FALSE, nrow(data))
  if (!is.null(existing)) {
    locked <- !is.na(existing) & existing != "answered"
    cl$status[locked] <- existing[locked]
    back <- c(dk = "dk", pna = "pna", skipped = "empty",
              not_shown = "not_shown", conflict = "conflict")
    cl$pattern[locked] <- back[as.character(existing[locked])]
  }
  list(mat = mat, status = cl$status, pattern = cl$pattern, locked = locked)
}

# Pick one set out of several for the single-set helpers.
single_set <- function(sets, which = NULL, call = rlang::caller_env()) {
  sets <- as_sets(sets, call = call)
  if (is.null(which)) {
    if (length(sets) > 1) {
      cli::cli_abort(
        c(
          "{.arg sets} holds {length(sets)} sets; choose one with {.arg which}.",
          "i" = "Available sets: {.val {names(sets)}}."
        ),
        call = call
      )
    }
    return(sets[[1]])
  }
  if (!rlang::is_string(which) || !which %in% names(sets)) {
    cli::cli_abort(
      c(
        "{.arg which} must be the name of one set in {.arg sets}.",
        "i" = "Available sets: {.val {names(sets)}}."
      ),
      call = call
    )
  }
  sets[[which]]
}
