q5_labels <- c(q5_car = "Car", q5_bus = "Bus", q5_bike = "Bike",
               q5_none = "None of the above")

q5_sets <- function(...) {
  cb_sets(cb_define(
    name = "q5", label = "Transport used",
    cols = starts_with("q5_"),
    option_labels = q5_labels,
    none = q5_none, dk = q5_dk, pna = q5_pna,
    ...
  ))
}

clean_example <- function(...) {
  cb_clean(cb_example, q5_sets(), ..., quiet = TRUE)
}

q5_cols <- c("q5_car", "q5_bus", "q5_bike", "q5_none")

# Proportion ticked among non-missing, per option
option_pct <- function(data) {
  vapply(data[q5_cols], function(x) mean(x, na.rm = TRUE), numeric(1))
}

# Rows of a gtsummary table as a plain data frame: label and first stat
tbl_rows <- function(tbl) {
  body <- tbl$table_body
  stat <- grep("^stat_", names(body), value = TRUE)[1]
  data.frame(variable = body$variable, row_type = body$row_type,
             label = body$label, stat = body[[stat]])
}
