skip_if_not_installed("gtsummary", "2.1.0")

option_rows <- function(tbl) {
  rows <- tbl_rows(tbl)
  rows <- rows[rows$variable %in% q5_cols, ]
  rlang::set_names(rows$stat, rows$label)
}

test_that("the table has N = 8, the right mean age and the target layout", {
  tbl <- cb_tbl_summary(
    clean_example(), q5_sets(), include = c(age, q5),
    type = age ~ "continuous",
    statistic = age ~ "{mean}", digits = age ~ 1
  )
  expect_s3_class(tbl, "gtsummary")
  expect_s3_class(tbl, "tbl_summary")

  # table N is the number of respondents
  expect_equal(tbl$table_styling$header$modify_stat_N[1], 8)
  expect_match(names(tibble::as_tibble(tbl))[2], "N = 8")

  rows <- tbl_rows(tbl)
  expect_equal(
    rows$label,
    c("age", "Transport used (n = 5)", "Car", "Bus", "Bike", "None of the above")
  )
  expect_equal(rows$row_type[2], "variable_group")
  expect_equal(rows$stat[1], "42.4") # 339 / 8
  expect_equal(
    option_rows(tbl),
    c(Car = "3 (60%)", Bus = "1 (20%)", Bike = "2 (40%)", `None of the above` = "1 (20%)")
  )
  # no "Unknown" rows for the set columns
  expect_false(any(rows$row_type == "missing"))
})

test_that("option rows are indented under the header", {
  tbl <- cb_tbl_summary(clean_example(), q5_sets(), include = q5)
  indent <- tbl$table_styling$indent
  body <- tbl$table_body
  indented <- vapply(seq_len(nrow(indent)), function(i) {
    hit <- rlang::eval_tidy(indent$rows[[i]], body)
    indent$n_spaces[i] > 0 && all(hit[body$variable %in% q5_cols])
  }, logical(1))
  expect_true(any(indented))
})

test_that("percentages follow the cleaning settings", {
  tbl_na <- cb_tbl_summary(clean_example(conflict = "na"), q5_sets(), include = q5)
  expect_equal(
    option_rows(tbl_na),
    c(Car = "2 (50%)", Bus = "1 (25%)", Bike = "2 (50%)", `None of the above` = "1 (25%)")
  )
  expect_equal(tbl_rows(tbl_na)$label[1], "Transport used (n = 4)")

  tbl_zero <- cb_tbl_summary(clean_example(empty = "zero"), q5_sets(), include = q5)
  expect_equal(
    option_rows(tbl_zero),
    c(Car = "3 (50%)", Bus = "1 (17%)", Bike = "2 (33%)", `None of the above` = "1 (17%)")
  )
  expect_equal(tbl_rows(tbl_zero)$label[1], "Transport used (n = 6)")
})

test_that("ordinary variables match plain tbl_summary()", {
  cleaned <- clean_example()
  plain <- gtsummary::tbl_summary(cleaned, include = c(age, group))
  mine <- cb_tbl_summary(cleaned, q5_sets(), include = c(age, group))
  expect_equal(mine$table_body, plain$table_body)

  with_set <- cb_tbl_summary(cleaned, q5_sets(), include = c(age, q5, group))
  keep <- with_set$table_body$variable %in% c("age", "group")
  expect_equal(with_set$table_body[keep, ], plain$table_body)

  plain_by <- gtsummary::tbl_summary(cleaned, include = age, by = group,
                                     missing = "always")
  mine_by <- cb_tbl_summary(cleaned, q5_sets(), include = c(q5, age), by = group,
                            missing = "always")
  keep <- mine_by$table_body$variable %in% "age"
  expect_equal(mine_by$table_body[keep, ], plain_by$table_body)
})

test_that("the default include shows every set and ordinary column in data order", {
  tbl <- cb_tbl_summary(clean_example(), q5_sets())
  expect_equal(
    unique(tbl$table_body$variable[!is.na(tbl$table_body$variable)]),
    c("id", "age", "group", q5_cols)
  )
})

test_that("show_none and show_status control the extra rows", {
  cleaned <- clean_example()
  no_none <- cb_tbl_summary(cleaned, q5_sets(), include = q5, show_none = FALSE)
  expect_equal(names(option_rows(no_none)), c("Car", "Bus", "Bike"))

  with_status <- cb_tbl_summary(cleaned, q5_sets(), include = c(q5, age), show_status = TRUE)
  rows <- tbl_rows(with_status)
  # status sits directly below the set, before the next variable
  expect_equal(unique(rows$variable[!is.na(rows$variable)]),
               c(q5_cols, "q5_status", "age"))
  status <- rows[rows$variable == "q5_status" & rows$row_type == "level", ]
  expect_equal(
    rlang::set_names(status$stat, status$label)[c("answered", "dk", "pna", "skipped")],
    c(answered = "5 (63%)", dk = "1 (13%)", pna = "1 (13%)", skipped = "1 (13%)")
  )
})

test_that("dk and pna columns never appear as option rows", {
  tbl <- cb_tbl_summary(clean_example(), q5_sets())
  expect_false(any(c("q5_dk", "q5_pna") %in% tbl$table_body$variable))
})

test_that("by splits the table and drops the count from the header", {
  tbl <- cb_tbl_summary(clean_example(), q5_sets(), include = c(age, q5), by = group)
  rows <- tbl_rows(tbl)
  expect_true("Transport used" %in% rows$label)
  expect_false(any(grepl("n = ", rows$label)))
  body <- tbl$table_body
  expect_equal(body$stat_1[body$variable %in% "q5_car"], "2 (67%)") # A: 2 of 3
  expect_equal(body$stat_2[body$variable %in% "q5_car"], "1 (50%)") # B: 1 of 2
})

test_that("add_overall(), add_p() and add_n() work and keep the header", {
  skip_if_not_installed("cardx")
  tbl <- cb_tbl_summary(clean_example(), q5_sets(), include = c(age, q5), by = group,
                        type = age ~ "continuous")

  overall <- gtsummary::add_overall(tbl)
  expect_s3_class(overall, "gtsummary")
  expect_equal(overall$table_body$stat_0[overall$table_body$variable %in% "q5_car"], "3 (60%)")
  expect_equal(tbl_rows(overall)$label, tbl_rows(tbl)$label)

  with_p <- gtsummary::add_p(tbl)
  expect_true("p.value" %in% names(with_p$table_body))
  expect_false(anyNA(with_p$table_body$p.value[with_p$table_body$variable %in% q5_cols]))

  chained <- gtsummary::add_n(gtsummary::add_p(gtsummary::add_overall(tbl)))
  expect_equal(tbl_rows(chained)$label, tbl_rows(tbl)$label)
  expect_equal(chained$table_body$n[chained$table_body$variable %in% "q5_car"], "5")
  expect_equal(sum(chained$table_body$row_type == "variable_group"), 1)
})

test_that("the table converts to gt and flextable", {
  tbl <- cb_tbl_summary(clean_example(), q5_sets(), include = c(age, q5))
  skip_if_not_installed("gt")
  expect_s3_class(gtsummary::as_gt(tbl), "gt_tbl")
  skip_if_not_installed("flextable")
  expect_s3_class(gtsummary::as_flex_table(tbl), "flextable")
})

test_that("uncleaned data is refused with a clear message", {
  expect_error(cb_tbl_summary(cb_example, q5_sets()), "cb_clean")
  expect_error(cb_tbl_summary(cb_example, q5_sets()), "q5_status")
})

test_that("a set name that clashes with a column is refused", {
  dat <- clean_example()
  dat$q5 <- 1
  expect_error(cb_tbl_summary(dat, q5_sets()), "also a column")
})

test_that("several sets each get their own header", {
  dat <- cb_example
  dat$q6_a <- c(1, 0, 0, 0, 1, 0, 0, 1)
  dat$q6_b <- c(0, 0, 1, 0, 1, 0, 0, 0)
  sets <- cb_sets(q5_sets(), cb_define("q6", "Second question", starts_with("q6_")))
  cleaned <- cb_clean(dat, sets, quiet = TRUE)
  tbl <- cb_tbl_summary(cleaned, sets, include = c(q6, age, q5))
  rows <- tbl_rows(tbl)
  expect_equal(
    rows$label[rows$row_type == "variable_group"],
    c("Second question (n = 4)", "Transport used (n = 5)")
  )
  expect_equal(rows$stat[rows$variable %in% "q6_a"], "3 (75%)")
  skip_if_not_installed("cardx")
  both <- gtsummary::add_overall(
    cb_tbl_summary(cleaned, sets, include = c(q6, age, q5), by = group)
  )
  expect_equal(sum(both$table_body$row_type == "variable_group"), 2)
  expect_equal(which(both$table_body$row_type == "variable_group")[1], 1)
})
