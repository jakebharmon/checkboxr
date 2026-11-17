test_that("cb_define() builds a set and cb_sets() names it", {
  sets <- q5_sets()
  expect_s3_class(sets, "cb_sets")
  expect_named(sets, "q5")
  expect_s3_class(sets$q5, "cb_set")
  expect_equal(sets$q5$label, "Transport used")
})

test_that("cb_define() validates its arguments", {
  expect_error(cb_define(name = 1, cols = a), "name")
  expect_error(cb_define("q", "Q"), "cols")
  expect_error(cb_define("q", "Q", a, option_labels = c("x", "y")), "option_labels")
  expect_error(cb_define("q", "Q", a, checked = NA), "checked")
  expect_error(cb_define("q", "Q", a, checked = 1, unchecked = 1), "both")
})

test_that("cb_sets() rejects duplicates and non-sets", {
  set <- cb_define("q5", "Q5", starts_with("q5_"))
  expect_error(cb_sets(set, set), "unique")
  expect_error(cb_sets("q5"), "cb_define")
  expect_error(cb_sets(), "at least one")
  expect_named(cb_sets(cb_sets(set), cb_define("q6", "Q6", a)), c("q5", "q6"))
})

test_that("a single set is accepted where cb_sets() output is expected", {
  set <- q5_sets()$q5
  expect_equal(
    cb_clean(cb_example, set, quiet = TRUE),
    clean_example()
  )
  expect_error(cb_clean(cb_example, "q5"), "cb_sets")
})

test_that("columns resolve from tidyselect, strings and character vectors", {
  cols <- c("q5_car", "q5_bus", "q5_bike", "q5_none", "q5_dk", "q5_pna")
  by_vector <- cb_define("q5", "Transport used", cols,
                         option_labels = q5_labels,
                         none = "q5_none", dk = "q5_dk", pna = "q5_pna")
  by_range <- cb_define("q5", "Transport used", q5_car:q5_pna,
                        option_labels = q5_labels,
                        none = q5_none, dk = q5_dk, pna = q5_pna)
  expect_equal(cb_clean(cb_example, by_vector, quiet = TRUE), clean_example())
  expect_equal(cb_clean(cb_example, by_range, quiet = TRUE), clean_example())
})

test_that("resolution errors name the set and the columns", {
  missing_col <- cb_define("q9", "Q9", starts_with("q5_"), dk = q5_nope)
  expect_error(cb_check(cb_example, missing_col), "q9")

  outside <- cb_define("q9", "Q9", c(q5_car, q5_bus), dk = q5_dk)
  expect_error(cb_check(cb_example, outside), "q5_dk")

  twice <- cb_define("q9", "Q9", starts_with("q5_"), dk = q5_dk, pna = q5_dk)
  expect_error(cb_check(cb_example, twice), "more than one role")

  nothing <- cb_define("q9", "Q9", starts_with("zzz"))
  expect_error(cb_check(cb_example, nothing), "no columns")

  bad_label <- cb_define("q9", "Q9", starts_with("q5_"),
                         option_labels = c(q5_train = "Train"))
  expect_error(cb_check(cb_example, bad_label), "q5_train")
})

test_that("option labels fall back to variable labels, then column names", {
  dat <- cb_example
  attr(dat$q5_bus, "label") <- "Bus (variable label)"
  set <- cb_define("q5", "Transport used", starts_with("q5_"),
                   option_labels = c(q5_car = "Car"),
                   none = q5_none, dk = q5_dk, pna = q5_pna)
  cleaned <- cb_clean(dat, set, quiet = TRUE)
  expect_equal(
    cb_collapse(cleaned, set)[c(1, 2)],
    c("Car, Bus (variable label)", "q5_bike")
  )
  # cleaning keeps the variable label
  expect_equal(attr(cleaned$q5_bus, "label"), "Bus (variable label)")
})

test_that("print methods run", {
  expect_output(expect_message(print(q5_sets()), "cb_sets"), NA)
  expect_message(print(q5_sets()), "starts_with")
  expect_message(print(q5_sets()$q5), "Transport used")
})
