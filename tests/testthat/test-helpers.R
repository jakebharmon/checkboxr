test_that("cb_count() counts substantive ticks on cleaned data", {
  expect_identical(
    cb_count(clean_example(), q5_sets()),
    c(2L, 1L, 0L, NA, NA, NA, 1L, 2L)
  )
  # raw data: nothing is NA yet
  expect_identical(
    cb_count(cb_example, q5_sets()),
    c(2L, 1L, 0L, 0L, 0L, 0L, 1L, 2L)
  )
})

test_that("cb_collapse() pastes ticked labels", {
  cleaned <- clean_example()
  expect_equal(
    cb_collapse(cleaned, q5_sets()),
    c("Car, Bus", "Bike", "None of the above", NA, NA, NA, "Car", "Car, Bike")
  )
  expect_equal(
    cb_collapse(cleaned, q5_sets(), sep = " | ", none = FALSE),
    c("Car | Bus", "Bike", "", NA, NA, NA, "Car", "Car | Bike")
  )
  expect_equal(cb_collapse(clean_example(empty = "zero"), q5_sets())[6], "")
})

test_that("cb_long() returns one row per tick", {
  long <- cb_long(clean_example(), q5_sets())
  expect_s3_class(long, "tbl_df")
  expect_equal(nrow(long), 7)
  expect_equal(long$id, c(1, 1, 2, 3, 7, 8, 8))
  expect_equal(
    long$option,
    c("q5_car", "q5_bus", "q5_bike", "q5_none", "q5_car", "q5_car", "q5_bike")
  )
  expect_equal(
    as.character(long$label),
    c("Car", "Bus", "Bike", "None of the above", "Car", "Car", "Bike")
  )
  expect_equal(levels(long$label), c("Car", "Bus", "Bike", "None of the above"))
  expect_true(all(long$set == "q5"))
  expect_true(all(c("age", "group") %in% names(long)))
  expect_false(any(c("q5_car", "q5_dk") %in% names(long)))

  expect_equal(nrow(cb_long(clean_example(), q5_sets(), none = FALSE)), 6)
})

test_that("cb_long() refuses to overwrite existing columns", {
  dat <- clean_example()
  dat$label <- "x"
  expect_error(cb_long(dat, q5_sets()), "label")
})

test_that("helpers need a set name when there are several sets", {
  dat <- cb_example
  dat$q6_a <- c(1, 0, 0, 0, 1, 0, 0, 1)
  dat$q6_b <- c(0, 0, 1, 0, 1, 0, 0, 0)
  sets <- cb_sets(q5_sets(), cb_define("q6", "Second question", starts_with("q6_")))
  cleaned <- cb_clean(dat, sets, quiet = TRUE)

  expect_error(cb_count(cleaned, sets), "which")
  expect_error(cb_count(cleaned, sets, which = "q7"), "q5")
  expect_identical(cb_count(cleaned, sets, which = "q6"), c(1L, NA, 1L, NA, 2L, NA, NA, 1L))
  expect_equal(cb_collapse(cleaned, sets, which = "q6")[5], "q6_a, q6_b")
  expect_equal(nrow(cb_long(cleaned, sets)), 7 + 5)
})
