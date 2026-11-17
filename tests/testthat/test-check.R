test_that("cb_check() counts each pattern in the raw data", {
  out <- cb_check(cb_example, q5_sets())
  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 1)
  expect_equal(out$set, "q5")
  expect_equal(out$n, 8)
  expect_equal(out$answered, 4)
  expect_equal(out$dk, 1)
  expect_equal(out$pna, 1)
  expect_equal(out$all_unticked, 1)
  expect_equal(out$conflict, 1)
  expect_equal(out$not_shown, 0)
  expect_true(out$has_none)
})

test_that("cb_check() does not change the data and works on cleaned data", {
  before <- cb_example
  cb_check(cb_example, q5_sets())
  expect_identical(cb_example, before)

  out <- cb_check(clean_example(conflict = "na"), q5_sets())
  expect_equal(out$dk, 1)
  expect_equal(out$pna, 1)
  expect_equal(out$all_unticked, 1)
  expect_equal(out$conflict, 1)
  expect_equal(out$answered, 4)
})

test_that("cb_check() counts not_shown rows", {
  dat <- cb_example
  dat$q5_car[1] <- NA
  out <- cb_check(dat, q5_sets())
  expect_equal(out$not_shown, 1)
  expect_equal(out$answered, 3)
})

test_that("cb_check() errors on values that are not ticked, unticked or NA", {
  dat <- cb_example
  dat$q5_bike[2] <- 3
  dat$q5_dk[5] <- 9
  expect_error(cb_check(dat, q5_sets()), "q5_bike")
  expect_error(cb_check(dat, q5_sets()), "q5_dk")
  expect_error(cb_check(dat, q5_sets()), "ticked, unticked")
})

test_that("cb_check() flags sets without a none option", {
  no_none <- cb_define("q5", "Transport used", c(q5_car, q5_bus, q5_bike, q5_dk, q5_pna),
                       dk = q5_dk, pna = q5_pna)
  expect_message(out <- cb_check(cb_example, no_none), "none")
  expect_false(out$has_none)
  expect_no_message(cb_check(cb_example, q5_sets()))
})

test_that("cb_check() returns one row per set", {
  dat <- cb_example
  dat$q6_a <- c(1, 0, 0, 0, 1, 0, 0, 1)
  dat$q6_b <- c(0, 0, 1, 0, 1, 0, 0, 0)
  sets <- cb_sets(q5_sets(), cb_define("q6", "Second question", starts_with("q6_")))
  out <- suppressMessages(cb_check(dat, sets))
  expect_equal(out$set, c("q5", "q6"))
  expect_equal(out$all_unticked, c(1, 4))
})
