set_cols <- c("q5_car", "q5_bus", "q5_bike", "q5_none", "q5_dk", "q5_pna")
keep <- c(q5_cols, "q5_status")

recode_example <- function(yes, no) {
  dat <- cb_example
  dat[set_cols] <- lapply(dat[set_cols], function(x) ifelse(x == 1, yes, no))
  dat
}

test_that("common codings are recognised without `checked`", {
  expected <- clean_example()[keep]
  codings <- list(
    c("TRUE", "FALSE"), c("true", "false"), c("T", "F"),
    c("Yes", "No"), c("YES", "NO"), c("y", "n"),
    c("Checked", "Unchecked"), c("checked", "unchecked"),
    c("Selected", "Not selected"), c(" Yes ", "No "), c("1", "0")
  )
  for (coding in codings) {
    dat <- recode_example(coding[1], coding[2])
    cleaned <- cb_clean(dat, q5_sets(), quiet = TRUE)
    expect_equal(cleaned[keep], expected, info = coding[1])
    expect_identical(cleaned$q5_dk, dat$q5_dk)
    expect_identical(cb_clean(cleaned, q5_sets(), quiet = TRUE), cleaned)
  }
})

test_that("columns of one set may mix codings", {
  dat <- cb_example
  dat$q5_car <- dat$q5_car == 1
  dat$q5_bus <- ifelse(dat$q5_bus == 1, "Yes", "No")
  dat$q5_dk <- factor(ifelse(dat$q5_dk == 1, "Checked", "Unchecked"))
  expect_equal(cb_clean(dat, q5_sets(), quiet = TRUE)[keep], clean_example()[keep])
})

test_that("ambiguous codings need `checked` and `unchecked`", {
  dat <- recode_example(1, 2)
  expect_error(cb_check(dat, q5_sets()), "ticked, unticked")
  expect_equal(
    cb_clean(dat, q5_sets(checked = 1, unchecked = 2), quiet = TRUE)[keep],
    clean_example()[keep]
  )
  # explicit values are matched exactly, and override the automatic ones
  flipped <- recode_example(0, 1)
  expect_equal(
    cb_clean(flipped, q5_sets(checked = 0, unchecked = 1), quiet = TRUE)[keep],
    clean_example()[keep]
  )
  yes_only <- recode_example("Oui", "No")
  expect_equal(
    cb_clean(yes_only, q5_sets(checked = "Oui"), quiet = TRUE)[keep],
    clean_example()[keep]
  )
})

test_that("empty strings are blanks, and blanks are not_shown by default", {
  dat <- recode_example("Yes", "No")
  dat$q5_bus[2] <- ""
  dat$q5_car[8] <- NA
  cleaned <- cb_clean(dat, q5_sets(), quiet = TRUE)
  expect_equal(as.character(cleaned$q5_status[c(2, 8)]), c("not_shown", "not_shown"))
})

test_that("blank = 'unticked' reads 1 / NA exports", {
  dat <- recode_example(1, NA)
  sets <- q5_sets(blank = "unticked")
  cleaned <- cb_clean(dat, sets, quiet = TRUE)
  expect_equal(cleaned[keep], clean_example()[keep])
  expect_identical(cleaned$q5_dk, dat$q5_dk)
  expect_identical(cb_clean(cleaned, sets, quiet = TRUE), cleaned)
  expect_equal(cb_check(dat, sets)[-1], cb_check(cb_example, q5_sets())[-1])
  expect_equal(cb_count(cleaned, sets), c(2L, 1L, 0L, NA, NA, NA, 1L, 2L))

  # without it, every row has a blank and is not_shown
  expect_true(all(cb_clean(dat, q5_sets(), quiet = TRUE)$q5_status == "not_shown"))
})

test_that("blank = 'unticked' reads option text in ticked cells", {
  dat <- cb_example
  for (col in set_cols) {
    dat[[col]] <- ifelse(dat[[col]] == 1, sub("q5_", "", col), "")
  }
  sets <- q5_sets(blank = "unticked")
  cleaned <- cb_clean(dat, sets, quiet = TRUE)
  expect_equal(cleaned[keep], clean_example()[keep])
  expect_identical(cb_clean(cleaned, sets, quiet = TRUE), cleaned)
})

test_that("blank = 'unticked' still honours explicit values and logicals", {
  dat <- recode_example("x", NA)
  dat$q5_bus[1] <- "?"
  expect_error(cb_check(dat, q5_sets(blank = "unticked", checked = "x")), "q5_bus")

  lgl <- recode_example(TRUE, NA)
  expect_equal(
    cb_clean(lgl, q5_sets(blank = "unticked"), quiet = TRUE)[keep],
    clean_example()[keep]
  )
  expect_error(cb_define("q", "Q", a, blank = "zero"), "blank")
})
