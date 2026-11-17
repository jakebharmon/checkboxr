test_that("cb_detect() drafts the toy set and guesses roles", {
  expect_message(draft <- cb_detect(cb_example, sep = "_"), "Draft")
  expect_s3_class(draft, "cb_sets")
  expect_named(draft, "q5")

  # the draft behaves like the hand-written sets
  cleaned <- cb_clean(cb_example, draft, quiet = TRUE)
  expect_equal(cleaned$q5_status, clean_example()$q5_status)
  expect_equal(cb_count(cleaned, draft), cb_count(clean_example(), q5_sets()))
  expect_equal(cb_check(cb_example, draft)[-2], cb_check(cb_example, q5_sets())[-2])
})

test_that("cb_detect() never changes the data", {
  before <- cb_example
  suppressMessages(cb_detect(cb_example, sep = "_"))
  expect_identical(cb_example, before)
})

test_that("cb_detect() handles REDCap-style names, labels and strings", {
  dat <- tibble::tibble(
    record_id = 1:3,
    transport___1 = c("Checked", "Unchecked", "Unchecked"),
    transport___2 = c("Unchecked", "Checked", "Unchecked"),
    transport___88 = c("Unchecked", "Unchecked", "Checked"),
    transport___99 = c("Unchecked", "Unchecked", "Unchecked"),
    notes___x = c("a", "b", "c"),
    single___1 = c(0, 1, 0)
  )
  attr(dat$transport___1, "label") <- "Transport used (choice=Car)"
  attr(dat$transport___2, "label") <- "Transport used (choice=Bus)"
  attr(dat$transport___88, "label") <- "Transport used (choice=Unknown)"
  attr(dat$transport___99, "label") <- "Transport used (choice=Prefer not to say)"

  draft <- suppressMessages(cb_detect(dat, pna_pattern = "prefer not"))
  expect_named(draft, "transport")
  expect_null(draft$transport$checked)
  expect_equal(draft$transport$blank, "not_shown")

  cleaned <- cb_clean(dat, draft, quiet = TRUE)
  expect_equal(as.character(cleaned$transport_status), c("answered", "answered", "dk"))
  expect_equal(cb_collapse(cleaned, draft), c("Car", "Bus", NA))
  expect_true(suppressMessages(cb_check(dat, draft))$has_none == FALSE)
})

test_that("cb_detect() reports when nothing is found", {
  expect_message(out <- cb_detect(cb_example), "No checkbox sets")
  expect_null(out)
  expect_error(cb_detect(cb_example, sep = ""), "sep")
})

test_that("cb_detect() ignores status columns on cleaned data", {
  draft <- suppressMessages(cb_detect(clean_example(), sep = "_"))
  expect_equal(
    rlang::quo_get_expr(draft$q5$cols),
    c("q5_car", "q5_bus", "q5_bike", "q5_none", "q5_dk", "q5_pna")
  )
})

test_that("cb_detect() guesses blank = 'unticked' for 1 / blank exports", {
  dat <- tibble::tibble(
    q1_a = c(1, NA, NA, 1),
    q1_b = c(NA, 1, NA, NA),
    q1_c = c(NA, NA, NA, NA)
  )
  draft <- suppressMessages(cb_detect(dat, sep = "_"))
  expect_equal(draft$q1$blank, "unticked")
  cleaned <- suppressMessages(cb_clean(dat, draft))
  expect_equal(as.character(cleaned$q1_status), c("answered", "answered", "skipped", "answered"))
  expect_equal(cleaned$q1_a, c(1L, 0L, NA, 1L))
  expect_equal(cleaned$q1_c, c(0L, 0L, NA, 0L))
})
