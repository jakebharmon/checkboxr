test_that("default cleaning gives the expected status and values", {
  cleaned <- clean_example()
  expect_equal(
    as.character(cleaned$q5_status),
    c("answered", "answered", "answered", "dk", "pna", "skipped", "answered", "answered")
  )
  expect_equal(
    levels(cleaned$q5_status),
    c("answered", "dk", "pna", "skipped", "not_shown", "conflict")
  )

  answered <- c(1, 2, 3, 7, 8)
  for (col in q5_cols) {
    expect_type(cleaned[[col]], "integer")
    expect_equal(cleaned[[col]][answered], as.integer(cb_example[[col]][answered]))
    expect_true(all(is.na(cleaned[[col]][c(4, 5, 6)])))
  }
  expect_equal(cleaned$q5_car[7], 1L)
  expect_equal(nrow(cleaned), 8)
})

test_that("option percentages match the spec under each setting", {
  expect_equal(
    option_pct(cb_example),
    c(q5_car = 3 / 8, q5_bus = 1 / 8, q5_bike = 2 / 8, q5_none = 1 / 8)
  )
  expect_equal(
    option_pct(clean_example()),
    c(q5_car = 3 / 5, q5_bus = 1 / 5, q5_bike = 2 / 5, q5_none = 1 / 5)
  )
  expect_equal(
    option_pct(clean_example(conflict = "na")),
    c(q5_car = 2 / 4, q5_bus = 1 / 4, q5_bike = 2 / 4, q5_none = 1 / 4)
  )
  expect_equal(
    option_pct(clean_example(empty = "zero")),
    c(q5_car = 3 / 6, q5_bus = 1 / 6, q5_bike = 2 / 6, q5_none = 1 / 6)
  )
})

test_that("cb_clean() is idempotent", {
  for (args in list(list(), list(conflict = "na"), list(empty = "zero"))) {
    once <- do.call(clean_example, args)
    twice <- do.call(cb_clean, c(list(once, q5_sets(), quiet = TRUE), args))
    expect_identical(twice, once)
  }
  # a third pass with default settings does not disturb earlier decisions
  once <- clean_example(conflict = "na")
  expect_identical(cb_clean(once, q5_sets(), quiet = TRUE), once)
})

test_that("dk and pna columns are never changed", {
  cleaned <- clean_example(conflict = "na")
  expect_identical(cleaned$q5_dk, cb_example$q5_dk)
  expect_identical(cleaned$q5_pna, cb_example$q5_pna)
})

test_that("the status column sits after the set and other columns are untouched", {
  cleaned <- clean_example()
  expect_named(cleaned, c(names(cb_example), "q5_status"))
  expect_identical(cleaned[c("id", "age", "group")], cb_example[c("id", "age", "group")])

  dat <- dplyr::relocate(cb_example, age, .after = dplyr::last_col())
  expect_equal(
    names(cb_clean(dat, q5_sets(), quiet = TRUE)),
    c("id", "group", "q5_car", "q5_bus", "q5_bike", "q5_none", "q5_dk", "q5_pna",
      "q5_status", "age")
  )
})

test_that("conflict = 'na' discards the row", {
  cleaned <- clean_example(conflict = "na")
  expect_equal(as.character(cleaned$q5_status[7]), "conflict")
  expect_true(all(is.na(unlist(cleaned[7, q5_cols]))))
})

test_that("conflict = 'error' stops and names the row", {
  expect_error(clean_example(conflict = "error"), "row 7")
  expect_error(clean_example(conflict = "error"), "q5")
  no_conflict <- cb_example[-7, ]
  expect_no_error(cb_clean(no_conflict, q5_sets(), conflict = "error", quiet = TRUE))
})

test_that("conflict = 'keep' sets the none column to 0", {
  dat <- cb_example
  dat$q5_none[1] <- 1 # car + bus + none
  cleaned <- cb_clean(dat, q5_sets(), quiet = TRUE)
  expect_equal(as.character(cleaned$q5_status[1]), "answered")
  expect_equal(cleaned$q5_none[1], 0L)
  expect_equal(cleaned$q5_car[1], 1L)
})

test_that("NA input is not_shown", {
  dat <- cb_example
  dat$q5_bus[2] <- NA
  dat[8, c("q5_car", "q5_bus", "q5_bike", "q5_none", "q5_dk", "q5_pna")] <- NA
  cleaned <- cb_clean(dat, q5_sets(), quiet = TRUE)
  expect_equal(as.character(cleaned$q5_status[c(2, 8)]), c("not_shown", "not_shown"))
  expect_true(all(is.na(unlist(cleaned[c(2, 8), q5_cols]))))
  expect_identical(cb_clean(cleaned, q5_sets(), quiet = TRUE), cleaned)
})

test_that("empty = 'zero' keeps all-unticked rows as answered", {
  cleaned <- clean_example(empty = "zero")
  expect_equal(as.character(cleaned$q5_status[6]), "answered")
  expect_equal(unlist(cleaned[6, q5_cols], use.names = FALSE), rep(0L, 4))
})

test_that("logical columns give the same result as 0/1", {
  set_cols <- grep("^q5_", names(cb_example), value = TRUE)
  dat <- cb_example
  dat[set_cols] <- lapply(dat[set_cols], function(x) x == 1)
  cleaned <- cb_clean(dat, q5_sets(), quiet = TRUE)
  expected <- clean_example()

  expect_equal(cleaned[c(q5_cols, "q5_status")], expected[c(q5_cols, "q5_status")])
  expect_identical(cleaned$q5_dk, dat$q5_dk)
  expect_identical(cb_clean(cleaned, q5_sets(), quiet = TRUE), cleaned)
  expect_equal(cb_count(cleaned, q5_sets()), cb_count(expected, q5_sets()))
})

test_that("Checked / Unchecked strings give the same result as 0/1", {
  set_cols <- grep("^q5_", names(cb_example), value = TRUE)
  dat <- cb_example
  dat[set_cols] <- lapply(dat[set_cols], function(x) ifelse(x == 1, "Checked", "Unchecked"))
  sets <- q5_sets(checked = "Checked")
  cleaned <- cb_clean(dat, sets, quiet = TRUE)
  expected <- clean_example()

  expect_equal(cleaned[c(q5_cols, "q5_status")], expected[c(q5_cols, "q5_status")])
  expect_identical(cleaned$q5_dk, dat$q5_dk)
  expect_identical(cleaned$q5_pna, dat$q5_pna)
  expect_identical(cb_clean(cleaned, sets, quiet = TRUE), cleaned)
  expect_equal(cb_check(dat, sets), cb_check(cb_example, q5_sets()))
  expect_equal(cb_count(cleaned, sets), cb_count(expected, q5_sets()))

  # factors behave like strings
  dat[set_cols] <- lapply(dat[set_cols], factor)
  expect_equal(cb_clean(dat, sets, quiet = TRUE)[c(q5_cols, "q5_status")],
               expected[c(q5_cols, "q5_status")])
})

test_that("custom checked and unchecked codes work and stay idempotent", {
  set_cols <- grep("^q5_", names(cb_example), value = TRUE)
  dat <- cb_example
  dat[set_cols] <- lapply(dat[set_cols], function(x) ifelse(x == 1, 2, 1))
  sets <- q5_sets(checked = 2, unchecked = 1)
  cleaned <- cb_clean(dat, sets, quiet = TRUE)
  expect_equal(cleaned[c(q5_cols, "q5_status")], clean_example()[c(q5_cols, "q5_status")])
  expect_identical(cb_clean(cleaned, sets, quiet = TRUE), cleaned)
})

test_that("unexpected values are an error naming set and column", {
  dat <- cb_example
  dat$q5_bus[3] <- 7
  expect_error(cb_clean(dat, q5_sets(), quiet = TRUE), "q5_bus")
  expect_error(cb_clean(dat, q5_sets(), quiet = TRUE), "q5")
})

test_that("cb_clean() prints one summary line per set", {
  expect_message(
    cb_clean(cb_example, q5_sets()),
    "3 of 8 respondents set to.*1 dk, 1 pna, 1 skipped"
  )
  expect_no_message(cb_clean(cb_example, q5_sets(), quiet = TRUE))
})

test_that("status_suffix is honoured and a foreign status column is refused", {
  cleaned <- cb_clean(cb_example, q5_sets(), status_suffix = "_why", quiet = TRUE)
  expect_true("q5_why" %in% names(cleaned))
  expect_identical(
    cb_clean(cleaned, q5_sets(), status_suffix = "_why", quiet = TRUE),
    cleaned
  )

  dat <- cb_example
  dat$q5_status <- "something else"
  expect_error(cb_clean(dat, q5_sets(), quiet = TRUE), "q5_status")
})

test_that("a status column read back as text is still recognised", {
  cleaned <- clean_example()
  as_text <- cleaned
  as_text$q5_status <- as.character(as_text$q5_status)
  expect_equal(cb_clean(as_text, q5_sets(), quiet = TRUE), cleaned)
})

test_that("several sets are cleaned independently", {
  dat <- cb_example
  dat$q6_a <- c(1, 0, 0, 0, 1, 0, 0, 1)
  dat$q6_b <- c(0, 0, 1, 0, 1, 0, 0, 0)
  sets <- cb_sets(q5_sets(), cb_define("q6", "Second question", starts_with("q6_")))
  cleaned <- cb_clean(dat, sets, quiet = TRUE)
  expect_equal(
    as.character(cleaned$q6_status),
    c("answered", "skipped", "answered", "skipped", "answered", "skipped", "skipped", "answered")
  )
  expect_equal(cleaned$q5_status, clean_example()$q5_status)
  expect_identical(cb_clean(cleaned, sets, quiet = TRUE), cleaned)
})

test_that("plain data frames and grouped tibbles work", {
  df <- as.data.frame(cb_example)
  expect_equal(
    tibble::as_tibble(cb_clean(df, q5_sets(), quiet = TRUE)),
    clean_example()
  )
  grouped <- dplyr::group_by(cb_example, group)
  expect_equal(
    dplyr::ungroup(cb_clean(grouped, q5_sets(), quiet = TRUE)),
    clean_example()
  )
  expect_error(cb_clean(1:3, q5_sets()), "data frame")
})
