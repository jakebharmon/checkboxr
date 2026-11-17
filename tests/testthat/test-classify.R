roles <- c("substantive", "substantive", "substantive", "none", "dk", "pna")

classify <- function(..., empty = "na", conflict = "keep") {
  mat <- matrix(as.integer(c(...)), nrow = 1)
  out <- classify_status(mat, roles, empty = empty, conflict = conflict)
  c(pattern = out$pattern, status = as.character(out$status))
}

test_that("any NA in the set is not_shown and wins over everything", {
  expect_equal(classify(NA, 0, 0, 0, 0, 0), c(pattern = "not_shown", status = "not_shown"))
  expect_equal(classify(1, 0, 0, 0, 1, NA), c(pattern = "not_shown", status = "not_shown"))
  expect_equal(classify(NA, NA, NA, NA, NA, NA), c(pattern = "not_shown", status = "not_shown"))
})

test_that("substantive with dk, pna or none is a conflict", {
  expect_equal(classify(1, 0, 0, 0, 1, 0), c(pattern = "conflict", status = "answered"))
  expect_equal(classify(1, 0, 0, 0, 0, 1), c(pattern = "conflict", status = "answered"))
  expect_equal(classify(0, 1, 0, 1, 0, 0), c(pattern = "conflict", status = "answered"))
  expect_equal(
    classify(1, 0, 0, 0, 1, 0, conflict = "na"),
    c(pattern = "conflict", status = "conflict")
  )
})

test_that("dk ticked with no substantive option is dk", {
  expect_equal(classify(0, 0, 0, 0, 1, 0), c(pattern = "dk", status = "dk"))
  # dk and pna together: dk
  expect_equal(classify(0, 0, 0, 0, 1, 1), c(pattern = "dk", status = "dk"))
  # dk with none: still no usable answer
  expect_equal(classify(0, 0, 0, 1, 1, 0), c(pattern = "dk", status = "dk"))
})

test_that("pna ticked with no substantive option is pna", {
  expect_equal(classify(0, 0, 0, 0, 0, 1), c(pattern = "pna", status = "pna"))
  expect_equal(classify(0, 0, 0, 1, 0, 1), c(pattern = "pna", status = "pna"))
})

test_that("none ticked alone is answered", {
  expect_equal(classify(0, 0, 0, 1, 0, 0), c(pattern = "none", status = "answered"))
})

test_that("a substantive tick is answered", {
  expect_equal(classify(1, 1, 0, 0, 0, 0), c(pattern = "substantive", status = "answered"))
})

test_that("all unticked follows `empty`", {
  expect_equal(classify(0, 0, 0, 0, 0, 0), c(pattern = "empty", status = "skipped"))
  expect_equal(
    classify(0, 0, 0, 0, 0, 0, empty = "zero"),
    c(pattern = "empty", status = "answered")
  )
})

test_that("the classifier is vectorised and returns the status levels", {
  mat <- as.matrix(cb_example[c("q5_car", "q5_bus", "q5_bike", "q5_none", "q5_dk", "q5_pna")])
  storage.mode(mat) <- "integer"
  out <- classify_status(mat, roles)
  expect_equal(
    as.character(out$status),
    c("answered", "answered", "answered", "dk", "pna", "skipped", "answered", "answered")
  )
  expect_equal(
    levels(out$status),
    c("answered", "dk", "pna", "skipped", "not_shown", "conflict")
  )
})

test_that("sets without none, dk or pna columns classify", {
  mat <- matrix(c(1L, 0L, 0L, 0L), nrow = 2)
  out <- classify_status(mat, c("substantive", "substantive"))
  expect_equal(as.character(out$status), c("answered", "skipped"))
})
