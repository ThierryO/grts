context("unify_length")
test_that("unify_length works on integers", {
  expect_error(unify_length(matrix(1L)))
  expect_error(
    unify_length(0:1, 1),
    "length\\(x\\) not less than or equal to new.length"
  )
  expect_error(
    unify_length(rep(0L, 2), 2),
    "vector contains duplicate values"
  )
  expect_error(
    unify_length(c(0L, NA), 2),
    "x contains 1 missing value"
  )
  expect_error(
    unify_length(c(0L, 1L, 10L), 4),
    "Current new.length = 4 is too small"
  )
  expect_equal(
    unify_length(c(0L, 1L, 10L), 11),
    0:10
  )
})

test_that("unify_length works on numerics", {
  expect_error(unify_length(matrix(1)))
  expect_error(
    unify_length(0:1, 1),
    "length\\(x\\) not less than or equal to new.length"
  )
  expect_error(
    unify_length(rep(0, 2), 2),
    "vector contains duplicate values"
  )
  expect_error(
    unify_length(c(0, NA), 2),
    "x contains 1 missing value"
  )
  expect_error(
    unify_length(c(0L, 1, 10), 4),
    "Current new.length = 4 is too small"
  )
  expect_equal(
    unify_length(c(0, 1, 10), 11),
    0:10
  )
})
