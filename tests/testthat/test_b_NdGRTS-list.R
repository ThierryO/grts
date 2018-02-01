context("NdGRTS on lists")
test_that("NdGRTS works on simple list", {
  # simple lists = only vectors of same length which is power of 2
  basic <- list(X = c(0, 1, 2, 3), Y = 0:1, Z = 0:3, A = 0:4)
  test_dimension <- function(basic, n){
    expect_is(
      result <- NdGRTS(basic[seq_len(n)]),
      "data.frame"
    )
    expect_identical(
      names(result),
      c(names(basic)[seq_len(n)], "OriginalRanking", "Ranking")
    )
    expect_identical(
      anyDuplicated(result$Ranking),
      0L
    )
  }
  test_dimension(basic, 1)
  test_dimension(basic, 2)
  test_dimension(basic, 3)
  test_dimension(basic, 4)
})

test_that("NdGRTS checks that the list contains only relevant types", {
  expect_error(
    NdGRTS(object = list()),
    "object must contain at least one element"
  )
  expect_error(
    NdGRTS(object = list(data.frame())),
    "all elements of object must be numeric or integer"
  )
})

test_that("NdGRTS handles optional arguments", {
  expect_is(
    NdGRTS(list(X = 0:2, Y = 0:2), new.length = 1),
    "data.frame"
  )
  expect_is(
    NdGRTS(list(X = 0:2, Y = 0:2), force = FALSE),
    "data.frame"
  )
  expect_error(
    NdGRTS(list(X = 0:2, Y = 0:2), force = NA),
    "force contains missing values"
  )
  expect_error(
    NdGRTS(list(X = 0:2, Y = 0:2), force = 1),
    "force is not a length one logical vector"
  )
})
