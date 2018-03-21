context("nd_grts on lists")
test_that("nd_grts works on simple list", {
  # simple lists = only vectors of same length which is power of 2
  basic <- list(X = c(0, 1, 2, 3), Y = 0:1, Z = 0:3, A = 0:4)
  test_dimension <- function(basic, n){
    expect_is(
      result <- nd_grts(basic[seq_len(n)]),
      "data.frame"
    )
    expect_identical(
      names(result),
      c(names(basic)[seq_len(n)], "original_ranking", "ranking")
    )
    expect_identical(
      anyDuplicated(result$ranking),
      0L
    )
    expect_identical(
      anyDuplicated(result$original_ranking),
      0L
    )
  }
  test_dimension(basic, 1)
  test_dimension(basic, 2)
  test_dimension(basic, 3)
  test_dimension(basic, 4)
})

test_that("nd_grts handles optional arguments", {
  expect_is(
    nd_grts(list(X = c(0, 1, 10), Y = 0:2), new.length = 5),
    "data.frame"
  )
  expect_error(
    nd_grts(list(X = 0:2, Y = 0:2), force = NA),
    "force contains missing values"
  )
  expect_error(
    nd_grts(list(X = 0:2, Y = 0:2), force = 1),
    "force is not a length one logical vector"
  )
  expect_is(
    nd_grts(
      list(X = c(0, 1, 10), Y = 0:2),
      reference = "Y",
      scale = c(X = 2)
    ),
    "data.frame"
  )
  expect_is(
    nd_grts(
      list(X = c(0, 1, 10), Y = 0:2),
      reference = "X",
      scale = c(Y = 1),
      new.length = 9
    ),
    "data.frame"
  )
})
