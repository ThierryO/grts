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
    expect_identical(
      anyDuplicated(result$OriginalRanking),
      0L
    )
  }
  test_dimension(basic, 1)
  test_dimension(basic, 2)
  test_dimension(basic, 3)
  test_dimension(basic, 4)
})

test_that("NdGRTS handles optional arguments", {
  expect_is(
    NdGRTS(data.frame(X = c(0, 1, 10), Y = 0:2), new.length = 5),
    "list"
  )
  expect_error(
    NdGRTS(data.frame(X = 0:2, Y = 0:2), force = NA),
    "force contains missing values"
  )
  expect_error(
    NdGRTS(data.frame(X = 0:2, Y = 0:2), force = 1),
    "force is not a length one logical vector"
  )
  expect_is(
    NdGRTS(
      list(X = c(0, 1, 10), Y = 0:2),
      reference = "Y",
      scale = c(X = 2)
    ),
    "data.frame"
  )
  expect_is(
    NdGRTS(
      list(X = c(0, 1, 10), Y = 0:2),
      reference = "X",
      scale = c(Y = 1),
      new.length = 9
    ),
    "data.frame"
  )
})
