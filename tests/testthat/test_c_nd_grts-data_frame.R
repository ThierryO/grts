context("nd_grts on data.frame")
test_that("nd_grts works on simple list", {
  # simple lists = only vectors of same length which is power of 2
  basic <- data.frame(
    X = c(0, 1, 2, 3),
    Y = 0:1,
    Z = letters[1:2],
    A = factor(letters[1:2]),
    stringsAsFactors = FALSE
  )
  test_dimension <- function(basic, n){
    expect_is(
      result <- nd_grts(basic[, seq_len(n), drop = FALSE]),
      "list"
    )
    expect_identical(
      names(result),
      c("object", "design")
    )
    expect_identical(
      names(result$object),
      c(names(basic)[seq_len(n)], "original_ranking", "ranking")
    )
    expect_identical(
      names(result$design),
      c(names(basic)[seq_len(n)], "original_ranking", "ranking")
    )
    expect_identical(
      anyDuplicated(result$design$ranking),
      0L
    )
    expect_identical(
      anyDuplicated(result$design$original_ranking),
      0L
    )
  }
  test_dimension(basic, 1)
  test_dimension(basic, 2)
  test_dimension(basic, 3)
  test_dimension(basic, 4)
})
test_that("nd_grts handles optional arguments", {
  input <- data.frame(X = c(0, 1, 10), Y = 0:2)
  expect_is(
    output <- nd_grts(
      input,
      reference = "Y",
      scale = c(X = 2)
    ),
    "list"
  )
  expect_identical(nrow(input), nrow(output$object))
  expect_is(
    output <- nd_grts(
      input,
      reference = "X",
      scale = c(Y = 1),
      new.length = 9
    ),
    "list"
  )
  expect_identical(nrow(input), nrow(output$object))
})
