context("NdGRTS on data.frame")
test_that("NdGRTS works on simple list", {
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
      result <- NdGRTS(basic[, seq_len(n), drop = FALSE]),
      "list"
    )
    expect_identical(
      names(result),
      c("object", "design")
    )
    expect_identical(
      names(result$object),
      c(names(basic)[seq_len(n)], "OriginalRanking", "Ranking")
    )
    expect_identical(
      names(result$design),
      c(names(basic)[seq_len(n)], "OriginalRanking", "Ranking")
    )
    expect_identical(
      anyDuplicated(result$design$Ranking),
      0L
    )
    expect_identical(
      anyDuplicated(result$design$OriginalRanking),
      0L
    )
  }
  test_dimension(basic, 1)
  test_dimension(basic, 2)
  test_dimension(basic, 3)
  test_dimension(basic, 4)
})
