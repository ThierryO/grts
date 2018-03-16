context("nd_ranking")
test_that("nd_ranking works with different levels and dimensions", {
  design <- function(level, n_var){
    as.matrix(
      expand.grid(
        rep(
          list(
            seq_len(2 ^ (level + 1)) - 1
          ),
          n_var
        )
      )
    )
  }
  test_design <- function(level, n_var){
    expect_is(
      ranking <- nd_ranking(design(level, n_var)),
      "numeric"
    )
    expect_equal(
      length(ranking),
      (2 ^ (level + 1)) ^ n_var
    )
    expect_equal(anyDuplicated(ranking), 0)
  }
  test_design(0, 1)
  test_design(0, 2)
  test_design(0, 3)
  test_design(0, 4)
  test_design(1, 1)
  test_design(1, 2)
  test_design(1, 3)
  test_design(1, 4)
  test_design(4, 1)
  test_design(4, 2)
  test_design(4, 3)
  test_design(4, 4)

})
