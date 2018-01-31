#' Workhorse for multidimensional GRTS
#' @param design the design matrix: one column for each dimension. Each column holds integer values between \eqn{0} and \eqn{2^{level+1}}
#' @param level the number of required recursive calls
#' @param n_var the number of columns in the design matrix
#' @export
NdRanking <- function(
  design,
  level = floor(log2(max(design) + 1)) - 1,
  n_var = ncol(design)
) {
  random_order <- sample(2 ^ n_var) - 1
  # random_order <- seq_len(2 ^ ncol(design)) - 1
  ranking <- ((design %/% (2 ^ level)) %% 2) %*%
    matrix(2 ^ (0:(n_var - 1)))
  ranking <- random_order[ranking + 1]
  if (level == 0) {
    return(ranking)
  }
  current <- ranking
  for (i in random_order) {
    ranking[current == i] <- i +
      NdRanking(
        design = design[current == i, , drop = FALSE],
        n_var = n_var,
        level = level - 1
      ) *
      (2 ^ ncol(design))
  }
  return(ranking)
}
