#'The workhorse of the package.
#'
#'Takes a square matrix of zeros with number of rows equal to a power of 2.
#'When the number of rows is at least 2, this matrix is split in to four equal
#'submatrices: in half along the columns and in half along the rows. Then the
#'QuadratRanking function is applied recursively while increasing the level
#'with 1.
#'
#'When the \code{Ranking} matrix recudes to a 1x1 matrix, the function returns
#'this 1x1 matrix. When the \code{Ranking} matrix is split into four
#'submatrices, the number 0 to 3 are assigned at random to one of the
#'submatrices. Then the values of the submatrix, after recursively applying the
#'QuadratRanking function, is increased with the random number times 4 to the
#'power of the level.
#'
#'WARNING: This function does not do any checking on the sanity of Ranking and
#'Level. That would be a computational burden since the function is called
#'recursively. Please use the \code{GRTS} wrapper function instead. That does
#'the necessary checking prior to calling \code{QuadratRanking}.
#'
#'
#'@param Ranking A square matrix with number of rows equal to a power of 2.
#'Must start with a matrix of zeros.
#'@param Level A square matrix with number of rows equal to a power of 2. Must
#'start with a matrix of zeros.
#'@return A matrix with the same dimension of \code{Ranking} filled with a
#'randomised order of points.
#'@author Thierry Onkelinx \email{Thierry.Onkelinx@@inbo.be}, Paul Quataert
#'@seealso \code{\link{GRTS}}
#'@export
#'@examples
#'
#'  QuadratRanking(Ranking = matrix(0, ncol = 4, nrow = 4))
#'
QuadratRanking <- function(Ranking, Level = 0){
  if(ncol(Ranking) > 1){
    Sequence <- sample(0:3)
    Q1 <- QuadratRanking(Ranking[seq_len(nrow(Ranking)) <= nrow(Ranking) / 2, seq_len(ncol(Ranking)) <= ncol(Ranking) / 2, drop = FALSE] + Sequence[1] * 4 ^ Level, Level = Level + 1)
    Q2 <- QuadratRanking(Ranking[seq_len(nrow(Ranking)) > nrow(Ranking) / 2, seq_len(ncol(Ranking)) <= ncol(Ranking) / 2, drop = FALSE] + Sequence[2] * 4 ^ Level, Level = Level + 1)
    Q3 <- QuadratRanking(Ranking[seq_len(nrow(Ranking)) <= nrow(Ranking) / 2, seq_len(ncol(Ranking)) > ncol(Ranking) / 2, drop = FALSE] + Sequence[3] * 4 ^ Level, Level = Level + 1)
    Q4 <- QuadratRanking(Ranking[seq_len(nrow(Ranking)) > nrow(Ranking) / 2, seq_len(ncol(Ranking)) > ncol(Ranking) / 2, drop = FALSE] + Sequence[4] * 4 ^ Level, Level = Level + 1)
    cbind(rbind(Q1, Q2), rbind(Q3, Q4))
  } else {
    Ranking
  }
}
