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
