translate <- function(x, y) {
  delta <- diff(head(y, 2))
  breaks <- c(y - delta / 2, max(y) + delta / 2)
  order <- findInterval(x, breaks)
  output <- rep(NA_integer_, length(y))
  output[order] <- x
  list(
    df = data.frame(
      x = x,
      y = y[order]
    ),
    vec = output
  )
}
