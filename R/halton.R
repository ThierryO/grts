#' calculates a van der Corput sequence
#' 
#' @param size the length of the resulting sequence
#' @param base a prime number that is used a the base
#' @param start the starting position. A random value is used when missing.
#' @export
vanderCorput <- function(
  size, 
  base = 2,
  start
  ){
  if(missing(start)){
    start <- sample.int(n = .Machine$integer.max - size, size = 1, replace = TRUE)
  }
  digits <- ceiling(log(start + size, base))
  p <- base ^ (seq_len(digits + 1) - 1)
  n <- seq_len(size) + start
  return(
    list(
      sequence = sapply(
        n, 
        function(n){
          lambda <- floor((n / head(p, -1))  %% base)
          sum(lambda / tail(p, -1))
        }
      ),
      start = start,
      base = base
    )
  )
}

#' Calculate a Halton sequence
#' 
#' @param size The required length of the Halton sequence
#' @param dim The number of dimensions of the Halton sequence
#' @param start Optimal starting values. Generated at random when missing
#' @export
#' @examples
#' plot(halton(size = 100)$coordinates)
#' points(halton(size = 100)$coordinates, col = "green")
#' points(halton(size = 100)$coordinates, col = "red")
#' points(halton(size = 100)$coordinates, col = "blue")

halton <- function(size, dim = 2, start){
  primes <- c(2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97, 101, 103, 107, 109, 113, 127, 131, 137, 139, 149, 151, 157, 163, 167, 173, 179, 181, 191, 193, 197, 199, 211, 223, 227, 229, 233, 239, 241, 251, 257, 263, 269, 271, 277, 281, 283, 293)
  if(missing(start)){
    start <- sample.int(.Machine$integer.max - size, size = dim, replace = TRUE)
  } else {
    if(length(start) != dim){
      stop("The length of start should be equal to dim.")
    }
  }
  if(length(primes) < dim){
    stop("Halton sequences currently only available for maximum ", length(primes), " dimensions.")
  }
  base <- sample(head(primes, dim))
  coordinates <- sapply(
    seq_len(dim), 
    function(i){
      vanderCorput(size = size, base = base[i], start = start[i])$sequence
    }
  )
  return(
    list(
      coordinates = coordinates, 
      base = base, 
      start = start
    )
  )
}
