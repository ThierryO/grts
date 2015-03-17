\name{halton}
\alias{halton}
\title{Calculate a Halton sequence}
\usage{
  halton(size, dim = 2, start)
}
\arguments{
  \item{size}{The required length of the Halton sequence}

  \item{dim}{The number of dimensions of the Halton
  sequence}

  \item{start}{Optimal starting values. Generated at random
  when missing}
}
\description{
  Calculate a Halton sequence
}
\examples{
plot(halton(size = 100)$coordinates)
points(halton(size = 100)$coordinates, col = "green")
points(halton(size = 100)$coordinates, col = "red")
points(halton(size = 100)$coordinates, col = "blue")
}

