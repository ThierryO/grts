\name{GRTS.default}
\alias{GRTS.default}
\title{Calculate a GRTS matrix with given dimensions.}
\usage{
  GRTS.default(nrow)
}
\arguments{
  \item{nrow}{The number of row and columns of the desired
  square output matrix. Must be a power of 2.}
}
\value{
  A square matrix with the dimensions of \code{object}
  filled with a randomised order of grid cells.
}
\description{
  Create a square matrix containing a randomised order of
  grid cells based on the GRTS algorithm.
}
\examples{
GRTS(4)
}
\author{
  Thierry Onkelinx \email{Thierry.Onkelinx@inbo.be}, Paul
  Quataert
}
\seealso{
  \code{\link{GRTS}}, \code{\link{QuadratRanking}}
}

