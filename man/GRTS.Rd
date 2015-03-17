\name{GRTS}
\Rdversion{1.1}
\alias{GRTS}

\title{
A generic function for GRTS samples
}
\description{
Choose the appropriate GRTS function based on the class of \code{object}.
}
\usage{
GRTS(object, ...)
}
\arguments{
  \item{object}{
	A basis on which to calculate the GRTS
}
  \item{...}{
  Furter arguments passed to the functions.
}
}
\value{
	Depends on the class of \code{object}
}
\author{
Thierry Onkelinx \email{Thierry.Onkelinx@inbo.be}, Paul Quataert
}
\seealso{
\code{\link{GRTS-methods}}, \code{\link{GRTS.default}}, \code{\link{GRTS.polygon}}
}
\examples{
  GRTS(4)
}
