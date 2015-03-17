\docType{methods}
\name{GRTS-methods}
\alias{GRTS}
\alias{GRTS-methods}
\alias{GRTS,numeric-method}
\alias{GRTS,SpatialPoints-method}
\alias{GRTS,SpatialPointsDataFrame-method}
\alias{GRTS,SpatialPolygons-method}
\alias{GRTS,SpatialPolygonsDataFrame-method}
\title{The GRTS wrapper function}
\arguments{
  \item{object}{The object on which to generate a GRTS
  sample}

  \item{...}{further arguments to the functions}
}
\value{
  A GRTS sample
}
\description{
  The GRTS wrapper function selects the appropriate GRTS
  function based on the object.
}
\section{Methods}{
  \describe{ \item{\code{signature(object =
  "numeric")}}{Use \code{GRTS.default}}
  \item{\code{signature(object = "SpatialPoints")}}{Use
  \code{GRTS.point}} \item{\code{signature(object =
  "SpatialPointsDataFrame")}}{Use \code{GRTS.point}}
  \item{\code{signature(object = "SpatialPolygons")}}{Use
  \code{GRTS.polygon}} \item{\code{signature(object =
  "SpatialPolygonsDataFrame")}}{Use \code{GRTS.polygon}} }
}
\examples{
GRTS(4)
}
\seealso{
  \code{\link{GRTS.default}}

  \code{\link{GRTS.point}}

  \code{\link{GRTS.polygon}}
}
\keyword{methods}

