\docType{methods}
\name{GRTS}
\alias{GRTS}
\alias{GRTS-methods}
\alias{GRTS,numeric-method}
\alias{GRTS,SpatialGridDataFrame-method}
\alias{GRTS,SpatialGrid-method}
\alias{GRTS,SpatialPointsDataFrame-method}
\alias{GRTS,SpatialPoints-method}
\alias{GRTS,SpatialPolygonsDataFrame-method}
\alias{GRTS,SpatialPolygons-method}
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
  \item{\code{signature(object = "SpatialGrid")}}{Use
  \code{GRTS.grid}} \item{\code{signature(object =
  "SpatialGridDataFrame")}}{Use \code{GRTS.grid}}
  \item{\code{signature(object = "SpatialPolygons")}}{Use
  \code{GRTS.polygon}} \item{\code{signature(object =
  "SpatialPolygonsDataFrame")}}{Use \code{GRTS.polygon}} }
}
\examples{
GRTS(4)
}
\seealso{
  \code{\link{GRTS.grid}}

  \code{\link{GRTS.point}}

  \code{\link{GRTS.polygon}}

  \code{\link{GRTS.default}}
}
\keyword{methods}

