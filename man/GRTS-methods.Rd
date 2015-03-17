\name{GRTS-methods}
\docType{methods}
\alias{GRTS-methods}
\alias{GRTS,numeric-method}
\alias{GRTS,SpatialPolygons-method}
\title{ ~~ Methods for Function \code{GRTS} in Package \pkg{GRTS} ~~}
\description{
 Selects the appropriate GRTS function depending on the class of \code{object}
}
\section{Methods}{
\describe{

\item{\code{signature(object = "numeric")}}{
 Use \code{GRTS.default}
}

\item{\code{signature(object = "SpatialPolygons")}}{
Use \code{GRTS.polygon}
}
}}
\keyword{methods}
