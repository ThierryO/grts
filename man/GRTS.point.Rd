\name{GRTS.point}
\alias{GRTS.point}
\title{GRTS sampling on points}
\usage{
  GRTS.point(spPoint, cellsize, RandomStart = TRUE)
}
\arguments{
  \item{spPoint}{A \code{\link{SpatialPoints}} or
  \code{\link{SpatialPointsDataFrame}} object}

  \item{cellsize}{The size of the GRTS grid cells. Set at
  half of the smallest distance between two points when
  missing.}

  \item{RandomStart}{logical. Should the starting point of
  grid be random or not.}
}
\value{
  a SpatialPoints object with
}
\description{
  GRTS sampling on points
}

