\name{GRTS.polygon}
\Rdversion{1.1}
\alias{GRTS.polygon}

\title{
Calculate a GRTS SpatialGridDataFrame based on a set of polygongs.
}
\description{
Creates a grid with given cell dimensions
}
\usage{
GRTS.polygon(spPolygon, cellsize, Subset = FALSE, RandomStart = FALSE)
}
\arguments{
  \item{spPolygon}{
	A \code{SpatialPolygons} object or a \code{SpatialPolygonsDataFrame} object that will define the bounding box or boundaries of the GRTS object.
}
  \item{cellsize}{
  A single number defining the size of each grid cell.
}
  \item{Subset}{
  A logical value indicating wether the entire GRTS grid should be returned (\code{FALSE}) or only the grid cell whos centroid fall inside \code{object} (\code{TRUE}). Defaults to FALSE.
}
  \item{RandomStart}{
  A logical value indicating wether the GRTS grid should start at the minimum of the coordinates plus half the \code{cellsize} (\code{FALSE}) or at a random location (\code{TRUE}). Defaults to FALSE.
}
}
\value{
	A \code{SpatialGridDataFrame} if \code{Subset == FALSE} or a \code{SpatialPixelsDataFrame} if \code{Subset == TRUE}. The variable \code{Ranking} contains the randomised order of the cells.
}
\author{
Thierry Onkelinx \email{Thierry.Onkelinx@inbo.be}, Paul Quataert
}
\seealso{
\code{\link{GRTS}}, \code{\link{QuadratRanking}}
}
\examples{
  Sr1 = Polygon(cbind(c(2,4,4,1,2),c(2,3,5,4,2)))
  Sr2 = Polygon(cbind(c(5,4,2,5),c(2,3,2,2)))
  Sr3 = Polygon(cbind(c(4,4,5,10,4),c(5,3,2,5,5)))
  Sr4 = Polygon(cbind(c(5,6,6,5,5),c(4,4,3,3,4)), hole = TRUE)
  
  Srs1 = Polygons(list(Sr1), "s1")
  Srs2 = Polygons(list(Sr2), "s2")
  Srs3 = Polygons(list(Sr3, Sr4), "s3/4")
  SpP = SpatialPolygons(list(Srs1,Srs2,Srs3), 1:3)
  output <- GRTS(SpP, cellsize = 0.1)
  spplot(output)
  output <- GRTS(SpP, cellsize = 0.1, Subset = TRUE, RandomStart = TRUE)
  spplot(output)
}
