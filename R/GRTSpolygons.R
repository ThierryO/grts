#' @export
#' @docType methods
#' @rdname GRTS-methods
#' @aliases GRTS,SpatialPolygons-method
#' @method GRTS SpatialPolygons-method
setMethod("GRTS", signature(object = "SpatialPolygons"), function(object, ...) {
  args <- list(...)
  if("cellsize" %in% names(args)){
    cellsize <- args[["cellsize"]]
  } else {
    stop("cellsize must be defined")
  }
  if("Subset" %in% names(args)){
    Subset <- args[["Subset"]]
  } else {
    Subset <- FALSE
  }
  if("RandomStart" %in% names(args)){
    RandomStart <- args[["RandomStart"]]
  } else {
    RandomStart <- FALSE
  }
  GRTS.polygon(spPolygon = object, cellsize = cellsize, Subset = Subset, RandomStart = RandomStart)
})

#' @export
#' @docType methods
#' @rdname GRTS-methods
#' @aliases GRTS,SpatialPolygonsDataFrame-method
#' @method GRTS SpatialPolygonsDataFrame-method
#' @seealso \code{\link{GRTS.polygon}}
setMethod("GRTS", signature(object = "SpatialPolygonsDataFrame"), function(object, ...) {
  args <- list(...)
  if("cellsize" %in% names(args)){
    cellsize <- args[["cellsize"]]
  } else {
    stop("cellsize must be defined")
  }
  if("Subset" %in% names(args)){
    Subset <- args[["Subset"]]
  } else {
    Subset <- FALSE
  }
  if("RandomStart" %in% names(args)){
    RandomStart <- args[["RandomStart"]]
  } else {
    RandomStart <- FALSE
  }
  GRTS.polygon(spPolygon = object, cellsize = cellsize, Subset = Subset, RandomStart = RandomStart)
})


#'Calculate a GRTS SpatialGridDataFrame based on a set of polygongs.
#'
#'Creates a grid with given cell dimensions
#'
#'
#'@param spPolygon A \code{SpatialPolygons} object or a
#'\code{SpatialPolygonsDataFrame} object that will define the bounding box or
#'boundaries of the GRTS object.
#'@param cellsize A single number defining the size of each grid cell.
#'@param Subset A logical value indicating wether the entire GRTS grid should
#'be returned (\code{FALSE}) or only the grid cell whos centroid fall inside
#'\code{object} (\code{TRUE}). Defaults to FALSE.
#'@param RandomStart A logical value indicating wether the GRTS grid should
#'start at the minimum of the coordinates plus half the \code{cellsize}
#'(\code{FALSE}) or at a random location (\code{TRUE}). Defaults to FALSE.
#'@return A \code{SpatialGridDataFrame} if \code{Subset == FALSE} or a
#'\code{SpatialPixelsDataFrame} if \code{Subset == TRUE}. The variable
#'\code{Ranking} contains the randomised order of the cells.
#'@author Thierry Onkelinx \email{Thierry.Onkelinx@@inbo.be}, Paul Quataert
#'@seealso \code{\link{GRTS}}, \code{\link{QuadratRanking}}
#'@examples
#'  library(sp)
#'  Sr1 = Polygon(cbind(c(2,4,4,1,2),c(2,3,5,4,2)))
#'  Sr2 = Polygon(cbind(c(5,4,2,5),c(2,3,2,2)))
#'  Sr3 = Polygon(cbind(c(4,4,5,10,4),c(5,3,2,5,5)))
#'  Sr4 = Polygon(cbind(c(5,6,6,5,5),c(4,4,3,3,4)), hole = TRUE)
#'  
#'  Srs1 = Polygons(list(Sr1), "s1")
#'  Srs2 = Polygons(list(Sr2), "s2")
#'  Srs3 = Polygons(list(Sr3, Sr4), "s3/4")
#'  SpP = SpatialPolygons(list(Srs1,Srs2,Srs3), 1:3)
#'  output <- GRTS(SpP, cellsize = 0.1)
#'  spplot(output)
#'  output <- GRTS(SpP, cellsize = 0.1, Subset = TRUE, RandomStart = TRUE)
#'  spplot(output)
#'@importFrom sp bbox GridTopology SpatialGridDataFrame gridded<-
GRTS.polygon <- function(spPolygon, cellsize, Subset = FALSE, RandomStart = FALSE){
  Xrange <- bbox(spPolygon)[1, ]
  Yrange <- bbox(spPolygon)[2, ]
  DimGrid <- ceiling(max(diff(Xrange), diff(Yrange)) / cellsize)
  DimGrid <- 2 ^ ceiling(log2(DimGrid))

  Result <- QuadratRanking(matrix(0L, ncol = DimGrid, nrow = DimGrid), Level = 0)
  if(RandomStart){
    Result <- Result[seq_len(ceiling(diff(Xrange)/cellsize) + 1), seq_len(ceiling(diff(Yrange)/cellsize) + 1)]
    GRID <- GridTopology(cellcentre.offset = c(x = min(Xrange), y = min(Yrange)) + runif(2, min = -cellsize, max = 0), cellsize = c(cellsize, cellsize), cells.dim = dim(Result))
  } else {
    Result <- Result[seq_len(ceiling(diff(Xrange)/cellsize)), seq_len(ceiling(diff(Yrange)/cellsize))]
    GRID <- GridTopology(cellcentre.offset = c(x = min(Xrange), y = min(Yrange)) + 0.5 * cellsize, cellsize = c(cellsize, cellsize), cells.dim = dim(Result))
  }
  Result <- SpatialGridDataFrame(grid = GRID, data = data.frame(Ranking = as.vector(Result)), proj4string = proj4string(spPolygon))
  rm(GRID)
  if(Subset){
    gc()
    gridded(Result) <- FALSE
    if("SpatialPolygons" %in% class(spPolygon)){
      Result <- Result[!is.na(over(Result, spPolygon)), ]
    } else if("SpatialPolygonsDataFrame" %in% class(spPolygon)){
      Result <- Result[!is.na(over(Result, spPolygon)[, 1]), ]
    }
    gridded(Result) <- TRUE
  }
  Result$Ranking <- as.numeric(factor(Result$Ranking)) - 1
  Result
}
