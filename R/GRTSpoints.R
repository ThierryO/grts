#' GRTS sampling on points
#' 
#' @param spPoint A \code{\link{SpatialPoints}} or \code{\link{SpatialPointsDataFrame}} object
#' @param cellsize The size of the GRTS grid cells. Set at half of the smallest distance between two points when missing.
#' @param RandomStart logical. Should the starting point of grid be random or not.
#' @export
#' @importFrom spatstat nndist owin as.ppp as.ppp.matrix as.ppp.data.frame
#' @importFrom sp gridded over GridTopology coordinates bbox SpatialGridDataFrame proj4string CRS SpatialPointsDataFrame
#' @return a SpatialPoints object with 
GRTS.point <- function(spPoint, cellsize, RandomStart = TRUE){
  #cellsize <- 1000
  #RandomStart <- TRUE
  Xrange <- bbox(spPoint)[1, ]
  Yrange <- bbox(spPoint)[2, ]
  if(missing(cellsize)){
    cellsize <- min(nndist(as.ppp(coordinates(spPoint), owin(xrange = Xrange, yrange = Yrange)))) / 2
  }
  DimGrid <- ceiling(max(diff(Xrange), diff(Yrange)) / cellsize) + 2
  DimGrid <- 2 ^ ceiling(log2(DimGrid))
  Result <- QuadratRanking(matrix(0L, ncol = DimGrid, nrow = DimGrid), Level = 0)
  if(RandomStart){
    Result <- Result[seq_len(ceiling(diff(Xrange)/cellsize) + 2), seq_len(ceiling(diff(Yrange)/cellsize) + 2)]
    GRID <- GridTopology(cellcentre.offset = c(x = min(Xrange), y = min(Yrange)) + runif(2, min = -1.5 * cellsize, max = -0.5 * cellsize), cellsize = c(cellsize, cellsize), cells.dim = dim(Result))
  } else {
    Result <- Result[seq_len(ceiling(diff(Xrange)/cellsize) + 1), seq_len(ceiling(diff(Yrange)/cellsize) + 1)]
    GRID <- GridTopology(cellcentre.offset = c(x = min(Xrange), y = min(Yrange)) - 0.5 * cellsize, cellsize = c(cellsize, cellsize), cells.dim = dim(Result))
  }
  Result <- SpatialGridDataFrame(grid = GRID, data = data.frame(Ranking = as.vector(Result)), proj4string = proj4string(spPoint))
  rm(GRID)
  gc()
  if("SpatialPoints" %in% class(spPoint)){
    spPoint <- 
      SpatialPointsDataFrame(
        coords = coordinates(spPoint),
        data = over(spPoint, Result),
        proj4string = CRS(proj4string(spPoint))
      )
  } else if("SpatialPointsDataFrame" %in% class(spPoint)){
    spPoint$Ranking <- over(spPoint, Result)$Ranking
  }
  spPoint$Ranking <- as.numeric(factor(spPoint$Ranking)) - 1
  return(list(spPoint = spPoint, GRTS = Result))  
}

#' @export
#' @docType methods
#' @rdname GRTS-methods
#' @aliases GRTS,SpatialPoints-method
#' @method GRTS SpatialPoints-method
#' @keywords methods
#' @seealso \code{\link{GRTS.point}}
setMethod("GRTS", signature(object = "SpatialPoints"), function(object, ...) {
  args <- list(...)
  if("RandomStart" %in% names(args)){
    RandomStart <- args[["RandomStart"]]
  } else {
    RandomStart <- FALSE
  }
  if("cellsize" %in% names(args)){
    GRTS.point(spPoint = object, cellsize = args[["cellsize"]], RandomStart = RandomStart)
  } else {
    GRTS.point(spPoint = object, RandomStart = RandomStart)
  }
})

#' @export
#' @docType methods
#' @rdname GRTS-methods
#' @aliases GRTS,SpatialPointsDataFrame-method
#' @method GRTS SpatialPointsDataFrame-method
setMethod("GRTS", signature(object = "SpatialPointsDataFrame"), function(object, ...) {
  args <- list(...)
  if("RandomStart" %in% names(args)){
    RandomStart <- args[["RandomStart"]]
  } else {
    RandomStart <- FALSE
  }
  if("cellsize" %in% names(args)){
    GRTS.point(spPoint = object, cellsize = args[["cellsize"]], RandomStart = RandomStart)
  } else {
    GRTS.point(spPoint = object, RandomStart = RandomStart)
  }
})
