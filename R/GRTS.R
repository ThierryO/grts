GRTS.default <- function(nrow) {
  nrow0 <- 2 ^ ceiling(log2(nrow))
  Result <- QuadratRanking(matrix(0L, ncol = nrow0, nrow = nrow0), Level = 0)
  if((log2(nrow) %% 2) != 0){
    Result <- Result[seq_len(nrow), seq_len(nrow)]
    Result <- matrix(as.numeric(factor(Result)) - 1, nrow = nrow)
  }
  return(Result)
}

setGeneric("GRTS", function(object, ...) {
   standardGeneric("GRTS")
})

setMethod("GRTS", signature(object = "numeric"), function(object, ...) {
  GRTS.default(nrow = object)
})

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

GRTS.polygon <- function(spPolygon, cellsize, Subset = FALSE, RandomStart = FALSE){
  Xrange <- bbox(spPolygon)[1, ]
  Yrange <- bbox(spPolygon)[2, ]
  DimGrid <- ceiling(max(diff(Xrange), diff(Yrange)) / cellsize)
  DimGrid <- 2 ^ ceiling(log2(DimGrid))

  Result <- QuadratRanking(matrix(0L, ncol = DimGrid, nrow = DimGrid), Level = 0)
  if(RandomStart){
    Result <- Result[seq_len(ceiling(diff(Xrange)/cellsize) + 1), seq_len(ceiling(diff(Yrange)/cellsize) + 1)]
  } else {
    Result <- Result[seq_len(ceiling(diff(Xrange)/cellsize)), seq_len(ceiling(diff(Yrange)/cellsize))]
  }
  if(RandomStart){
    GRID <- GridTopology(cellcentre.offset = c(x = min(Xrange), y = min(Yrange)) + runif(2, min = -cellsize, max = 0), cellsize = c(cellsize, cellsize), cells.dim = dim(Result))
  } else {
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
