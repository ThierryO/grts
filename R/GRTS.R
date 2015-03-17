GRTS.default <- function(nrow) {
  if(log2(nrow) %% 1 != 0){
    warning("nrow must be a power of 2. It is increased to the nearest power of 2.")
    nrow <- 2 ^ ceiling(log2(nrow))
  }  
  QuadratRanking(matrix(0L, ncol = nrow, nrow = nrow), Level = 0)
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
  Result <- Result[seq_len(ceiling(diff(Xrange)/cellsize)), seq_len(ceiling(diff(Yrange)/cellsize))]
  if(RandomStart){
    GRID <- GridTopology(cellcentre.offset = c(x = min(Xrange), y = min(Yrange)) + runif(2, min = 0, max = cellsize), cellsize = c(cellsize, cellsize), cells.dim = dim(Result))
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
    Result <- Result[order(Result$Ranking), ]
    Result$Ranking <- seq_along(Result$Ranking)
  }
  Result
}
