#' GRTS sampling on grids
#' 
#' @param spGrid A \code{\link{SpatialGrid}} or \code{\link{SpatialGridDataFrame}} object
#' @param ... Ignored
#' @export
#' @importFrom sp SpatialGridDataFrame
#' @return a SpatialGridDataFrame object with a variable 'Ranking' indicating the GRTS order
GRTS.grid <- function(spGrid, ...){
  DimGrid <- max(spGrid@grid@cells.dim)
  DimGrid <- 2 ^ ceiling(log2(DimGrid))
  Result <- QuadratRanking(matrix(0L, ncol = DimGrid, nrow = DimGrid), Level = 0)
  Result <- Result[seq_len(spGrid@grid@cells.dim[1]), ]
  Result <- Result[, seq_len(spGrid@grid@cells.dim[2])]
  Result <- as.vector(Result)
  if(class(spGrid) == "Spatialgrid"){
    spGrid <- SpatialGridDataFrame(
      grid = spGrid@grid, 
      data = data.frame(
        Ranking = as.integer(factor(Result)) - 1
      ),
      proj4string = spGrid@proj4string
    )
  } else {
    spGrid$Ranking <- as.integer(factor(Result)) - 1
  }
  return(spGrid)  
}

#' @export
#' @docType methods
#' @rdname GRTS-methods
#' @aliases GRTS,SpatialGrid-method
#' @method GRTS SpatialGrid-method
#' @keywords methods
#' @seealso \code{\link{GRTS.grid}}
setMethod("GRTS", signature(object = "SpatialGrid"), function(object, ...) {
  GRTS.grid(spGrid = object, ...)
})

#' @export
#' @docType methods
#' @rdname GRTS-methods
#' @aliases GRTS,SpatialGridDataFrame-method
#' @method GRTS SpatialGridDataFrame-method
setMethod("GRTS", signature(object = "SpatialGridDataFrame"), function(object, ...) {
  GRTS.grid(spGrid = object, ...)
})
