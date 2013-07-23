#'Calculate a GRTS matrix with given dimensions.
#'
#'Create a square matrix containing a randomised order of grid cells based on
#'the GRTS algorithm.
#'
#'
#'@param nrow The number of row and columns of the desired square output
#'matrix. Must be a power of 2.
#'@return A square matrix with the dimensions of \code{object} filled with a
#'randomised order of grid cells.
#'@author Thierry Onkelinx \email{Thierry.Onkelinx@@inbo.be}, Paul Quataert
#'@seealso \code{\link{GRTS}}, \code{\link{QuadratRanking}}
#'@examples
#'
#'  GRTS(4)
#'
GRTS.default <- function(nrow) {
  nrow0 <- 2 ^ ceiling(log2(nrow))
  Result <- QuadratRanking(matrix(0L, ncol = nrow0, nrow = nrow0), Level = 0)
  if((log2(nrow) %% 2) != 0){
    Result <- Result[seq_len(nrow), seq_len(nrow)]
    Result <- matrix(as.numeric(factor(Result)) - 1, nrow = nrow)
  }
  return(Result)
}

#' @name GRTS-methods
#' @title The GRTS wrapper function
#' @description The GRTS wrapper function selects the appropriate GRTS function based on the object.
#' @param object The object on which to generate a GRTS sample
#' @param ... further arguments to the functions
#' @return A GRTS sample
#' @export
#' @docType methods
#' @rdname GRTS-methods
#' @keywords methods
setGeneric("GRTS", function(object, ...) {
   standardGeneric("GRTS")
})

#' @export
#' @name GRTS-methods
#' @docType methods
#' @rdname GRTS-methods
#' @aliases GRTS,numeric-method
#' @method GRTS numeric-method
#' @seealso \code{\link{GRTS.default}}
#' @section Methods:
#' \describe{
#'  \item{\code{signature(object = "numeric")}}{Use \code{GRTS.default}}
#'  \item{\code{signature(object = "SpatialPoints")}}{Use \code{GRTS.point}}
#'  \item{\code{signature(object = "SpatialPointsDataFrame")}}{Use \code{GRTS.point}}
#'  \item{\code{signature(object = "SpatialGrid")}}{Use \code{GRTS.grid}}
#'  \item{\code{signature(object = "SpatialGridDataFrame")}}{Use \code{GRTS.grid}}
#'  \item{\code{signature(object = "SpatialPolygons")}}{Use \code{GRTS.polygon}}
#'  \item{\code{signature(object = "SpatialPolygonsDataFrame")}}{Use \code{GRTS.polygon}}
#' }
#' @examples
#'  GRTS(4)
setMethod("GRTS", signature(object = "numeric"), function(object, ...) {
  GRTS.default(nrow = object)
})

