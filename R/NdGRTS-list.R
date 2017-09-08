#' @export
#' @name NdGRTS-methods
#' @docType methods
#' @rdname NdGRTS-methods
#' @aliases NdGRTS,numeric-method
#' @aliases NdGRTS
#' @method NdGRTS list-method
#' @importFrom methods setMethod
#' @include NdGRTS.R
setMethod("NdGRTS", signature(object = "list"), function(object, ...) {
  design <- expand.grid(object)
  design$Ranking <- NdRanking(as.matrix(design))
  return(design)
})
