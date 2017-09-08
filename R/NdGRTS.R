#' @name NdGRTS-methods
#' @aliases NdGRTS
#' @title Calculate the N dimensional GRTS ranking for the object
#' @param object The object on which to generate the N dimensional GRTS ranking
#' @param ... further arguments to the functions
#' @export
#' @docType methods
#' @rdname GRTS-methods
#' @keywords methods
#' @importFrom methods setGeneric
#' @return a data.frame with the design and ranking
setGeneric("NdGRTS", function(object, ...) {
   standardGeneric("NdGRTS") # nocov
})
