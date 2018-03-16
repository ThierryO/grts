#' @name nd_grts-methods
#' @aliases nd_grts
#' @title Calculate the N dimensional GRTS ranking for the object
#' @description While GRTS() works only on 2 dimensional data, nd_grts() can work with several dimensions, starting from 1.
#' @param object The object on which to generate the N dimensional GRTS ranking
#' @param ... further arguments to the functions. See Details
#' @details Optional arguments
#' @export
#' @docType methods
#' @rdname nd_grts-methods
#' @keywords methods
#' @importFrom methods setGeneric
#' @return a data.frame with the design and ranking
setGeneric("nd_grts", function(object, ...) {
   standardGeneric("nd_grts") # nocov
})
