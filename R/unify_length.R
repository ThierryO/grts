#' @name unify_length-methods
#' @aliases unify_length
#' @title Expand a vector to given length
#' @description Dummy intermediate values will be added.
#' @param x the vector to expand
#' @param new.length the required length of the vector
#' @export
#' @docType methods
#' @rdname unify_length-methods
#' @keywords methods
#' @importFrom methods setGeneric
#' @return a data.frame with the design and ranking
setGeneric("unify_length", function(x, new.length) {
   standardGeneric("unify_length") # nocov
})
