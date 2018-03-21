#' @export
#' @name unify_length-methods
#' @docType methods
#' @rdname unify_length-methods
#' @aliases unify_length,integer-method
#' @aliases unify_length
#' @method unify_length integer-method
#' @importFrom methods setMethod
#' @include unify_length.R
setMethod("unify_length", signature(x = "integer"), function(x, new.length) {
  return(unify_length(x = as.numeric(x), new.length = new.length))
})
