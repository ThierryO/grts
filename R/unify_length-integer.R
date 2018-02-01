#' @export
#' @name unify_length-methods
#' @docType methods
#' @rdname unify_length-methods
#' @aliases unify_length,integer-method
#' @aliases unify_length
#' @method unify_length integer-method
#' @importFrom methods setMethod
#' @importFrom assertthat assert_that is.count noNA
#' @include unify_length.R
setMethod("unify_length", signature(x = "integer"), function(x, new.length) {
  assert_that(is.vector(x))
  assert_that(is.count(new.length))
  assert_that(length(x) <= new.length)
  assert_that(anyDuplicated(x) == 0, msg = "vector contains duplicate values")
  assert_that(noNA(x))

  new.x <- seq(min(x), max(x), length = new.length)
  distance <- abs(outer(new.x, x, "-"))
  nearest <- apply(distance, 2, which.min)
  if (anyDuplicated(nearest)) {
    stop("Current new.length = ", new.length, " is too small")
  }

  return(new.x)
})
