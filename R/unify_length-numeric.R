#' @export
#' @name unify_length-methods
#' @docType methods
#' @rdname unify_length-methods
#' @aliases unify_length,numeric-method
#' @aliases unify_length
#' @method unify_length numeric-method
#' @importFrom methods setMethod
#' @importFrom assertthat assert_that is.count noNA
#' @include unify_length.R
setMethod("unify_length", signature(x = "numeric"), function(x, new.length) {
  assert_that(is.vector(x))
  assert_that(is.count(new.length))
  assert_that(length(x) <= new.length)
  assert_that(anyDuplicated(x) == 0, msg = "vector contains duplicate values")
  assert_that(noNA(x)) #nolint

  step <- diff(range(x)) / (new.length - 1)
  if (anyDuplicated(round(x / step))) {
    stop("Current new.length = ", new.length, " is too small")
  }
  return(seq(min(x), max(x), length = new.length))
})
