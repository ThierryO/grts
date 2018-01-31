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
setMethod("unify_length", signature(x = "numeric"), function(x, new.length) {
  assert_that(is.vector(x))
  assert_that(is.count(new.length))
  assert_that(length(x) <= new.length)
  assert_that(anyDuplicated(x) == 0, msg = "vector contains duplicate values")
  assert_that(noNA(x))

  min_diff <- min(diff(sort(x)))
  step <- diff(range(x)) / new.length
  if (min_diff < step) {
    stop(
      "new.length is too small. Increase it to at least ",
      ceiling(diff(range(x)) / min_diff)
    )
  }

  return(seq(min(x), by = step, length = new.length))
})
