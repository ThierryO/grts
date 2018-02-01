#' @export
#' @name NdGRTS-methods
#' @docType methods
#' @rdname NdGRTS-methods
#' @aliases NdGRTS,list-method
#' @aliases NdGRTS
#' @method NdGRTS list-method
#' @importFrom methods setMethod
#' @importFrom assertthat assert_that has_name is.flag
#' @include NdGRTS.R
setMethod("NdGRTS", signature(object = "list"), function(object, ...) {
  assert_that(
    length(object) > 0,
    msg = "object must contain at least one element"
  )
  assert_that(
    all(sapply(object, is.vector)),
    msg = "all elements of object must be vectors"
  )

  # make all vectors to length 2^x
  n <- sapply(object, length)
  n2 <- max(2 ^ ceiling(log2(n)))
  dots <- list(...)
  if (has_name(dots, "new.length")) {
    n2 <- max(n2, 2 ^ ceiling(log2(dots$new.length)))
  }
  if (has_name(dots, "force")) {
    assert_that(
      is.flag(dots$force),
      msg = "force is not a length one logical vector"
    )
    assert_that(noNA(dots$force), msg = "force contains missing values")
  } else {
    dots$force <- FALSE
  }
  if (!dots$force && n2 ^ length(object) > 1e8) {
    stop(
      "Design would contain ", n2 ^ length(object),
      " objects. Rerun with force = TRUE to continue."
    )
  }

  for (i in seq_along(object)) {
    object[[i]] <- unify_length(object[[i]], n2)
  }

  design <- expand.grid(object)
  design$Ranking <- NdRanking(as.matrix(design))

  return(design)
})
