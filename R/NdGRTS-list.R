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
  assert_that(
    all(sapply(object, inherits, what = c("numeric", "integer"))),
    msg = "all elements of object must be numeric or integer"
  )
  assert_that(!is.null(names(object)), msg = "object has no names")

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
    ) # nocov
  }


  unified <- lapply(object, unify_length, new.length = n2)

  design <- expand.grid(rep(list(seq_len(n2)), length(object)))
  colnames(design) <- names(object)
  design$Ranking <- NdRanking(as.matrix(design))

  for (i in seq_along(unified)) {
    if (inherits(object[[i]], "numeric")) {
      design[, i] <- unified[[i]][design[, i]]
    } else {
      distance <- abs(outer(unified[[i]], object[[i]], "-"))
      nearest <- apply(distance, 2, which.min)
      design <- design[design[, i] %in% nearest, ]
      z <- vector("integer", n2)
      z[nearest] <- object[[i]]
      design[, i] <- z[design[, i]]
    }
  }

  return(design)
})
