#' @export
#' @name NdGRTS-methods
#' @docType methods
#' @rdname NdGRTS-methods
#' @aliases NdGRTS,list-method
#' @aliases NdGRTS
#' @method NdGRTS list-method
#' @importFrom methods setMethod
#' @importFrom assertthat assert_that
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
  for (i in which(n < n2)) {
    link <- unify_length(object[[i]], n2)
    object[[i]] <- link[, "new"]
    attr(object[[i]], "link") <- link
  }

  design <- expand.grid(object)
  design$Ranking <- NdRanking(as.matrix(design))

  for (i in which(n < n2)) {
    link <- attr(object[[i]], "link")
    if (any(is.na(link[, "old"]))) {
      link <- as.data.frame(link[!is.na(link[, "old"]), ])
      colnames(link)[2] <- paste0("old", as.integer(Sys.time()))
      design <- merge(design, link, by.x = names(object)[i], by.y = "new")
      design[, names(object)[i]] <- design[, colnames(link)[2]]
      design[, colnames(link)[2]] <- NULL
    }
  }
  if (any(n < n2)) {
    design$Ranking <- rank(design$Ranking)
  }

  return(design[, c(names(object), "Ranking")])
})
