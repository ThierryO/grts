#' @export
#' @name nd_grts-methods
#' @docType methods
#' @rdname nd_grts-methods
#' @aliases nd_grts,data.frame-method
#' @aliases nd_grts
#' @method nd_grts data.frame-method
#' @importFrom methods setMethod
#' @include nd_grts-list.R
setMethod("nd_grts", signature(object = "data.frame"), function(object, ...) {
  object_list <- lapply(object, unique)
  for (i in which(sapply(object_list, is.integer))) {
    object_list[[i]] <- min(object_list[[i]]):max(object_list[[i]])
  }
  for (i in which(sapply(object_list, is.character))) {
    object_list[[i]] <- factor(object_list[[i]])
  }
  for (i in which(sapply(object_list, is.factor))) {
    this_level <- levels(object_list[[i]])
    object_list[[i]] <- as.integer(object_list[[i]])
    attr(object_list[[i]], "levels") <- this_level
  }

  design <- nd_grts(object_list, ...)
  level <- lapply(object_list, attr, "levels")
  for (i in names(level)[!sapply(level, is.null)]) {
    design[, i] <- factor(design[, i], labels = level[[i]])
  }
  for (i in colnames(object)[sapply(object, inherits, "numeric")]) {
    d <- sort(unique(design[, i]))
    object[, i] <- d[cut(object[, i], breaks = c(-Inf, d))]
  }
  object <- merge(object, design)
  object$ranking <- rank(object$original_ranking)

  return(list(object = object, design = design))
})
