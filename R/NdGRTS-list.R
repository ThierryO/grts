#' @export
#' @name NdGRTS-methods
#' @docType methods
#' @rdname NdGRTS-methods
#' @aliases NdGRTS,list-method
#' @aliases NdGRTS
#' @method NdGRTS list-method
#' @importFrom methods setMethod
#' @importFrom assertthat assert_that has_name is.flag is.string noNA
#' @importFrom dplyr %>%
#' @importFrom stats setNames
#' @include NdGRTS.R
#' @details
#' - `new.length` the new length of the unified vector. This will be rounded upwards to a power of 2.
#' - `force` force the calculation of large designs
#' - `reference` the name of the variable used as reference for the fixed aspect ratio
#' - `scale` a named vector with the scales used of the fixed aspect ratio. `reference = "X"` and `scale = c(Y = 2)` will fixed aspect ratio so that `Y / X = 2`
setMethod("NdGRTS", signature(object = "list"), function(object, ...) {
  assert_that(
    length(object) > 0,
    msg = "object must contain at least one element"
  )
  assert_that(
    all(sapply(object, inherits, what = c("numeric", "integer"))),
    msg = "all elements of object must be numeric or integer"
  )
  assert_that(!is.null(names(object)), msg = "object has no names")
  numerics <- sapply(object, inherits, "numeric")
  rep(FALSE, length(object)) %>%
    setNames(names(object)) -> longer

  dots <- list(...)
  if (has_name(dots, "reference")) {
    assert_that(
      is.string(dots$reference),
      msg = "reference must be a single character"
    )
    assert_that(
      has_name(object, dots$reference),
      msg = "reference must match a name in object"
    )
    assert_that(
      has_name(dots, "scale"),
      msg = "scale must be specified when specifying a reference"
    )
    assert_that(is.numeric(dots$scale), msg = "scale must be a numeric vector")
    assert_that(noNA(dots$scale), msg = "scale cannot contain missing values")
    assert_that(
      all(dots$scale > 0),
      msg = "scale must contain only strict positive values"
    )
    assert_that(!is.null(names(dots$scale)), msg = "scale must have names")
    assert_that(
      names(dots$scale) %in% names(object),
      msg = "all names in scale must be present in object"
    )
    assert_that(
      !any(dots$reference %in% names(dots$scale)),
      msg = "scale contains reference"
    )

    object[names(dots$scale)] <- lapply(
      names(dots$scale),
      function(x) {
        object[[x]] / dots$scale[x]
      }
    )

    ref_range <- diff(range(object[[dots$reference]]))
    scale_range <- sapply(
      object[names(dots$scale)],
      function(x){diff(range(x))}
    )
    if (ref_range < max(scale_range)) {
      object[[dots$reference]] <- c(
        object[[dots$reference]],
        min(object[[dots$reference]]) +
          (ref_range + c(-1, 1) * max(scale_range)) / 2
      )
      ref_range <- max(scale_range)
      longer[dots$reference] <- TRUE
    }
    for (x in names(scale_range)[scale_range < ref_range]) {
      object[[x]] <- c(
        object[[x]],
        min(object[[x]]) + (scale_range[x] + c(-1, 1) * ref_range) / 2
      )
      longer[x] <- TRUE
    }
  }

  # make all vectors to length 2^x
  n <- sapply(object, length)
  n2 <- max(2 ^ ceiling(log2(n)))
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
    stop( #nocov start
      "Design would contain ", n2 ^ length(object),
      " objects. Rerun with force = TRUE to continue."
    ) #nocov end
  }

  unified <- lapply(object, unify_length, new.length = n2)
  object[longer] <- lapply(object[longer], head, -2)

  design <- expand.grid(rep(list(seq_len(n2)), length(object)))
  colnames(design) <- names(object)
  design$OriginalRanking <- NdRanking(as.matrix(design))

  if (has_name(dots, "reference")) {
    for (x in names(dots$scale)) {
      unified[[x]] <- unified[[x]] * dots$scale[x]
      object[[x]] <- object[[x]] * dots$scale[x]
    }
  }

  for (i in seq_along(unified)) {
    if (numerics[i]) {
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

  design$Ranking <- rank(design$OriginalRanking)

  return(design)
})
