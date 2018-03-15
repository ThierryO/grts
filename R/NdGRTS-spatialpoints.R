#' @export
#' @name NdGRTS-methods
#' @docType methods
#' @rdname NdGRTS-methods
#' @aliases NdGRTS,SpatialPoints-method
#' @aliases NdGRTS
#' @method NdGRTS SpatialPoints-method
#' @importFrom methods setMethod
#' @importFrom assertthat has_name assert_that is.number
#' @importFrom dplyr %>%
#' @importFrom sp coordinates SpatialPixelsDataFrame fullgrid<- over SpatialPointsDataFrame
#' @importFrom spatstat as.ppp as.ppp.matrix as.ppp.data.frame owin nndist
#' @importFrom stats setNames
#' @include NdGRTS-data_frame.R
#' @details
#' - `cellsize` the cellsize for the spatial grid. Defaults to one thirth the minimum distance between nearest neighbours
setMethod("NdGRTS", signature(object = "SpatialPoints"), function(object, ...) {
  dots <- list(...)
  if (has_name(dots, "reference")) {
    warning("the aspect ratio is current fixed to 1 on SpatialPoints")
  }
  if (has_name(dots, "cellsize")) {
    assert_that(
      is.number(dots$cellsize),
      msg = "cellsize must be a single number"
    )
    assert_that(dots$cellsize > 0, msg = "cellsize must be strict positive")
  } else {
    object %>%
      coordinates() %>%
      as.ppp(owin(xrange = bbox(object)[1, ], yrange = bbox(object)[2, ])) %>%
      nndist() %>%
      min() %>%
      "/"(3) -> dots$cellsize
  }
  bbox(object) %>%
    apply(1, diff) %>%
    "/"(dots$cellsize) %>%
    max() %>%
    log2() %>%
    ceiling() -> levels
  seq_len(2 ^ levels) %>%
    scale(scale = FALSE) %>%
    as.vector() %>%
    "*"(dots$cellsize) %>%
    outer(rowMeans(bbox(object)), "+") %>%
    as.data.frame() %>%
    as.list() %>%
    NdGRTS(
      reference = rownames(bbox(object))[1],
      scale = setNames(1, rownames(bbox(object))[2]),
      ...
    ) -> output
  grid <- SpatialPixelsDataFrame(
    points = output[, 1:2],
    data = output[, 3:4],
    proj4string = object@proj4string
  ) %>%
    `fullgrid<-`(TRUE)
  set <- SpatialPointsDataFrame(
    object,
    data = over(object, grid)
  )
  set$Ranking <- rank(set$OriginalRanking)
  return(list(object = set, design = grid))
})
