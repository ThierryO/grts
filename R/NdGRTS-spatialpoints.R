#' @export
#' @name NdGRTS-methods
#' @docType methods
#' @rdname NdGRTS-methods
#' @aliases NdGRTS,SpatialPoints-method
#' @aliases NdGRTS
#' @method NdGRTS SpatialPoints-method
#' @importFrom methods setMethod
#' @importFrom assertthat has_name assert_that noNA
#' @importFrom dplyr %>% mutate_at
#' @importFrom sp coordinates SpatialPixelsDataFrame fullgrid<- over SpatialPointsDataFrame
#' @importFrom spatstat as.ppp as.ppp.matrix as.ppp.data.frame owin nndist
#' @importFrom stats setNames
#' @include NdGRTS-list.R
#' @details
#' - `cellsize` the cellsize for the spatial grid. Defaults to one thirth of the minimum distance between nearest neighbours
setMethod("NdGRTS", signature(object = "SpatialPoints"), function(object, ...) {
  dots <- list(...)
  if (has_name(dots, "reference")) {
    warning("the first coordinate is used as reference")
  }
  if (has_name(dots, "scale")) {
    warning("scale is ignored with SpatialPoints. use cellsize instead")
  }
  if (has_name(dots, "cellsize")) {
    assert_that(is.numeric(dots$cellsize), msg = "cellsize must be numeric")
    assert_that(
      noNA(dots$cellsize),
      msg = "cellsize cannot contain missing values"
    )
    assert_that(length(dots$cellsize) == 2, msg = "cellsize must be length 2")
    assert_that(
      all(dots$cellsize > 0),
      msg = "cellsize must be strict positive"
    )
  } else {
    object %>%
      coordinates() %>%
      as.ppp(owin(xrange = bbox(object)[1, ], yrange = bbox(object)[2, ])) %>%
      nndist() %>%
      min() %>%
      "/"(3) %>%
      rep(2) -> dots$cellsize
  }
  the_names <- rownames(bbox(object))
  ratio <- dots$cellsize / dots$cellsize[1]
  bbox(object) %>%
    apply(1, diff) %>%
    "/"(dots$cellsize) %>%
    max() %>%
    log2() %>%
    ceiling() -> levels
  seq_len(2 ^ levels) %>%
    scale(scale = FALSE) %>%
    as.vector() %>%
    outer(X = dots$cellsize, "*") %>%
    '+'(rowMeans(bbox(object))) %>%
    '/'(ratio) %>%
    t() %>%
    as.data.frame() %>%
    setNames(the_names) %>%
    as.list() %>%
    NdGRTS(
      reference = the_names[1],
      scale = setNames(1, the_names[2])
    ) %>%
    mutate_at(the_names[2], function(x){x * ratio[2]}) -> output
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
