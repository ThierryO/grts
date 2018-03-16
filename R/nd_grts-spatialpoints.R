#' @export
#' @name nd_grts-methods
#' @docType methods
#' @rdname nd_grts-methods
#' @aliases nd_grts,SpatialPoints-method
#' @aliases nd_grts
#' @method nd_grts SpatialPoints-method
#' @importFrom methods setMethod
#' @importFrom assertthat has_name assert_that noNA
#' @importFrom dplyr %>% mutate_at
#' @importFrom sp coordinates SpatialPixelsDataFrame fullgrid<- over SpatialPointsDataFrame
#' @importFrom spatstat as.ppp as.ppp.matrix as.ppp.data.frame owin nndist
#' @importFrom stats setNames
#' @include nd_grts-list.R
#' @details
#' - `cellsize` the cellsize for the spatial grid. Defaults to one thirth of the minimum distance between nearest neighbours
setMethod(
  "nd_grts",
  signature(object = "SpatialPoints"),
  function(object, ...)
{
  dots <- list(...)
  if (has_name(dots, "reference")) {
    warning("the first coordinate is used as reference")
  }
  if (has_name(dots, "scale")) {
    warning("scale is ignored with SpatialPoints. use cellsize instead")
  }
  if (has_name(dots, "cellsize")) {
    dots$cellsize <- check_cellsize(object, dots$cellsize)
  } else {
    dots$cellsize <- check_cellsize(object)
  }
  the_names <- rownames(bbox(object))
  ratio <- dots$cellsize / dots$cellsize[1]
  bbox(object) %>%
    apply(1, diff) %>%
    `/`(dots$cellsize) %>%
    max() %>%
    log2() %>%
    ceiling() -> levels
  seq_len(2 ^ levels) %>%
    scale(scale = FALSE) %>%
    as.vector() %>%
    outer(X = dots$cellsize, "*") %>%
    `+`(rowMeans(bbox(object))) %>%
    `/`(ratio) %>%
    t() %>%
    as.data.frame() %>%
    setNames(the_names) %>%
    as.list() %>%
    nd_grts(
      reference = the_names[1],
      scale = setNames(1, the_names[2])
    ) %>%
    mutate_at(
      the_names[2],
      function(x){
        x * ratio[2]
      }
    ) -> output
  grid <- SpatialPixelsDataFrame( #nolint
    points = output[, 1:2],
    data = output[, 3:4],
    proj4string = object@proj4string
  ) %>%
    `fullgrid<-`(TRUE)
  set <- SpatialPointsDataFrame( #nolint
    object,
    data = over(object, grid)
  )
  set$ranking <- rank(set$original_ranking)
  return(list(object = set, design = grid))
})
