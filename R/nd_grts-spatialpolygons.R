#' @export
#' @name nd_grts-methods
#' @docType methods
#' @rdname nd_grts-methods
#' @aliases nd_grts,SpatialPolygons-method
#' @aliases nd_grts
#' @method nd_grts SpatialPolygons-method
#' @importFrom methods setMethod
#' @importFrom stats setNames
#' @importFrom sp bbox over SpatialPixelsDataFrame fullgrid<-
#' @importFrom dplyr %>% mutate_at distinct mutate inner_join
#' @include nd_grts-list.R
setMethod(
  "nd_grts",
  signature(object = "SpatialPolygons"),
  function(object, ...) {
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
      stop("cellsize is required on SpatialPolygons")
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
      points = output[, the_names],
      data = output[, c("original_ranking", "ranking")],
      proj4string = object@proj4string
    )
    set <- grid[!is.na(over(grid, object)), "original_ranking"]
    set@data %>%
      distinct(.data$original_ranking) %>%
      mutate(ranking = rank(.data$original_ranking)) %>%
      inner_join(x = set@data, by = "original_ranking") -> set@data
    fullgrid(grid) <- TRUE
    return(list(object = set, design = grid))
  }
)
