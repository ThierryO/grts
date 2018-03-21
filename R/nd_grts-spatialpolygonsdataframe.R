#' @export
#' @name nd_grts-methods
#' @docType methods
#' @rdname nd_grts-methods
#' @aliases nd_grts,SpatialPolygonsDataFrame-method
#' @aliases nd_grts
#' @method nd_grts SpatialPolygonsDataFrame-method
#' @importFrom methods setMethod
#' @importFrom assertthat has_name assert_that
#' @importFrom sp bbox over SpatialPointsDataFrame coordinates
#' @importFrom dplyr %>% mutate inner_join select bind_cols mutate_at distinct select_at
#' @importFrom rlang .data
#' @importFrom stats setNames
#' @include nd_grts-list.R
setMethod(
  "nd_grts",
  signature(object = "SpatialPolygonsDataFrame"),
  function(object, ...) {
    dots <- list(...)
    if (has_name(dots, "reference")) {
      warning("the first coordinate is used as reference")
    }
    if (has_name(dots, "cellsize")) {
      dots$cellsize <- check_cellsize(object, dots$cellsize)
    } else {
      stop("cellsize is required on SpatialPolygonsDataFrame")
    }
    the_names <- rownames(bbox(object))
    ratio <- dots$cellsize / dots$cellsize[1]
    bbox(object) %>%
      apply(1, diff) %>%
      `/`(dots$cellsize) %>%
      max() %>%
      log2() %>%
      ceiling() -> levels
    if (has_name(dots, "scale")) {
      if (any(names(dots$scale) %in% the_names)) {
        dots$scale <- dots$scale[!names(dots$scale) %in% the_names]
      }
    } else {
      dots$scale <- numeric(0)
    }
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
      c(
        object@data %>%
          lapply(unique)
      ) %>%
      nd_grts(
        reference = the_names[1],
        scale = c(setNames(1, the_names[2]), dots$scale)
      ) %>%
      mutate_at(
        the_names[2],
        function(x){
          x * ratio[2]
        }
      ) -> output
    id <- paste0("id", as.integer(Sys.time()))
    object@data[, id] <- seq_along(object)
    output %>%
      select_at(the_names) %>%
      SpatialPoints(proj4string = object@proj4string) %>%
      over(object[id]) %>%
      bind_cols(output) %>%
      inner_join(object@data, by = names(object)) %>%
      select(-1) %>%
      mutate(ranking = rank(.data$original_ranking)) -> set
    set <- SpatialPointsDataFrame(
      coords = set[, the_names],
      data = set[, !colnames(set) %in% the_names]
    )
    return(list(object = set, design = output))
  }
)
