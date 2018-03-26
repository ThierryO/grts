#' @export
#' @name nd_grts-methods
#' @docType methods
#' @rdname nd_grts-methods
#' @aliases nd_grts,SpatialPointsDataFrame-method
#' @aliases nd_grts
#' @method nd_grts SpatialPointsDataFrame-method
#' @importFrom methods setMethod
#' @importFrom assertthat has_name assert_that
#' @importFrom sp bbox over SpatialPointsDataFrame coordinates SpatialPixelsDataFrame fullgrid<-
#' @importFrom dplyr %>% mutate inner_join select bind_cols mutate_at distinct
#' @importFrom rlang .data
#' @importFrom stats setNames
#' @include nd_grts-list.R
setMethod(
  "nd_grts",
  signature(object = "SpatialPointsDataFrame"),
  function(object, ...) {
    dots <- list(...)
    if (has_name(dots, "reference")) {
      warning("the first coordinate is used as reference")
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
    object_list <- lapply(object@data, unique)
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
      c(object_list) %>%
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
    level <- lapply(object_list, attr, "levels")
    for (i in names(level)[!sapply(level, is.null)]) {
      unique(object_list[[i]]) %>%
        translate(unique(output[[i]])) %>%
        `[[`("vec") %>%
        `[`(output[[i]]) -> output[[i]]
      output <- output[!is.na(output[[i]]), ]
      output[[i]] <- factor(output[[i]], labels = level[[i]])
      if (inherits(object[[i]], "character")) {
        output[[i]] <- as.character(output[[i]])
      }
    }
    for (i in names(object_list)[sapply(object_list, inherits, "numeric")]) {
      ts <- as.integer(Sys.time())
      unique(object_list[[i]]) %>%
        translate(unique(output[[i]])) %>%
        `[[`("df") %>%
        rename_all(paste0, ts) %>%
        inner_join(output, by = setNames(i, paste0("y", ts))) %>%
        rename_at(paste0("y", ts), function(...) {
          i
        }) %>%
        select(colnames(output)) -> output
    }
    id <- paste0("id", as.integer(Sys.time()))
    output[, id] <- seq_len(nrow(output))
    grid <- SpatialPixelsDataFrame( #nolint
      points = output[, the_names],
      data = output[id],
      proj4string = object@proj4string
    ) %>%
      `fullgrid<-`(TRUE)
    grid %>%
      over(x = object) %>%
      inner_join(
        output %>%
          select(c(id, the_names)),
        by = id
      ) %>%
      select(-1) %>%
      bind_cols(object@data) %>%
      inner_join(output, by = c(the_names, colnames(object@data))) %>%
      select(c(colnames(object@data), "original_ranking")) %>%
      SpatialPointsDataFrame(coords = coordinates(object)) -> set
    set@data %>%
      distinct(.data$original_ranking) %>%
      mutate(ranking = rank(.data$original_ranking) - 1) %>%
      inner_join(x = set@data, by = "original_ranking") -> set@data
    return(list(object = set, design = output[names(output) != id]))
  }
)
