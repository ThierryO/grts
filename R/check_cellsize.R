#' check if the cellsize is appropriate
#' @param object the spatial object
#' @param cellsize the optional cellsize. If missing the function will return a relevant cellsize
#' @export
#' @importFrom dplyr %>%
#' @importFrom assertthat has_name assert_that noNA
#' @importFrom sp coordinates
#' @importFrom spatstat as.ppp as.ppp.matrix as.ppp.data.frame owin nndist
check_cellsize <- function(object, cellsize) {
  if (missing(cellsize)) {
    object %>%
      coordinates() %>%
      unique() %>%
      as.ppp(owin(xrange = bbox(object)[1, ], yrange = bbox(object)[2, ])) %>%
      nndist() %>%
      min() %>%
      `/`(3) %>%
      rep(2) -> cellsize
      return(cellsize)
  }
  assert_that(is.numeric(cellsize), msg = "cellsize must be numeric")
  assert_that(noNA(cellsize), msg = "cellsize cannot contain missing values") #nolint
  assert_that(length(cellsize) == 2, msg = "cellsize must be length 2")
  assert_that(all(cellsize > 0), msg = "cellsize must be strict positive")
  return(cellsize)
}
