context("nd_grts on SpatialPolygons")
library(sp)
library(dplyr)
test_that("nd_grts works on SpatialPolygons", {
  object <- SpatialPolygons(list(
    Polygons(list(Polygon(cbind(c(0, 1, 1, 0), c(0, 0, 1, 0)))), ID = 1),
    Polygons(list(Polygon(cbind(2 + c(0, 1, 1, 0), c(0, 0, 1, 0)))), ID = 2),
    Polygons(list(Polygon(cbind(c(0, 1, 1, 0), 2 + c(0, 0, 1, 0)))), ID = 3),
    Polygons(list(Polygon(cbind(2 + c(0, 1, 1, 0), 2 + c(0, 0, 1, 0)))), ID = 4)
  ))
  cellsize <- c(0.5, 0.5)
  expect_is(
    output <- nd_grts(object, cellsize = cellsize),
    "list"
  )
  expect_named(output, c("object", "design"))
  expect_is(output$object, "SpatialPixelsDataFrame")
  expect_is(output$design, "SpatialGridDataFrame")
  expect_identical(
    length(unique(output$object$original_ranking)),
    nrow(unique(output$object@data))
  )
  expect_identical(
    length(unique(output$design$original_ranking)),
    nrow(unique(output$design@data))
  )
  expect_equal(
    gridparameters(output$design)$cellsize,
    cellsize
  )
  expect_true(all(bbox(output$design)[, 1] <= bbox(object)[, 1]))
  expect_true(all(bbox(output$design)[, 2] >= bbox(object)[, 2]))

  expect_error(
    nd_grts(object, scale = c(0.5, 0.5), reference = "X"),
    "cellsize is required"
  )

})
