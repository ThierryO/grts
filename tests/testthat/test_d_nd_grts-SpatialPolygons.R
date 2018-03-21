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

context("nd_grts on SpatialPolygonsDataFrame")
test_that("nd_grts works on SpatialPolygonsDataFrame", {
  object <- list(
    Polygons(list(Polygon(cbind(c(0, 1, 1, 0), c(0, 0, 1, 0)))), ID = 1),
    Polygons(list(Polygon(cbind(2 + c(0, 1, 1, 0), c(0, 0, 1, 0)))), ID = 2),
    Polygons(list(Polygon(cbind(c(0, 1, 1, 0), 2 + c(0, 0, 1, 0)))), ID = 3),
    Polygons(list(Polygon(cbind(2 + c(0, 1, 1, 0), 2 + c(0, 0, 1, 0)))), ID = 4)
  ) %>%
    SpatialPolygons() %>%
    SpatialPolygonsDataFrame(data.frame(A = c(0, 0, 1, 1)))
  cellsize <- c(0.5, 0.5)
  expect_is(
    output <- nd_grts(object, cellsize = cellsize),
    "list"
  )
  expect_named(output, c("object", "design"))
  expect_is(output$object, "SpatialPointsDataFrame")
  expect_is(output$design, "data.frame")
  expect_identical(
    length(unique(output$object$original_ranking)),
    nrow(unique(output$object@data))
  )
  expect_identical(
    length(unique(output$design$original_ranking)),
    nrow(output$design)
  )
  expect_equal(
    output$design %>%
      distinct(x) %>%
      slice(1:2) %>%
      pull(1) %>%
      diff(),
    cellsize[1]
  )
  expect_equal(
    output$design %>%
      distinct(y) %>%
      slice(1:2) %>%
      pull(1) %>%
      diff(),
    cellsize[2]
  )

  output$design %>%
    select(x, y) %>%
    summarise_all(min) %>%
    `<=`(bbox(object)[, 1]) %>%
    all() %>%
    expect_true()
  output$design %>%
    select(x, y) %>%
    summarise_all(max) %>%
    `>=`(bbox(object)[, 2]) %>%
    all() %>%
    expect_true()

  expect_error(
    nd_grts(object),
    "cellsize is required"
  )
  expect_warning(
    nd_grts(object, cellsize = cellsize, reference = "x")
  )
})
