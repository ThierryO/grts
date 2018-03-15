context("NdGRTS on SpatialPoints")
library(sp)
test_that("NdGRTS works on SpatialPoints", {
  n <- 10

  object <- SpatialPoints(cbind(X = runif(n), Y = runif(n)))
  expect_is(output <- NdGRTS(object), "list")
  expect_identical(names(output), c("object", "design"))
  expect_is(output$object, "SpatialPointsDataFrame")
  expect_identical(length(output$object), length(object))
  expect_is(output$design, "SpatialGridDataFrame")

  cellsize <- 0.1
  expect_is(output <- NdGRTS(object, cellsize = cellsize), "list")
  expect_equal(
    gridparameters(output$design)$cellsize,
    rep(cellsize, 2)
  )

  expect_warning(NdGRTS(object, reference = "Y"))
})
