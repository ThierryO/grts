context("NdGRTS on SpatialPoints")
library(sp)
test_that("NdGRTS works on SpatialPoints", {
  set.seed(20180315)
  n <- 10

  object <- SpatialPoints(cbind(X = runif(n), Y = runif(n, max = 2)))
  expect_is(output <- NdGRTS(object), "list")
  expect_identical(names(output), c("object", "design"))
  expect_is(output$object, "SpatialPointsDataFrame")
  expect_identical(length(output$object), length(object))
  expect_is(output$design, "SpatialGridDataFrame")

  cellsize <- c(0.1, 0.2)
  expect_is(output <- NdGRTS(object, cellsize = cellsize), "list")
  expect_equal(gridparameters(output$design)$cellsize, cellsize)

  expect_warning(NdGRTS(object, reference = "Y"))
  expect_warning(NdGRTS(object, scale = "Y"))
})
