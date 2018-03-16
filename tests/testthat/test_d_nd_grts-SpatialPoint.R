context("nd_grts on SpatialPoints")
library(sp)
test_that("nd_grts works on SpatialPoints", {
  set.seed(20180315)
  n <- 10

  object <- SpatialPoints(cbind(x = runif(n), y = runif(n, max = 2))) #nolint
  expect_is(output <- nd_grts(object), "list")
  expect_identical(names(output), c("object", "design"))
  expect_is(output$object, "SpatialPointsDataFrame")
  expect_identical(length(output$object), length(object))
  expect_is(output$design, "SpatialGridDataFrame")

  cellsize <- c(0.1, 0.2)
  expect_is(output <- nd_grts(object, cellsize = cellsize), "list")
  expect_equal(gridparameters(output$design)$cellsize, cellsize)

  expect_warning(nd_grts(object, reference = "Y"))
  expect_warning(nd_grts(object, scale = "Y"))
})

# test_that("nd_grts works on SpatialPointsDataFrame", {
#   set.seed(20180315)
#   n <- 20
#
#   object <- SpatialPointsDataFrame(
#     coords = cbind(X = runif(n, max = 100), Y = runif(n, max = 100)),
#     data = data.frame(A = sample(0:3, n, replace = TRUE), B = runif(n))
#   )
#   expect_is(output <- nd_grts(object), "list")
# })
