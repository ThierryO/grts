context("nd_grts on SpatialPoints")
library(sp)
test_that("nd_grts works on SpatialPoints", {
  set.seed(20180315)
  n <- 10

  object <- SpatialPoints(cbind(x = runif(n), y = runif(n, max = 2))) #nolint
  expect_is(output <- nd_grts(object), "list")
  expect_named(output, c("object", "design"))
  expect_is(output$object, "SpatialPointsDataFrame")
  expect_identical(length(output$object), length(object))
  expect_is(output$design, "SpatialGridDataFrame")
  expect_identical(
    length(unique(output$object$original_ranking)),
    nrow(unique(output$object@data))
  )
  expect_identical(
    length(unique(output$design$original_ranking)),
    nrow(unique(output$design@data))
  )

  cellsize <- c(0.1, 0.2)
  expect_is(output <- nd_grts(object, cellsize = cellsize), "list")
  expect_equal(gridparameters(output$design)$cellsize, cellsize)
  expect_identical(
    length(unique(output$object$original_ranking)),
    nrow(unique(output$object@data))
  )
  expect_identical(
    length(unique(output$design$original_ranking)),
    nrow(unique(output$design@data))
  )

  cellsize <- c(0.5, 0.5)
  expect_is(output <- nd_grts(object, cellsize = cellsize), "list")
  expect_identical(
    length(unique(output$object$original_ranking)),
    nrow(unique(output$object@data))
  )
  expect_identical(
    length(unique(output$design$original_ranking)),
    nrow(unique(output$design@data))
  )
  expect_identical(
    length(unique(output$design$ranking)),
    nrow(unique(output$design@data))
  )

  expect_warning(nd_grts(object, reference = "Y"))
  expect_warning(nd_grts(object, scale = "Y"))
})

test_that("nd_grts works on SpatialPointsDataFrame", {
  set.seed(20180315)
  n <- 20

  object <- SpatialPointsDataFrame(
    coords = cbind(
      X = sample(0:3, n, replace = TRUE),
      Y = sample(0:3, n, replace = TRUE)
    ),
    data = data.frame(A = sample(0:1, n, replace = TRUE))
  )
  expect_is(output <- nd_grts(object), "list")
  expect_named(output, c("object", "design"))
  expect_is(output$object, "SpatialPointsDataFrame")
  expect_identical(length(output$object), length(object))
  expect_is(output$design, "data.frame")
  expect_identical(
    length(unique(output$object$original_ranking)),
    nrow(unique(output$object@data[, c("original_ranking", "ranking")]))
  )
  expect_identical(
    length(unique(output$design$original_ranking)),
    nrow(unique(output$design[, c("original_ranking", "ranking")]))
  )
  expect_identical(
    names(output$object),
    c(names(object), "original_ranking", "ranking")
  )
  expect_identical(
    sort(names(output$design)),
    as.data.frame(object[1, ]) %>%
      colnames() %>%
      c("original_ranking", "ranking") %>%
      sort()
  )

  expect_is(output <- nd_grts(object, cellsize = c(2, 2)), "list")
  expect_identical(
    length(unique(output$object$original_ranking)),
    nrow(unique(output$object@data[, c("original_ranking", "ranking")]))
  )
  expect_identical(
    length(unique(output$design$original_ranking)),
    nrow(unique(output$design[, c("original_ranking", "ranking")]))
  )

  expect_is(
    output <- nd_grts(object, cellsize = c(1, 1), scale = c(X = 1, A = 1)),
    "list"
  )
  expect_identical(
    length(unique(output$object$original_ranking)),
    nrow(unique(output$object@data[, c("original_ranking", "ranking")]))
  )
  expect_identical(
    length(unique(output$design$original_ranking)),
    nrow(unique(output$design[, c("original_ranking", "ranking")]))
  )

  expect_warning(nd_grts(object, reference = "Y"))

  object@data$A <- object@data$A * 1.5
  expect_is(output <- nd_grts(object), "list")
  expect_identical(
    sapply(output$object@data[, names(object), drop = FALSE], class),
    sapply(object@data, class)
  )

  object@data$A <- as.character(object@data$A)
  expect_is(output <- nd_grts(object), "list")
  expect_identical(
    sapply(output$object@data[, names(object), drop = FALSE], class),
    sapply(object@data, class)
  )

  object@data$A <- factor(object@data$A)
  expect_is(output <- nd_grts(object), "list")
  expect_identical(
    sapply(output$object@data[, names(object), drop = FALSE], class),
    sapply(object@data, class)
  )
  expect_identical(
    levels(output$object$A),
    levels(object$A)
  )
})
