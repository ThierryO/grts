expect_equal(
  vanderCorput(10, base = 2, start = 1),
  list(
    sequence = c(0.25, .75, .125, .625, .375, .875, .0625, .5626, .3125, .8125),
    start = 1,
    base = 2
  ),
  tolerance = 2e-4
)
