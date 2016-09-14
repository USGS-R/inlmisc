context("raster")
library(raster)

test_that("bump diconnected cells", {
  set.seed(0)
  r.top <- raster(ncols=10, nrows=10)
  r.bot <- raster(ncols=10, nrows=10)
  r.top[] <- rnorm(ncell(r.top), mean=12)
  r.bot[] <- rnorm(ncell(r.bot), mean=10)
  rs <- stack(r.top, r.bot)
  r <- BumpDisconnectCells(rs, min.overlap=0.1)

  expect_is(r, "RasterLayer")
  expect_equal(sum(as.vector(r)), -12.6)
  expect_warning(BumpDisconnectCells(rs, max.itr=10))
})
