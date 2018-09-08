context("raster")

test_that("bump diconnected cells", {
  set.seed(0)
  r_top <- raster::raster(ncols = 10, nrows = 10)
  r_bot <- raster::raster(ncols = 10, nrows = 10)
  r_top[] <- rnorm(raster::ncell(r_top), mean = 12)
  r_bot[] <- rnorm(raster::ncell(r_bot), mean = 10)
  rs <- raster::stack(r_top, r_bot)
  r <- BumpDisconnectCells(rs, min.overlap = 0.1)

  expect_is(r, "RasterLayer")
  expect_equal(sum(as.vector(r)), -12.6)
  expect_warning(BumpDisconnectCells(rs, max.itr=10))
})
