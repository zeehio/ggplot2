context("geom-hline-vline-abline")


# Visual tests ------------------------------------------------------------

test_that("check h/v/abline transformed on basic projections", {
  dat <- data.frame(x = LETTERS[1:5], y = 1:5)
  plot <- ggplot(dat, aes(x, y)) +
    geom_col(width = 1) +
    geom_point() +
    geom_vline(xintercept = 3, colour = "red") +
    geom_hline(yintercept = 3, colour = "blue") +
    geom_abline(intercept = 0, slope = 1, colour = "purple") +
    labs(x = NULL, y = NULL) +
    coord_cartesian(expand = FALSE)

  vdiffr::expect_doppelganger(
    "cartesian lines intersect mid-bars",
    plot
  )
  vdiffr::expect_doppelganger(
    "flipped lines intersect mid-bars",
    plot + coord_flip()
  )
  vdiffr::expect_doppelganger(
    "polar lines intersect mid-bars",
    plot + coord_polar()
  )
})

test_that("curved lines in map projections", {
  nz <- subset(map_data("nz"), region == "North.Island ")
  nzmap <- ggplot(nz, aes(long, lat, group = group)) +
    geom_path() +
    geom_hline(yintercept = -38.6) + # roughly Taupo
    geom_vline(xintercept = 176) +
    coord_map()

  vdiffr::expect_doppelganger("straight lines in mercator",
    nzmap
  )
  vdiffr::expect_doppelganger("lines curved in azequalarea",
    nzmap + coord_map(projection = 'azequalarea', orientation = c(-36.92, 174.6, 0))
  )
})

test_that("geom_hline and geom_vline accept POSIXct as intercept", {
  xdf <- data.frame(
    datetime = seq(as.POSIXct('2015-01-01'), length.out = 5, by = 'day'))
  plt <- ggplot(xdf, aes(datetime, datetime)) +
    geom_point() +
    geom_vline(aes(xintercept = datetime)) +
    geom_hline(aes(yintercept = datetime)) + scale_x_datetime() + scale_y_datetime()
  plt_build <- ggplot_build(plt)
  expect_true(inherits(plt_build$data[[2]]$xintercept, "POSIXct"))
  expect_true(inherits(plt_build$data[[3]]$yintercept, "POSIXct"))
})

