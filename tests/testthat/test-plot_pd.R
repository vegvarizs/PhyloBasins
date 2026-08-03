# =============================================================================
# test-plot_pd.R
# =============================================================================

test_that("plot_pd() returns a ggplot object", {

  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")

  pb <- pb_test_project(stage = "pd")

  shape <- sf::st_as_sf(

    data.frame(

      HYBAS_ID = c("S1", "S2"),

      x = c(0, 1),

      y = c(0, 1)

    ),

    coords = c("x", "y"),

    crs = 4326

  )

  shape <- sf::st_buffer(shape, dist = 0.1)

  p <- plot_pd(
    pb,
    shape
  )

  expect_s3_class(
    p,
    "ggplot"
  )

})



test_that("plot_pd() forwards optional arguments", {

  skip_if_not_installed("sf")
  skip_if_not_installed("ggplot2")

  pb <- pb_test_project(stage = "pd")

  shape <- sf::st_as_sf(

    data.frame(

      HYBAS_ID = c("S1", "S2"),

      x = c(0, 1),

      y = c(0, 1)

    ),

    coords = c("x", "y"),

    crs = 4326

  )

  shape <- sf::st_buffer(shape, dist = 0.1)

  p <- plot_pd(

    pb,

    shape,

    palette = "magma",

    legend_title = "PD"

  )

  expect_s3_class(
    p,
    "ggplot"
  )

})
