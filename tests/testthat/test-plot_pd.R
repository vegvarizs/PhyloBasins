# =============================================================================
# Tests for plot_pd()
# =============================================================================

test_that("plot_pd() returns a ggplot object", {

  pb <- pb_test_project(stage = "pd")

  pb <- read_geometry(
    pb,
    file = system.file(
      "extdata",
      "example_geometry.geojson",
      package = "PhyloBasins"
    ),
    verbose = FALSE
  )

  pb <- prepare_geometry(
    pb,
    geometry_id = "SiteID",
    verbose = FALSE
  )

  p <- plot_pd(pb)

  expect_s3_class(
    p,
    "ggplot"
  )

})

# ------------------------------------------------------------------------------

test_that("plot_pd() forwards optional arguments", {

  pb <- pb_test_project(stage = "pd")

  pb <- read_geometry(
    pb,
    file = system.file(
      "extdata",
      "example_geometry.geojson",
      package = "PhyloBasins"
    ),
    verbose = FALSE
  )

  pb <- prepare_geometry(
    pb,
    geometry_id = "SiteID",
    verbose = FALSE
  )

  p <- plot_pd(
    pb,
    palette = "magma",
    legend_title = "PD"
  )

  expect_s3_class(
    p,
    "ggplot"
  )

})

# ------------------------------------------------------------------------------

test_that("plot_pd() rejects projects without PD", {

  pb <- pb_test_project(stage = "community")

  pb <- read_geometry(
    pb,
    file = system.file(
      "extdata",
      "example_geometry.geojson",
      package = "PhyloBasins"
    ),
    verbose = FALSE
  )

  pb <- prepare_geometry(
    pb,
    geometry_id = "SiteID",
    verbose = FALSE
  )

  expect_error(
    plot_pd(pb),
    "has no values"
  )

})
