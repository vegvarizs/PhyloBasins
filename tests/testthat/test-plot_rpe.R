# =============================================================================
# Tests for plot_rpe()
# =============================================================================

test_that("plot_rpe() returns a ggplot object", {

  pb <- pb_test_project(stage = "rpe")

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

  p <- plot_rpe(pb)

  expect_s3_class(
    p,
    "ggplot"
  )

})

# ------------------------------------------------------------------------------

test_that("plot_rpe() forwards optional arguments", {

  pb <- pb_test_project(stage = "rpe")

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

  p <- plot_rpe(
    pb,
    palette = "magma",
    legend_title = "RPE"
  )

  expect_s3_class(
    p,
    "ggplot"
  )

})

# ------------------------------------------------------------------------------

test_that("plot_rpe() rejects projects without RPE", {

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
    plot_rpe(pb),
    "has not been computed"
  )

})
# ------------------------------------------------------------------------------

test_that("plot_rpe() accepts different palettes", {

  pb <- pb_test_project(stage = "rpe")

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

  expect_s3_class(
    plot_rpe(
      pb,
      palette = "viridis"
    ),
    "ggplot"
  )

  expect_s3_class(
    plot_rpe(
      pb,
      palette = "plasma"
    ),
    "ggplot"
  )

  expect_s3_class(
    plot_rpe(
      pb,
      palette = "magma"
    ),
    "ggplot"
  )

})

# ------------------------------------------------------------------------------

test_that("plot_rpe() rejects unknown palette", {

  pb <- pb_test_project(stage = "rpe")

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
    plot_rpe(
      pb,
      palette = "foobar"
    ),
    "Unknown palette"
  )

})
