# =============================================================================
# Tests for plot_pe()
# =============================================================================

test_that("plot_pe() returns a ggplot object", {

  pb <- pb_test_project(stage = "pe")

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

  p <- plot_pe(pb)

  expect_s3_class(
    p,
    "ggplot"
  )

})

# ------------------------------------------------------------------------------

test_that("plot_pe() forwards optional arguments", {

  pb <- pb_test_project(stage = "pe")

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

  p <- plot_pe(
    pb,
    palette = "magma",
    legend_title = "PE"
  )

  expect_s3_class(
    p,
    "ggplot"
  )

})

# ------------------------------------------------------------------------------

test_that("plot_pe() rejects projects without PE", {

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
    plot_pe(pb),
    "has not been computed"
  )

})

# ------------------------------------------------------------------------------

test_that("plot_pe() accepts different palettes", {

  pb <- pb_test_project(stage = "pe")

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
    plot_pe(
      pb,
      palette = "viridis"
    ),
    "ggplot"
  )

  expect_s3_class(
    plot_pe(
      pb,
      palette = "plasma"
    ),
    "ggplot"
  )

  expect_s3_class(
    plot_pe(
      pb,
      palette = "magma"
    ),
    "ggplot"
  )

})

# ------------------------------------------------------------------------------

test_that("plot_pe() rejects unknown palette", {

  pb <- pb_test_project(stage = "pe")

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
    plot_pe(
      pb,
      palette = "foobar"
    ),
    "Unknown palette"
  )

})
