# =============================================================================
# Tests for plot_geometry()
# =============================================================================

test_that("plot_geometry() returns a ggplot object", {

  pb <- pb_project()

  pb <- read_geometry(

    pb,

    file = system.file(

      "extdata",

      "example_geometry.geojson",

      package = "PhyloBasins"

    ),

    verbose = FALSE

  )

  pb <- validate_geometry(
    pb,
    verbose = FALSE
  )

  p <- plot_geometry(pb)

  expect_s3_class(
    p,
    "ggplot"
  )

})

# ------------------------------------------------------------------------------

test_that("plot_geometry() plots a numeric fill vector", {

  pb <- pb_project()

  pb <- read_geometry(

    pb,

    file = system.file(

      "extdata",

      "example_geometry.geojson",

      package = "PhyloBasins"

    ),

    verbose = FALSE

  )

  pb <- validate_geometry(
    pb,
    verbose = FALSE
  )

  fill <- c(10, 20, 30, 40)

  p <- plot_geometry(

    pb,

    fill = fill

  )

  expect_s3_class(
    p,
    "ggplot"
  )

})

# ------------------------------------------------------------------------------

test_that("plot_geometry() rejects incorrect fill length", {

  pb <- pb_project()

  pb <- read_geometry(

    pb,

    file = system.file(

      "extdata",

      "example_geometry.geojson",

      package = "PhyloBasins"

    ),

    verbose = FALSE

  )

  pb <- validate_geometry(
    pb,
    verbose = FALSE
  )

  expect_error(

    plot_geometry(

      pb,

      fill = 1:3

    ),

    "one value per geometry feature"

  )

})

# ------------------------------------------------------------------------------

test_that("plot_geometry() supports different palettes", {

  pb <- pb_project()

  pb <- read_geometry(

    pb,

    file = system.file(

      "extdata",

      "example_geometry.geojson",

      package = "PhyloBasins"

    ),

    verbose = FALSE

  )

  pb <- validate_geometry(
    pb,
    verbose = FALSE
  )

  fill <- 1:4

  expect_s3_class(

    plot_geometry(
      pb,
      fill = fill,
      palette = "viridis"
    ),

    "ggplot"

  )

  expect_s3_class(

    plot_geometry(
      pb,
      fill = fill,
      palette = "magma"
    ),

    "ggplot"

  )

  expect_s3_class(

    plot_geometry(
      pb,
      fill = fill,
      palette = "plasma"
    ),

    "ggplot"

  )

})

# ------------------------------------------------------------------------------

test_that("plot_geometry() rejects unknown palettes", {

  pb <- pb_project()

  pb <- read_geometry(

    pb,

    file = system.file(

      "extdata",

      "example_geometry.geojson",

      package = "PhyloBasins"

    ),

    verbose = FALSE

  )

  pb <- validate_geometry(
    pb,
    verbose = FALSE
  )

  expect_error(

    plot_geometry(

      pb,

      fill = 1:4,

      palette = "foobar"

    ),

    "Unknown palette"

  )

})

# ------------------------------------------------------------------------------

test_that("plot_geometry() works with missing values", {

  pb <- pb_project()

  pb <- read_geometry(

    pb,

    file = system.file(

      "extdata",

      "example_geometry.geojson",

      package = "PhyloBasins"

    ),

    verbose = FALSE

  )

  pb <- validate_geometry(
    pb,
    verbose = FALSE
  )

  fill <- c(1, NA, 3, 4)

  p <- plot_geometry(

    pb,

    fill = fill

  )

  expect_s3_class(
    p,
    "ggplot"
  )

})
