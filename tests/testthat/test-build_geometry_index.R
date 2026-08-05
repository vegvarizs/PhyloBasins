# =============================================================================
# Tests for build_geometry_index()
# =============================================================================

test_that("build_geometry_index() creates an identifier index", {

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

  pb <- build_geometry_index(

    pb,

    id_column = "SiteID",

    verbose = FALSE

  )

  expect_false(
    is.null(pb$geometry$cache$id_index)
  )

  expect_identical(

    names(pb$geometry$cache$id_index),

    c("S1", "S2", "S3", "S4")

  )

  expect_identical(

    unname(pb$geometry$cache$id_index),

    1:4

  )

})

# ------------------------------------------------------------------------------

test_that("build_geometry_index() stores cache metadata", {

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

  pb <- build_geometry_index(

    pb,

    id_column = "SiteID",

    verbose = FALSE

  )

  expect_identical(

    pb$geometry$cache$id_column,

    "SiteID"

  )

  expect_equal(

    pb$geometry$cache$n_features,

    4

  )

  expect_s3_class(

    pb$geometry$cache$bbox,

    "bbox"

  )

})

# ------------------------------------------------------------------------------

test_that("build_geometry_index() rejects missing identifier column", {

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

    build_geometry_index(

      pb,

      id_column = "foobar",

      verbose = FALSE

    ),

    "Column 'foobar'"

  )

})

# ------------------------------------------------------------------------------

test_that("build_geometry_index() is idempotent", {

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

  pb1 <- build_geometry_index(

    pb,

    id_column = "SiteID",

    verbose = FALSE

  )

  pb2 <- build_geometry_index(

    pb1,

    id_column = "SiteID",

    verbose = FALSE

  )

  expect_identical(

    pb1$geometry$cache$id_index,

    pb2$geometry$cache$id_index

  )

})

# ------------------------------------------------------------------------------

test_that("build_geometry_index() rebuilds cache when overwrite = TRUE", {

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

  pb <- build_geometry_index(

    pb,

    id_column = "SiteID",

    verbose = FALSE

  )

  pb <- build_geometry_index(

    pb,

    id_column = "SiteID",

    overwrite = TRUE,

    verbose = FALSE

  )

  expect_identical(

    names(pb$geometry$cache$id_index),

    c("S1", "S2", "S3", "S4")

  )

})
