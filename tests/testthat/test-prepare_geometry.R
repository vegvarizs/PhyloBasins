# =============================================================================
# Tests for prepare_geometry()
# =============================================================================

test_that("prepare_geometry() builds a complete geometry pipeline", {

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

  expect_true(
    pb$geometry$loaded
  )

  expect_true(
    pb$geometry$validation$valid
  )

  expect_false(
    is.null(pb$geometry$cache$id_index)
  )

  expect_false(
    is.null(pb$geometry$cache$link)
  )

  expect_true(
    isTRUE(pb$geometry$metadata$prepared)
  )

})

# ------------------------------------------------------------------------------

test_that("prepare_geometry() is idempotent", {

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

  pb1 <- prepare_geometry(

    pb,

    geometry_id = "SiteID",

    verbose = FALSE

  )

  pb2 <- prepare_geometry(

    pb1,

    geometry_id = "SiteID",

    verbose = FALSE

  )

  expect_identical(

    pb1$geometry$cache$id_index,

    pb2$geometry$cache$id_index

  )

  expect_identical(

    pb1$geometry$cache$link,

    pb2$geometry$cache$link

  )

})

# ------------------------------------------------------------------------------

test_that("prepare_geometry() rebuilds caches when overwrite = TRUE", {

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

  pb <- prepare_geometry(

    pb,

    geometry_id = "SiteID",

    overwrite = TRUE,

    verbose = FALSE

  )

  expect_true(
    pb$geometry$validation$valid
  )

  expect_true(
    isTRUE(pb$geometry$metadata$prepared)
  )

})

# ------------------------------------------------------------------------------

test_that("prepare_geometry() fails if geometry is missing", {

  pb <- pb_test_project(stage = "community")

  expect_error(
    prepare_geometry(pb, geometry_id = "SiteID"),
    "geometry"
  )

})
