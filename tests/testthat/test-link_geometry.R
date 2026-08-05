# =============================================================================
# Tests for link_geometry()
# =============================================================================

test_that("link_geometry() correctly links community and geometry", {

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

  pb <- validate_geometry(
    pb,
    verbose = FALSE
  )

  pb <- build_geometry_index(
    pb,
    id_column = "SiteID",
    verbose = FALSE
  )

  pb <- link_geometry(
    pb,
    geometry_id = "SiteID",
    verbose = FALSE
  )

  expect_false(
    is.null(pb$geometry$cache$link)
  )

  expect_identical(
    pb$geometry$cache$link$community_to_geometry,
    1:4
  )

  expect_identical(
    pb$geometry$cache$link$geometry_to_community,
    1:4
  )

})

# ------------------------------------------------------------------------------

test_that("link_geometry() stores identifier metadata", {

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

  pb <- validate_geometry(
    pb,
    verbose = FALSE
  )

  pb <- build_geometry_index(
    pb,
    id_column = "SiteID",
    verbose = FALSE
  )

  pb <- link_geometry(
    pb,
    geometry_id = "SiteID",
    verbose = FALSE
  )

  expect_identical(
    pb$geometry$cache$link$geometry_id,
    "SiteID"
  )

  expect_null(
    pb$geometry$cache$link$community_id
  )

})

# ------------------------------------------------------------------------------

test_that("link_geometry() fails if geometry has not been loaded", {

  pb <- pb_test_project(stage = "community")

  expect_error(

    link_geometry(
      pb,
      geometry_id = "SiteID"
    ),

    "geometry"

  )

})

# ------------------------------------------------------------------------------

test_that("link_geometry() fails if identifier column is missing", {

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

  pb <- validate_geometry(
    pb,
    verbose = FALSE
  )

  pb <- build_geometry_index(
    pb,
    id_column = "SiteID",
    verbose = FALSE
  )

  expect_error(

    link_geometry(
      pb,
      geometry_id = "foobar",
      verbose = FALSE
    ),

    "Geometry column 'foobar'"

  )

})

# ------------------------------------------------------------------------------

test_that("link_geometry() is idempotent", {

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

  pb <- validate_geometry(
    pb,
    verbose = FALSE
  )

  pb <- build_geometry_index(
    pb,
    id_column = "SiteID",
    verbose = FALSE
  )

  pb1 <- link_geometry(
    pb,
    geometry_id = "SiteID",
    verbose = FALSE
  )

  pb2 <- link_geometry(
    pb1,
    geometry_id = "SiteID",
    verbose = FALSE
  )

  expect_identical(
    pb1$geometry$cache$link,
    pb2$geometry$cache$link
  )

})

# ------------------------------------------------------------------------------

test_that("link_geometry() rebuilds mapping when overwrite = TRUE", {

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

  pb <- validate_geometry(
    pb,
    verbose = FALSE
  )

  pb <- build_geometry_index(
    pb,
    id_column = "SiteID",
    verbose = FALSE
  )

  pb <- link_geometry(
    pb,
    geometry_id = "SiteID",
    verbose = FALSE
  )

  pb <- link_geometry(
    pb,
    geometry_id = "SiteID",
    overwrite = TRUE,
    verbose = FALSE
  )

  expect_identical(
    pb$geometry$cache$link$community_to_geometry,
    1:4
  )

})
