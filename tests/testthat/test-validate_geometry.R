# =============================================================================
# Tests for validate_geometry()
# =============================================================================

test_that("validate_geometry() validates a correct geometry", {

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

  expect_true(
    pb$geometry$validation$valid
  )

  expect_equal(
    pb$geometry$validation$n_features,
    4
  )

  expect_true(
    pb$geometry$validation$has_crs
  )

  expect_true(
    pb$geometry$validation$all_valid
  )

})

# ------------------------------------------------------------------------------

test_that("validate_geometry() rejects missing geometry", {

  pb <- pb_project()

  expect_error(

    validate_geometry(

      pb,

      verbose = FALSE

    ),

    "Geometry has not been loaded"

  )

})

# ------------------------------------------------------------------------------

test_that("validate_geometry() rejects an empty geometry", {

  pb <- pb_project()

  geom <- sf::st_sf(

    SiteID = character(),

    geometry = sf::st_sfc(

      crs = 4326

    )

  )

  pb$geometry <- new_geometry(

    sf = geom,

    loaded = TRUE

  )

  expect_error(

    validate_geometry(

      pb,

      verbose = FALSE

    ),

    "contains no features"

  )

})

# ------------------------------------------------------------------------------

test_that("validate_geometry() rejects non-sf objects", {

  pb <- pb_project()

  pb$geometry <- new_geometry(

    sf = data.frame(x = 1),

    loaded = TRUE

  )

  expect_error(

    validate_geometry(

      pb,

      verbose = FALSE

    ),

    "must be an sf object"

  )

})

# ------------------------------------------------------------------------------

test_that("validate_geometry() detects missing CRS", {

  pb <- pb_project()

  geom <- sf::st_sf(

    SiteID = "S1",

    geometry = sf::st_sfc(

      sf::st_polygon(

        list(

          rbind(

            c(0,0),

            c(1,0),

            c(1,1),

            c(0,1),

            c(0,0)

          )

        )

      )

    )

  )

  pb$geometry <- new_geometry(

    sf = geom,

    loaded = TRUE

  )

  expect_warning(

    validate_geometry(

      pb,

      verbose = FALSE

    ),

    "coordinate reference system"

  )

})

# ------------------------------------------------------------------------------

test_that("validate_geometry() stores validation metadata", {

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

  expect_true(

    "checked" %in%

      names(

        pb$geometry$validation

      )

  )

  expect_s3_class(

    pb$geometry$validation$checked,

    "POSIXct"

  )

})
