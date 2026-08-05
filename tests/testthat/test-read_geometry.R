# =============================================================================
# Tests for read_geometry()
# =============================================================================

test_that("read_geometry() imports geometry from an sf object", {

  pb <- pb_project()

  geom <- sf::st_sf(

    SiteID = c("S1", "S2"),

    geometry = sf::st_sfc(

      sf::st_polygon(list(rbind(
        c(0,0), c(1,0), c(1,1), c(0,1), c(0,0)
      ))),

      sf::st_polygon(list(rbind(
        c(1,0), c(2,0), c(2,1), c(1,1), c(1,0)
      ))),

      crs = 4326

    )

  )

  pb <- read_geometry(

    pb,

    data = geom,

    verbose = FALSE

  )

  expect_s3_class(
    pb$geometry,
    "pb_geometry"
  )

  expect_true(
    pb$geometry$loaded
  )

  expect_s3_class(
    pb$geometry$sf,
    "sf"
  )

  expect_equal(
    nrow(pb$geometry$sf),
    2
  )

})

# ------------------------------------------------------------------------------

test_that("read_geometry() imports geometry from file", {

  pb <- pb_project()

  file <- system.file(

    "extdata",

    "example_geometry.gpkg",

    package = "PhyloBasins"

  )

  skip_if_not(file.exists(file))

  pb <- read_geometry(

    pb,

    file = file,

    verbose = FALSE

  )

  expect_true(
    pb$geometry$loaded
  )

  expect_true(
    nrow(pb$geometry$sf) > 0
  )

})

# ------------------------------------------------------------------------------

test_that("read_geometry() stores source filename", {

  pb <- pb_project()

  file <- system.file(

    "extdata",

    "example_geometry.gpkg",

    package = "PhyloBasins"

  )

  skip_if_not(file.exists(file))

  pb <- read_geometry(

    pb,

    file = file,

    verbose = FALSE

  )

  expect_true(

    grepl(

      "example_geometry",

      basename(pb$geometry$file)

    )

  )

})

# ------------------------------------------------------------------------------

test_that("read_geometry() rejects missing input", {

  pb <- pb_project()

  expect_error(

    read_geometry(pb),

    "Either 'file' or 'data'"

  )

})

# ------------------------------------------------------------------------------

test_that("read_geometry() rejects simultaneous file and data", {

  pb <- pb_project()

  geom <- sf::st_sf(

    id = 1,

    geometry = sf::st_sfc(

      sf::st_polygon(list(rbind(
        c(0,0), c(1,0), c(1,1), c(0,1), c(0,0)
      ))),

      crs = 4326

    )

  )

  expect_error(

    read_geometry(

      pb,

      file = "dummy.gpkg",

      data = geom

    ),

    "Specify only one"

  )

})

# ------------------------------------------------------------------------------

test_that("read_geometry() rejects non-sf objects", {

  pb <- pb_project()

  expect_error(

    read_geometry(

      pb,

      data = data.frame(a = 1)

    ),

    "sf object"

  )

})
