# =============================================================================
# Tests for pb_geometry
# =============================================================================

test_that("new_geometry() creates a valid geometry object", {

  x <- new_geometry()

  expect_s3_class(
    x,
    "pb_geometry"
  )

  expect_false(
    x$loaded
  )

  expect_null(
    x$sf
  )

  expect_true(
    is.list(x$validation)
  )

})

test_that("is.pb_geometry() detects geometry objects", {

  x <- new_geometry()

  expect_true(
    is.pb_geometry(x)
  )

  expect_false(
    is.pb_geometry(list())
  )

  expect_false(
    is.pb_geometry(NULL)
  )

})

test_that("new_geometry() stores supplied values", {

  geom <- sf::st_as_sf(

    data.frame(

      id = 1,

      wkt = "POINT (0 0)"

    ),

    wkt = "wkt",

    crs = 4326

  )

  x <- new_geometry(

    sf = geom,

    file = "dummy.gpkg",

    loaded = TRUE,

    validation = list(
      valid = TRUE
    ),

    metadata = list(
      version = "test"
    )

  )

  expect_true(
    x$loaded
  )

  expect_identical(
    x$file,
    "dummy.gpkg"
  )

  expect_s3_class(
    x$sf,
    "sf"
  )

  expect_true(
    x$validation$valid
  )

  expect_identical(
    x$metadata$version,
    "test"
  )

})

test_that("geometry cache contains expected elements", {

  x <- new_geometry()

  expect_named(

    x$cache,

    c(

      "id_index",

      "centroids",

      "neighbours",

      "spatial_index"

    )

  )

})

test_that("print.pb_geometry() returns invisibly", {

  x <- new_geometry()

  expect_invisible(

    print(x)

  )

})

test_that("loaded geometry prints correctly", {

  geom <- sf::st_as_sf(

    data.frame(

      id = 1,

      wkt = "POINT (0 0)"

    ),

    wkt = "wkt",

    crs = 4326

  )

  x <- new_geometry(

    sf = geom,

    loaded = TRUE

  )

  expect_invisible(

    print(x)

  )

})
