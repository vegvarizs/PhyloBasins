# =============================================================================
# End-to-end geometry pipeline
# =============================================================================

test_that("Complete geometry pipeline works", {

  ## --------------------------------------------------------------------------
  ## Build test project
  ## --------------------------------------------------------------------------

  pb <- pb_test_project(stage = "rpe")

  ## --------------------------------------------------------------------------
  ## Read geometry
  ## --------------------------------------------------------------------------

  pb <- read_geometry(

    pb,

    file = system.file(

      "extdata",

      "example_geometry.geojson",

      package = "PhyloBasins"

    ),

    verbose = FALSE

  )

  ## --------------------------------------------------------------------------
  ## Prepare geometry
  ## --------------------------------------------------------------------------

  pb <- prepare_geometry(

    pb,

    geometry_id = "SiteID",

    verbose = FALSE

  )

  ## --------------------------------------------------------------------------
  ## Join metrics
  ## --------------------------------------------------------------------------

  geom <- join_metric_to_geometry(pb)

  ## --------------------------------------------------------------------------
  ## Checks
  ## --------------------------------------------------------------------------

  expect_s3_class(
    geom,
    "sf"
  )

  expect_equal(
    nrow(geom),
    4
  )

  expect_true(
    all(
      c("pd", "pe", "rpe") %in%
        names(geom)
    )
  )

  expect_false(
    anyNA(geom$pd)
  )

  expect_false(
    anyNA(geom$pe)
  )

  expect_false(
    anyNA(geom$rpe)
  )

})

# ------------------------------------------------------------------------------

test_that("Pipeline returns plottable geometry", {

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

  geom <- join_metric_to_geometry(
    pb,
    metrics = "pd"
  )

  expect_s3_class(
    geom,
    "sf"
  )

  expect_true(
    "pd" %in% names(geom)
  )

})

# ------------------------------------------------------------------------------

test_that("Pipeline can be executed repeatedly", {

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

  geom1 <- join_metric_to_geometry(pb)

  geom2 <- join_metric_to_geometry(pb)

  expect_identical(
    names(geom1),
    names(geom2)
  )

  expect_equal(
    nrow(geom1),
    nrow(geom2)
  )

})
