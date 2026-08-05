# =============================================================================
# Tests for join_metric_to_geometry()
# =============================================================================

test_that("join_metric_to_geometry() joins a single metric", {

  pb <- pb_test_project(stage = "pd")

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

  expect_equal(
    nrow(geom),
    4
  )

  expect_equal(
    sum(!is.na(geom$pd)),
    4
  )

})

# ------------------------------------------------------------------------------

test_that("join_metric_to_geometry() joins multiple metrics", {

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
    metrics = c(
      "pd",
      "pe",
      "rpe"
    )
  )

  expect_true(
    all(
      c("pd","pe","rpe") %in%
        names(geom)
    )
  )

})

# ------------------------------------------------------------------------------

test_that("join_metric_to_geometry() exports all metrics", {

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

  geom <- join_metric_to_geometry(pb)

  expect_true(
    all(
      names(pb$metrics) %in%
        names(geom)
    )
  )

})

# ------------------------------------------------------------------------------

test_that("join_metric_to_geometry() rejects unknown metrics", {

  pb <- pb_test_project(stage = "pd")

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

    join_metric_to_geometry(
      pb,
      metrics = "foobar"
    ),

    "Unknown metric"

  )

})

# ------------------------------------------------------------------------------

test_that("join_metric_to_geometry() updates project geometry", {

  pb <- pb_test_project(stage = "pd")

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

  pb <- join_metric_to_geometry(
    pb,
    metrics = "pd",
    copy_geometry = FALSE
  )

  expect_true(
    "pd" %in%
      names(pb$geometry$sf)
  )

})

# ------------------------------------------------------------------------------

test_that("join_metric_to_geometry() rejects existing columns", {

  pb <- pb_test_project(stage = "pd")

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

  pb <- join_metric_to_geometry(
    pb,
    metrics = "pd",
    copy_geometry = FALSE
  )

  expect_error(

    join_metric_to_geometry(
      pb,
      metrics = "pd",
      copy_geometry = FALSE
    ),

    "already exists"

  )

})
