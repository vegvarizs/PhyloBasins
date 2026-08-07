# =============================================================================
# compute_turnover()
# =============================================================================

test_that("compute_turnover computes turnover", {

  pb <- pb_test_project(
    stage = "community"
  )

  pb <- compute_turnover(
    pb,
    verbose = FALSE
  )

  expect_true(
    pb$metrics$turnover$computed
  )

  expect_true(
    is.data.frame(
      pb$metrics$turnover$values
    )
  )

  expect_identical(

    names(
      pb$metrics$turnover$values
    ),

    c(
      "HYBAS_ID",
      "turnover"
    )

  )

  expect_equal(

    nrow(
      pb$metrics$turnover$values
    ),

    nrow(
      pb$community$matrix
    )

  )

})

test_that("turnover values are finite", {

  pb <- pb_test_project(
    stage = "community"
  )

  pb <- compute_turnover(
    pb,
    verbose = FALSE
  )

  expect_false(

    anyNA(
      pb$metrics$turnover$values$turnover
    )

  )

  expect_false(

    any(
      is.nan(
        pb$metrics$turnover$values$turnover
      )
    )

  )

  expect_false(

    any(
      is.infinite(
        pb$metrics$turnover$values$turnover
      )
    )

  )

})

test_that("turnover values lie between 0 and 1", {

  pb <- pb_test_project(
    stage = "community"
  )

  pb <- compute_turnover(
    pb,
    verbose = FALSE
  )

  expect_true(

    all(
      pb$metrics$turnover$values$turnover >= 0
    )

  )

  expect_true(

    all(
      pb$metrics$turnover$values$turnover <= 1
    )

  )

})

test_that("turnover site IDs equal community site IDs", {

  pb <- pb_test_project(
    stage = "community"
  )

  pb <- compute_turnover(
    pb,
    verbose = FALSE
  )

  expect_identical(

    pb$metrics$turnover$values$HYBAS_ID,

    pb$community$sites

  )

})

test_that("compute_turnover is idempotent", {

  pb <- pb_test_project(
    stage = "community"
  )

  pb <- compute_turnover(
    pb,
    verbose = FALSE
  )

  turnover1 <- pb$metrics$turnover$values

  pb <- compute_turnover(
    pb,
    verbose = FALSE
  )

  turnover2 <- pb$metrics$turnover$values

  expect_identical(
    turnover1,
    turnover2
  )

})

test_that("compute_turnover overwrite works", {

  pb <- pb_test_project(
    stage = "community"
  )

  pb <- compute_turnover(
    pb,
    verbose = FALSE
  )

  pb$metrics$turnover$values$turnover[1] <- -999

  pb <- compute_turnover(
    pb,
    overwrite = TRUE,
    verbose = FALSE
  )

  expect_true(

    all(
      pb$metrics$turnover$values$turnover >= 0
    )

  )

})

test_that("compute_turnover requires loaded community", {

  pb <- pb_project()

  expect_error(

    compute_turnover(
      pb,
      verbose = FALSE
    ),

    "Community has not been loaded."

  )

})

test_that("verbose does not change result", {

  pb1 <- pb_test_project(
    stage = "community"
  )

  pb1 <- compute_turnover(
    pb1,
    verbose = TRUE
  )

  pb2 <- pb_test_project(
    stage = "community"
  )

  pb2 <- compute_turnover(
    pb2,
    verbose = FALSE
  )

  expect_identical(

    pb1$metrics$turnover$values,

    pb2$metrics$turnover$values

  )

})
