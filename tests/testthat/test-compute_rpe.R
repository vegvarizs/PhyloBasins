# =============================================================================
# compute_rpe()
# =============================================================================

test_that("compute_rpe computes RPE", {

  pb <- pb_test_project(
    stage = "branch_ranges"
  )

  pb <- compute_rpe(
    pb,
    verbose = FALSE
  )

  expect_true(
    pb$metrics$rpe$computed
  )

  expect_true(
    is.data.frame(
      pb$metrics$rpe$values
    )
  )

  expect_identical(

    names(
      pb$metrics$rpe$values
    ),

    c(
      "HYBAS_ID",
      "rpe"
    )

  )

  expect_equal(

    nrow(
      pb$metrics$rpe$values
    ),

    nrow(
      pb$community$matrix
    )

  )

})

test_that("RPE values are finite", {

  pb <- pb_test_project(
    stage = "rpe"
  )

  expect_false(

    anyNA(
      pb$metrics$rpe$values$rpe
    )

  )

  expect_false(

    any(
      is.nan(
        pb$metrics$rpe$values$rpe
      )
    )

  )

  expect_false(

    any(
      is.infinite(
        pb$metrics$rpe$values$rpe
      )
    )

  )

})

test_that("RPE site IDs equal site names", {

  pb <- pb_test_project(
    stage = "rpe"
  )

  expect_identical(

    pb$metrics$rpe$values$HYBAS_ID,

    rownames(
      pb$site_branch_matrix$matrix
    )

  )

})

test_that("compute_rpe respects overwrite = FALSE", {

  pb <- pb_test_project(
    stage = "rpe"
  )

  original <- pb$metrics$rpe$values

  pb <- compute_rpe(
    pb,
    overwrite = FALSE,
    verbose = FALSE
  )

  expect_identical(

    pb$metrics$rpe$values,

    original

  )

})

test_that("compute_rpe allows overwrite = TRUE", {

  pb <- pb_test_project(
    stage = "rpe"
  )

  expect_silent(

    pb <- compute_rpe(

      pb,

      overwrite = TRUE,

      verbose = FALSE

    )

  )

})

test_that("missing branch ranges throws error", {

  pb <- pb_test_project(
    stage = "site_branch_matrix"
  )

  expect_error(

    compute_rpe(pb),

    "Branch ranges"

  )

})

test_that("missing site-branch matrix throws error", {

  pb <- pb_test_project(
    stage = "community"
  )

  expect_error(

    compute_rpe(pb),

    "Site-branch matrix"

  )

})

test_that("verbose does not change result", {

  pb1 <- pb_test_project(
    stage = "branch_ranges"
  )

  pb1 <- compute_rpe(

    pb1,

    verbose = TRUE

  )

  pb2 <- pb_test_project(
    stage = "branch_ranges"
  )

  pb2 <- compute_rpe(

    pb2,

    verbose = FALSE

  )

  expect_identical(

    pb1$metrics$rpe$values,

    pb2$metrics$rpe$values

  )

})

test_that("RPE values are non-negative", {

  pb <- pb_test_project(
    stage = "rpe"
  )

  expect_true(

    all(
      pb$metrics$rpe$values$rpe >= 0
    )

  )

})
