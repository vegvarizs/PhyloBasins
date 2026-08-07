# =============================================================================
# run_pipeline()
# =============================================================================

test_that("run_pipeline completes full workflow", {

  pb <- pb_test_project(stage = "community")

  pb <- run_pipeline(
    pb,
    verbose = FALSE
  )

  expect_true(pb$tree$prepared)
  expect_true(pb$branches$prepared)
  expect_true(pb$site_branch_matrix$built)
  expect_true(pb$branch_ranges$computed)

  expect_true(pb$metrics$pd$computed)
  expect_true(pb$metrics$pe$computed)
  expect_true(pb$metrics$rpe$computed)

})

test_that("all metric vectors have correct length", {

  pb <- pb_test_project(stage = "community")

  pb <- run_pipeline(
    pb,
    verbose = FALSE
  )

  n_sites <- nrow(pb$site_branch_matrix$matrix)

  expect_true(
    is.data.frame(pb$metrics$pd$values)
  )

  expect_true(
    is.data.frame(pb$metrics$pe$values)
  )

  expect_true(
    is.data.frame(pb$metrics$rpe$values)
  )

  expect_identical(
    names(pb$metrics$pd$values),
    c("HYBAS_ID", "pd")
  )

  expect_identical(
    names(pb$metrics$pe$values),
    c("HYBAS_ID", "pe")
  )

  expect_identical(
    names(pb$metrics$rpe$values),
    c("HYBAS_ID", "rpe")
  )

  expect_equal(
    nrow(pb$metrics$pd$values),
    n_sites
  )

  expect_equal(
    nrow(pb$metrics$pe$values),
    n_sites
  )

  expect_equal(
    nrow(pb$metrics$rpe$values),
    n_sites
  )
})

test_that("run_pipeline is idempotent", {

  pb <- pb_test_project(stage = "community")

  pb <- run_pipeline(
    pb,
    verbose = FALSE
  )

  pd1  <- pb$metrics$pd$values
  pe1  <- pb$metrics$pe$values
  rpe1 <- pb$metrics$rpe$values

  pb <- run_pipeline(
    pb,
    overwrite = TRUE,
    verbose = FALSE
  )

  expect_equal(
    pd1,
    pb$metrics$pd$values
  )

  expect_equal(
    pe1,
    pb$metrics$pe$values
  )

  expect_equal(
    rpe1,
    pb$metrics$rpe$values
  )

})

test_that("missing community throws error", {

  pb <- pb_test_project(stage = "branches")

  expect_error(

    run_pipeline(pb),

    "community|Community"

  )

})

test_that("verbose does not change results", {

  pb1 <- pb_test_project(stage = "community")

  pb1 <- run_pipeline(
    pb1,
    verbose = TRUE
  )

  pb2 <- pb_test_project(stage = "community")

  pb2 <- run_pipeline(
    pb2,
    verbose = FALSE
  )

  expect_equal(
    pb1$metrics$pd$values,
    pb2$metrics$pd$values
  )

  expect_equal(
    pb1$metrics$pe$values,
    pb2$metrics$pe$values
  )

  expect_equal(
    pb1$metrics$rpe$values,
    pb2$metrics$rpe$values
  )

})
