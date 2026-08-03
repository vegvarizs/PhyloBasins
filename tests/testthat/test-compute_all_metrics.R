# =============================================================================
# compute_all_metrics()
# =============================================================================

test_that("compute_all_metrics computes every metric", {

  pb <- pb_test_project(stage = "site_branch_matrix")

  pb <- compute_all_metrics(
    pb,
    verbose = FALSE
  )

  expect_true(pb$branch_ranges$computed)

  expect_true(pb$metrics$pd$computed)
  expect_true(pb$metrics$pe$computed)
  expect_true(pb$metrics$rpe$computed)

  expect_true(
    is.numeric(pb$metrics$pd$values)
  )

  expect_true(
    is.numeric(pb$metrics$pe$values)
  )

  expect_true(
    is.numeric(pb$metrics$rpe$values)
  )

})

test_that("all metric vectors have correct length", {

  pb <- pb_test_project(stage = "site_branch_matrix")

  pb <- compute_all_metrics(
    pb,
    verbose = FALSE
  )

  n_sites <- nrow(pb$site_branch_matrix$matrix)

  expect_length(
    pb$metrics$pd$values,
    n_sites
  )

  expect_length(
    pb$metrics$pe$values,
    n_sites
  )

  expect_length(
    pb$metrics$rpe$values,
    n_sites
  )

})

test_that("compute_all_metrics is idempotent", {

  pb <- pb_test_project(stage = "site_branch_matrix")

  pb <- compute_all_metrics(
    pb,
    verbose = FALSE
  )

  pd1  <- pb$metrics$pd$values
  pe1  <- pb$metrics$pe$values
  rpe1 <- pb$metrics$rpe$values

  pb <- compute_all_metrics(
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

test_that("missing site-branch matrix throws error", {

  pb <- pb_test_project(stage = "community")

  expect_error(

    compute_all_metrics(pb),

    "Site-branch matrix"

  )

})

test_that("verbose does not change results", {

  pb1 <- pb_test_project(stage = "site_branch_matrix")

  pb1 <- compute_all_metrics(
    pb1,
    verbose = TRUE
  )

  pb2 <- pb_test_project(stage = "site_branch_matrix")

  pb2 <- compute_all_metrics(
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
