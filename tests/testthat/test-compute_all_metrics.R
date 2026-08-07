# =============================================================================
# compute_all_metrics()
# =============================================================================

test_that("compute_all_metrics computes every metric", {

  pb <- pb_test_project(
    stage = "site_branch_matrix"
  )

  pb <- compute_all_metrics(
    pb,
    verbose = FALSE
  )

  expect_true(
    pb$branch_ranges$computed
  )

  expect_true(pb$metrics$pd$computed)
  expect_true(pb$metrics$pe$computed)
  expect_true(pb$metrics$rpe$computed)

  expect_true(
    is.data.frame(pb$metrics$pd$values)
  )

  expect_true(
    is.data.frame(pb$metrics$pe$values)
  )

  expect_true(
    is.data.frame(pb$metrics$rpe$values)
  )

})

test_that("all metric tables have correct dimensions", {

  pb <- pb_test_project(
    stage = "site_branch_matrix"
  )

  pb <- compute_all_metrics(
    pb,
    verbose = FALSE
  )

  n_sites <- nrow(
    pb$site_branch_matrix$matrix
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

})

test_that("compute_all_metrics is idempotent", {

  pb <- pb_test_project(
    stage = "site_branch_matrix"
  )

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

  expect_identical(
    pd1,
    pb$metrics$pd$values
  )

  expect_identical(
    pe1,
    pb$metrics$pe$values
  )

  expect_identical(
    rpe1,
    pb$metrics$rpe$values
  )

})

test_that("missing site-branch matrix throws error", {

  pb <- pb_test_project(
    stage = "community"
  )

  expect_error(

    compute_all_metrics(pb),

    "Site-branch matrix"

  )

})

test_that("verbose does not change results", {

  pb1 <- pb_test_project(
    stage = "site_branch_matrix"
  )

  pb1 <- compute_all_metrics(
    pb1,
    verbose = TRUE
  )

  pb2 <- pb_test_project(
    stage = "site_branch_matrix"
  )

  pb2 <- compute_all_metrics(
    pb2,
    verbose = FALSE
  )

  expect_identical(
    pb1$metrics$pd$values,
    pb2$metrics$pd$values
  )

  expect_identical(
    pb1$metrics$pe$values,
    pb2$metrics$pe$values
  )

  expect_identical(
    pb1$metrics$rpe$values,
    pb2$metrics$rpe$values
  )

})
