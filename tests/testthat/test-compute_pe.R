# =============================================================================
# PhyloBasins
#
# Tests for compute_pe()
# =============================================================================

test_that("compute_pe exists", {

  expect_true(
    exists("compute_pe")
  )

})

test_that("compute_pe computes PE", {

  pb <- pb_test_project(
    stage = "branch_ranges"
  )

  pb <- compute_pe(
    pb,
    verbose = FALSE
  )

  expect_true(
    pb$metrics$pe$computed
  )

  expect_true(
    is.data.frame(
      pb$metrics$pe$values
    )
  )

  expect_identical(

    names(
      pb$metrics$pe$values
    ),

    c(
      "HYBAS_ID",
      "pe"
    )

  )

  expect_equal(

    nrow(
      pb$metrics$pe$values
    ),

    nrow(
      pb$community$matrix
    )

  )

  expect_false(

    anyNA(
      pb$metrics$pe$values$pe
    )

  )

  expect_true(

    all(
      pb$metrics$pe$values$pe >= 0
    )

  )

})

test_that("compute_pe is idempotent", {

  pb <- pb_test_project(
    stage = "branch_ranges"
  )

  pb <- compute_pe(
    pb,
    verbose = FALSE
  )

  pe1 <- pb$metrics$pe$values

  pb <- compute_pe(
    pb,
    verbose = FALSE
  )

  pe2 <- pb$metrics$pe$values

  expect_identical(
    pe1,
    pe2
  )

})

test_that("compute_pe overwrite works", {

  pb <- pb_test_project(
    stage = "branch_ranges"
  )

  pb <- compute_pe(
    pb,
    verbose = FALSE
  )

  pb$metrics$pe$values$pe[1] <- -999

  pb <- compute_pe(
    pb,
    overwrite = TRUE,
    verbose = FALSE
  )

  expect_true(

    all(
      pb$metrics$pe$values$pe >= 0
    )

  )

})
