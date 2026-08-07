# =============================================================================
# Tests for compute_richness()
# =============================================================================

test_that("compute_richness computes species richness", {

  pb <- pb_test_project(
    stage = "community"
  )

  pb <- compute_richness(
    pb,
    verbose = FALSE
  )

  expect_true(
    !is.null(pb$metrics$richness)
  )

  expect_true(
    pb$metrics$richness$computed
  )

  expect_equal(
    nrow(pb$metrics$richness$values),
    nrow(pb$community$matrix)
  )

  expect_equal(
    pb$metrics$richness$values$richness,
    as.numeric(rowSums(pb$community$matrix > 0))
  )

 })

test_that("compute_richness is idempotent", {

  pb <- pb_test_project(
    stage = "community"
  )

  pb <- compute_richness(
    pb,
    verbose = FALSE
  )

  richness1 <- pb$metrics$richness$values

  pb <- compute_richness(
    pb,
    verbose = FALSE
  )

  richness2 <- pb$metrics$richness$values

  expect_identical(
    richness1,
    richness2
  )

})

test_that("compute_richness overwrite works", {

  pb <- pb_test_project(
    stage = "community"
  )

  pb <- compute_richness(
    pb,
    verbose = FALSE
  )

  pb$metrics$richness$values$richness[1] <- -999

  pb <- compute_richness(
    pb,
    overwrite = TRUE,
    verbose = FALSE
  )

  expect_equal(
    pb$metrics$richness$values$richness,
    as.numeric(rowSums(pb$community$matrix > 0))
  )

})

test_that("compute_richness requires loaded community", {

  pb <- pb_project()

  expect_error(

    compute_richness(
      pb,
      verbose = FALSE
    ),

    "Community has not been loaded."

  )

})
