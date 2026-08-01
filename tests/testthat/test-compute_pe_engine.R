# =============================================================================
# Tests for compute_pe_engine()
# =============================================================================

test_that("compute_pe_engine computes PE correctly", {

  M <- matrix(
    c(
      1, 1, 0,
      0, 1, 1,
      1, 1, 1
    ),
    byrow = TRUE,
    nrow = 3
  )

  rownames(M) <- c("site1", "site2", "site3")

  weighted_length <- c(1, 2, 3)

  pe <- compute_pe_engine(
    site_branch_matrix = M,
    weighted_length = weighted_length
  )

  expect_equal(
    pe,
    c(
      site1 = 3,
      site2 = 5,
      site3 = 6
    )
  )

})

test_that("branch weights modify PE correctly", {

  M <- matrix(
    c(
      1, 1, 0,
      0, 1, 1,
      1, 1, 1
    ),
    byrow = TRUE,
    nrow = 3
  )

  weighted_length <- c(1, 2, 3)

  pe <- compute_pe_engine(
    site_branch_matrix = M,
    weighted_length = weighted_length,
    branch_weights = c(2, 1, 1)
  )

  expect_equal(
    pe,
    c(4, 5, 7)
  )

})

test_that("identity weights equal default result", {

  M <- matrix(
    c(
      1, 1,
      0, 1
    ),
    byrow = TRUE,
    nrow = 2
  )

  weighted_length <- c(4, 2)

  pe1 <- compute_pe_engine(
    M,
    weighted_length
  )

  pe2 <- compute_pe_engine(
    M,
    weighted_length,
    branch_weights = c(1, 1)
  )

  expect_equal(pe1, pe2)

})

test_that("incorrect weighted_length length throws error", {

  M <- matrix(
    1,
    nrow = 2,
    ncol = 3
  )

  expect_error(
    compute_pe_engine(
      M,
      weighted_length = c(1, 2)
    ),
    "weighted_length"
  )

})

test_that("incorrect branch_weights length throws error", {

  M <- matrix(
    1,
    nrow = 2,
    ncol = 3
  )

  expect_error(
    compute_pe_engine(
      M,
      weighted_length = c(1, 2, 3),
      branch_weights = c(1, 1)
    ),
    "branch_weights"
  )

})

test_that("sparse and dense matrices give identical PE", {

  skip_if_not_installed("Matrix")

  dense <- matrix(
    c(
      1, 0, 1,
      0, 1, 1,
      1, 1, 0
    ),
    byrow = TRUE,
    nrow = 3
  )

  sparse <- Matrix::Matrix(
    dense,
    sparse = TRUE
  )

  weighted_length <- c(2, 3, 4)

  pe_dense <- compute_pe_engine(
    dense,
    weighted_length
  )

  pe_sparse <- compute_pe_engine(
    sparse,
    weighted_length
  )

  expect_equal(
    pe_dense,
    pe_sparse
  )

})

test_that("zero weighted branches contribute nothing", {

  M <- matrix(
    c(
      1, 1, 1,
      0, 1, 1
    ),
    byrow = TRUE,
    nrow = 2
  )

  weighted_length <- c(2, 0, 5)

  pe <- compute_pe_engine(
    M,
    weighted_length
  )

  expect_equal(
    pe,
    c(7, 5)
  )

})

test_that("empty communities have zero PE", {

  M <- matrix(
    c(
      0, 0, 0,
      1, 0, 1
    ),
    byrow = TRUE,
    nrow = 2
  )

  weighted_length <- c(2, 3, 5)

  pe <- compute_pe_engine(
    M,
    weighted_length
  )

  expect_equal(
    pe,
    c(0, 7)
  )

})

test_that("result is a named numeric vector", {

  M <- matrix(
    c(
      1, 0,
      0, 1
    ),
    byrow = TRUE,
    nrow = 2
  )

  rownames(M) <- c("A", "B")

  pe <- compute_pe_engine(
    M,
    c(2, 3)
  )

  expect_true(is.numeric(pe))
  expect_false(is.matrix(pe))

  expect_named(pe, c("A", "B"))

})
