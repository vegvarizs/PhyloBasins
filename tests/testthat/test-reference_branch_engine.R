# =============================================================================
# PhyloBasins
#
# Tests for reference_branch_engine()
#
# These tests verify that invalid inputs are rejected correctly.
# Algorithm correctness is tested in
# test-reference_branch_engine_small_tree.R.
# =============================================================================

test_that("tree must be prepared", {

  tree <- new_tree(
    loaded = TRUE,
    prepared = FALSE
  )

  branches <- new_branches()
  branches$prepared <- TRUE
  branches$table <- data.frame(
    branch_id = character(),
    descendant_species = I(list()),
    stringsAsFactors = FALSE
  )

  community <- new_community(
    matrix = matrix(logical(), nrow = 0, ncol = 0),
    loaded = TRUE,
    prepared = TRUE
  )

  expect_error(
    reference_branch_engine(
      tree,
      branches,
      community
    ),
    "prepared"
  )

})


test_that("branch table must be prepared", {

  tree <- new_tree(
    loaded = TRUE,
    prepared = TRUE
  )

  branches <- new_branches()
  branches$prepared <- FALSE
  branches$table <- data.frame(
    branch_id = character(),
    descendant_species = I(list()),
    stringsAsFactors = FALSE
  )

  community <- new_community(
    matrix = matrix(logical(), nrow = 0, ncol = 0),
    loaded = TRUE,
    prepared = TRUE
  )

  expect_error(
    reference_branch_engine(
      tree,
      branches,
      community
    ),
    "prepared"
  )

})


test_that("community must be prepared", {

  tree <- new_tree(
    loaded = TRUE,
    prepared = TRUE
  )

  branches <- new_branches()
  branches$prepared <- TRUE
  branches$table <- data.frame(
    branch_id = character(),
    descendant_species = I(list()),
    stringsAsFactors = FALSE
  )

  community <- new_community(
    matrix = matrix(logical(), nrow = 0, ncol = 0),
    loaded = TRUE,
    prepared = FALSE
  )

  expect_error(
    reference_branch_engine(
      tree,
      branches,
      community
    ),
    "prepared"
  )

})
