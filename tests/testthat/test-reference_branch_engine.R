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

  branches <- pb_branches()
  branches$prepared <- TRUE

  branches$table <- data.frame(
    branch_id = character(),
    stringsAsFactors = FALSE
  )

  branches$cache$descendant_species <- list()

  community <- pb_community(
    matrix = matrix(
      FALSE,
      nrow = 0,
      ncol = 0
    ),
    sites = character(),
    taxa = character(),
    loaded = TRUE
  )

  expect_error(
    reference_branch_engine(
      tree,
      branches,
      community
    ),
    "Tree must be prepared."
  )

})


test_that("branch table must be prepared", {

  tree <- new_tree(
    loaded = TRUE,
    prepared = TRUE
  )

  branches <- pb_branches()
  branches$prepared <- FALSE

  branches$table <- data.frame(
    branch_id = character(),
    stringsAsFactors = FALSE
  )

  branches$cache$descendant_species <- list()

  community <- pb_community(
    matrix = matrix(
      FALSE,
      nrow = 0,
      ncol = 0
    ),
    sites = character(),
    taxa = character(),
    loaded = TRUE
  )

  expect_error(
    reference_branch_engine(
      tree,
      branches,
      community
    ),
    "Branch table must be prepared."
  )

})


test_that("community must be prepared", {

  tree <- new_tree(
    loaded = TRUE,
    prepared = TRUE
  )

  branches <- pb_branches()
  branches$prepared <- TRUE

  branches$table <- data.frame(
    branch_id = character(),
    stringsAsFactors = FALSE
  )

  branches$cache$descendant_species <- list()

  community <- pb_community(
    matrix = matrix(
      FALSE,
      nrow = 0,
      ncol = 0
    ),
    sites = character(),
    taxa = character(),
    loaded = FALSE
  )

  expect_error(
    reference_branch_engine(
      tree,
      branches,
      community
    ),
    "Community must be prepared."
  )

})
