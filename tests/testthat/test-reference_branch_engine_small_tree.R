# =============================================================================
# PhyloBasins
#
# Reference regression test for reference_branch_engine()
#
# This test verifies the complete branch-incidence algorithm using
# a manually constructed four-species tree with a known solution.
# =============================================================================

library(Matrix)

test_that("reference engine reproduces known small example", {

  # ---------------------------------------------------------------------------
  # Small reference tree
  #
  #            Root
  #           /    \
  #          A      N1
  #                /  \
  #               B    N2
  #                   /  \
  #                  C    D
  # ---------------------------------------------------------------------------

  phy <- ape::read.tree(
    text = "(A,(B,(C,D)));"
  )

  phy <- ape::reorder.phylo(phy, "postorder")

  tree <- new_tree(
    phy = phy,
    loaded = TRUE,
    prepared = TRUE
  )

  # ---------------------------------------------------------------------------
  # Branch table
  # ---------------------------------------------------------------------------

  branch_table <- data.frame(

    branch_id = c(
      "Root",
      "N1",
      "N2",
      "A",
      "B",
      "C",
      "D"
    ),

    stringsAsFactors = FALSE

  )

  branch_table$descendant_species <- list(

    c("A","B","C","D"),
    c("B","C","D"),
    c("C","D"),
    "A",
    "B",
    "C",
    "D"

  )

  branches <- pb_branches()

  branches$table <- branch_table
  branches$prepared <- TRUE

  # Current Branch Engine stores descendants in the cache
  branches$cache$descendant_species <-
    branch_table$descendant_species

  # ---------------------------------------------------------------------------
  # Community matrix
  # ---------------------------------------------------------------------------

  comm <- matrix(

    c(

      TRUE, FALSE, FALSE, FALSE,
      FALSE, TRUE, FALSE, FALSE,
      FALSE, TRUE, TRUE, FALSE,
      FALSE, FALSE, FALSE, TRUE

    ),

    nrow = 4,
    byrow = TRUE

  )

  rownames(comm) <- c(
    "S1",
    "S2",
    "S3",
    "S4"
  )

  colnames(comm) <- c(
    "A",
    "B",
    "C",
    "D"
  )

  community <- pb_community(

    matrix = comm,

    sites = rownames(comm),

    taxa = colnames(comm),

    loaded = TRUE

  )

  # ---------------------------------------------------------------------------
  # Build reference matrix
  # ---------------------------------------------------------------------------

  sb <- reference_branch_engine(

    tree,

    branches,

    community

  )

  # ---------------------------------------------------------------------------
  # Expected result
  # ---------------------------------------------------------------------------

  expected <- matrix(

    c(

      TRUE, FALSE, FALSE, TRUE,  FALSE, FALSE, FALSE,
      TRUE, TRUE,  FALSE, FALSE, TRUE,  FALSE, FALSE,
      TRUE, TRUE,  TRUE,  FALSE, TRUE,  TRUE,  FALSE,
      TRUE, TRUE,  TRUE,  FALSE, FALSE, FALSE, TRUE

    ),

    nrow = 4,
    byrow = TRUE

  )

  rownames(expected) <- rownames(comm)
  colnames(expected) <- branch_table$branch_id

  # ---------------------------------------------------------------------------
  # Structural checks
  # ---------------------------------------------------------------------------

  expect_s3_class(
    sb,
    "pb_site_branch_matrix"
  )

  expect_true(
    sb$built
  )

  expect_s4_class(
    sb$matrix,
    "lgCMatrix"
  )

  expect_equal(
    sb$sites,
    rownames(comm)
  )

  expect_equal(
    sb$branches,
    branch_table$branch_id
  )

  # ---------------------------------------------------------------------------
  # Exact regression test
  # ---------------------------------------------------------------------------

  expect_identical(

    as.matrix(sb$matrix),

    expected

  )

  # ---------------------------------------------------------------------------
  # Final validation
  # ---------------------------------------------------------------------------

  expect_invisible(

    validate_site_branch_matrix(sb)

  )

})
