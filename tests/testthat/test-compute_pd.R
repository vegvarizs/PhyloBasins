# =============================================================================
# PhyloBasins
#
# Regression test for compute_pd()
# =============================================================================

library(ape)
library(Matrix)

test_that("compute_pd reproduces known PD values", {

  # ---------------------------------------------------------------------------
  # Reference tree
  # ---------------------------------------------------------------------------

  phy <- read.tree(text = "(A,(B,(C,D)));")
  phy <- reorder.phylo(phy, "postorder")

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

    length = c(
      1,
      2,
      3,
      1,
      1,
      1,
      1
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

  branches <- new_branches()

  branches$table <- branch_table
  branches$prepared <- TRUE

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

    byrow = TRUE,
    nrow = 4

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

  community <- new_community(

    matrix = comm,
    loaded = TRUE,
    prepared = TRUE

  )

  # ---------------------------------------------------------------------------
  # Site × branch matrix
  # ---------------------------------------------------------------------------

  sb <- reference_branch_engine(

    tree,
    branches,
    community

  )

  # ---------------------------------------------------------------------------
  # Assemble project
  # ---------------------------------------------------------------------------

  pb <- pb_project()

  pb$tree <- tree
  pb$branches <- branches
  pb$community <- community
  pb$site_branch_matrix <- sb

  # initialise PD object

  pb$metrics$pd <- new_pd()

  # ---------------------------------------------------------------------------
  # Compute PD
  # ---------------------------------------------------------------------------

  str(pb$site_branch_matrix)

  str(pb$site_branch_matrix$matrix)

  pb <- compute_pd(pb)

  # ---------------------------------------------------------------------------
  # Checks
  # ---------------------------------------------------------------------------

  expect_true(
    pb$metrics$pd$computed
  )

  expect_equal(

    pb$metrics$pd$values,

    c(

      S1 = 2,
      S2 = 4,
      S3 = 8,
      S4 = 7

    )

  )

  expect_identical(

    names(pb$metrics$pd$values),

    c("S1","S2","S3","S4")

  )

})
