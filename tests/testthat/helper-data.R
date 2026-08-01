# =============================================================================
# Helper objects for unit tests
# =============================================================================

# -----------------------------------------------------------------------------
# Example tree
# -----------------------------------------------------------------------------

make_test_tree <- function() {

  list(

    prepared = TRUE,

    n_tips = 3L,

    n_nodes = 2L,

    total_nodes = 5L

  )

}

# -----------------------------------------------------------------------------
# Example branch table
# -----------------------------------------------------------------------------

make_test_branches <- function() {

  br <- pb_branches()

  br$prepared <- TRUE

  br$table <- data.frame(

    branch_id = c("b1", "b2", "b3"),

    length = c(1, 2, 3),

    stringsAsFactors = FALSE

  )

  br

}

# -----------------------------------------------------------------------------
# Example site × branch matrix
# -----------------------------------------------------------------------------

make_test_site_branch_matrix <- function() {

  sbm <- pb_site_branch_matrix()

  sbm$built <- TRUE

  sbm$matrix <- matrix(

    c(

      1, 1, 0,
      0, 1, 1,
      1, 1, 1

    ),

    byrow = TRUE,

    nrow = 3,

    dimnames = list(

      c("site1", "site2", "site3"),

      c("b1", "b2", "b3")

    )

  )

  sbm

}
# -----------------------------------------------------------------------------
# Minimal valid project
# -----------------------------------------------------------------------------

make_test_project <- function() {

  pb <- pb_project()

  pb$tree <- make_test_tree()

  pb$branches <- make_test_branches()

  pb$site_branch_matrix <- make_test_site_branch_matrix()

  pb$branch_ranges <- pb_branch_ranges()

  validate_pb_project(pb)

  pb

}
