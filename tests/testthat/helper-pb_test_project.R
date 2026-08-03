# =============================================================================
# Helper functions for PhyloBasins unit tests
# =============================================================================

PB_TEST_STAGES <- c(
  "empty",
  "tree",
  "prepared_tree",
  "branches",
  "community",
  "site_branch_matrix",
  "branch_ranges",
  "pd",
  "pe",
  "rpe"
)

# -----------------------------------------------------------------------------
# Create a test project
# -----------------------------------------------------------------------------

pb_test_project <- function(
    stage = PB_TEST_STAGES
) {

  stage <- match.arg(stage)

  # ---------------------------------------------------------------------------
  # Locate example data
  # ---------------------------------------------------------------------------

  tree_file <- system.file(
    "extdata",
    "example_tree.nwk",
    package = "PhyloBasins"
  )

  community_file <- system.file(
    "extdata",
    "example_community.csv",
    package = "PhyloBasins"
  )

  if (tree_file == "") {
    stop("Cannot locate inst/extdata/example_tree.nwk.")
  }

  if (community_file == "") {
    stop("Cannot locate inst/extdata/example_community.csv.")
  }

  # ---------------------------------------------------------------------------
  # Empty project
  # ---------------------------------------------------------------------------

  pb <- pb_project()

  if (stage == "empty") {
    return(pb)
  }

  # ---------------------------------------------------------------------------
  # Tree
  # ---------------------------------------------------------------------------

  pb <- read_tree(
    pb,
    file = tree_file,
    verbose = FALSE
  )

  pb <- validate_tree_data(
    pb,
    verbose = FALSE
  )

  if (stage == "tree") {
    return(pb)
  }

  # ---------------------------------------------------------------------------
  # Prepared tree
  # ---------------------------------------------------------------------------

  pb <- prepare_tree(
    pb,
    verbose = FALSE
  )

  if (stage == "prepared_tree") {
    return(pb)
  }

  # ---------------------------------------------------------------------------
  # Branch table
  # ---------------------------------------------------------------------------

  pb <- build_branch_table(
    pb,
    verbose = FALSE
  )

  if (stage == "branches") {
    return(pb)
  }

  # ---------------------------------------------------------------------------
  # Community
  # ---------------------------------------------------------------------------

  pb <- read_community(
    pb,
    file = community_file,
    verbose = FALSE
  )

  if (stage == "community") {
    return(pb)
  }

  # ---------------------------------------------------------------------------
  # Site-branch matrix
  # ---------------------------------------------------------------------------

  pb <- build_site_branch_matrix(
    pb,
    verbose = FALSE
  )

  if (stage == "site_branch_matrix") {
    return(pb)
  }

  # ---------------------------------------------------------------------------
  # Branch ranges
  # ---------------------------------------------------------------------------

  pb <- compute_branch_ranges(
    pb,
    verbose = FALSE
  )

  if (stage == "branch_ranges") {
    return(pb)
  }

  # ---------------------------------------------------------------------------
  # PD
  # ---------------------------------------------------------------------------

  pb <- compute_pd(
    pb,
    verbose = FALSE
  )

  if (stage == "pd") {
    return(pb)
  }

  # ---------------------------------------------------------------------------
  # PE
  # ---------------------------------------------------------------------------

  pb <- compute_pe(
    pb,
    verbose = FALSE
  )

  if (stage == "pe") {
    return(pb)
  }

  # ---------------------------------------------------------------------------
  # RPE
  # ---------------------------------------------------------------------------

  pb <- compute_rpe(
    pb,
    verbose = FALSE
  )

  pb

}
