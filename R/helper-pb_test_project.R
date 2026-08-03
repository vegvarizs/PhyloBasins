# =============================================================================
# Helper functions for PhyloBasins unit tests
# =============================================================================

#' Create a test project
#'
#' Creates a minimal pb_project object and optionally executes the pipeline
#' up to a selected stage.
#'
#' @param stage
#' Character. One of:
#'
#'   "empty"
#'   "tree"
#'   "prepared_tree"
#'   "branches"
#'   "community"
#'   "site_branch_matrix"
#'   "branch_ranges"
#'   "pd"
#'   "pe"
#'   "rpe"
#'
#' @return
#' A valid pb_project object.
#'
#' @keywords internal

pb_test_project <- function(

  stage = c(
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

) {

  stage <- match.arg(stage)

  pb <- pb_project()

  if (stage == "empty")
    return(pb)

  ## ---------------------------------------------------------------------
  ## Read tree
  ## ---------------------------------------------------------------------

  pb <- read_tree(

    pb,

    file = system.file(
      "extdata",
      "example_tree.nwk",
      package = "PhyloBasins"
    ),

    verbose = FALSE

  )

  pb <- validate_tree_data(
    pb
  )


  if (stage == "tree")
    return(pb)


  ## ---------------------------------------------------------------------
  ## Prepare tree
  ## ---------------------------------------------------------------------

  pb <- prepare_tree(

    pb,

    verbose = FALSE

  )

  if (stage == "prepared_tree")
    return(pb)

  ## ---------------------------------------------------------------------
  ## Branches
  ## ---------------------------------------------------------------------

  pb <- build_branch_table(
    pb,
    verbose = FALSE
  )

  if (stage == "branches")
    return(pb)

  ## ---------------------------------------------------------------------
  ## Community
  ## ---------------------------------------------------------------------

  pb <- read_community(

    pb,

    file = system.file(
      "extdata",
      "example_community.csv",
      package = "PhyloBasins"
    ),

    verbose = FALSE

  )

  if (stage == "community")
    return(pb)

  ## ---------------------------------------------------------------------
  ## Site-branch matrix
  ## ---------------------------------------------------------------------

  pb <- build_site_branch_matrix(

    pb,

    verbose = FALSE

  )

  if (stage == "site_branch_matrix")
    return(pb)

  ## ---------------------------------------------------------------------
  ## Branch ranges
  ## ---------------------------------------------------------------------

  pb <- compute_branch_ranges(

    pb,

    verbose = FALSE

  )

  if (stage == "branch_ranges")
    return(pb)

  ## ---------------------------------------------------------------------
  ## PD
  ## ---------------------------------------------------------------------

  pb <- compute_pd(

    pb,

    verbose = FALSE

  )

  if (stage == "pd")
    return(pb)

  ## ---------------------------------------------------------------------
  ## PE
  ## ---------------------------------------------------------------------

  pb <- compute_pe(

    pb,

    verbose = FALSE

  )

  if (stage == "pe")
    return(pb)

  ## ---------------------------------------------------------------------
  ## RPE
  ## ---------------------------------------------------------------------

  pb <- compute_rpe(

    pb,

    verbose = FALSE

  )

  pb

}
