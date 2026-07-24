# =============================================================================
# Build Site × Branch Matrix
# =============================================================================

#' Build the site × branch matrix
#'
#' Builds the sparse site × branch incidence matrix from a prepared
#' PhyloBasins project.
#'
#' The project must already contain
#'
#' * a prepared phylogenetic tree,
#' * a prepared branch table,
#' * a prepared community matrix.
#'
#' @param pb A PhyloBasins project.
#'
#' @return
#' Updated project containing
#' \code{pb$site_branch_matrix}.
#'
#' @export
build_site_branch_matrix <- function(pb) {

  validate_pb_project(pb)

  validate_tree(pb$tree)
  validate_branches(pb$branches)
  validate_community(pb$community)

  pb$site_branch_matrix <- reference_branch_engine(

    tree      = pb$tree,
    branches  = pb$branches,
    community = pb$community

  )

  pb

}
