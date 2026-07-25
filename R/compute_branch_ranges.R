# =============================================================================
# PhyloBasins
#
# Compute branch ranges
# =============================================================================

#' Compute branch ranges
#'
#' Calculates the geographic range of every branch as the number of sites
#' in which the branch is represented.
#'
#' Requires a prepared site × branch matrix.
#'
#' @param pb A PhyloBasins project.
#'
#' @return
#' Updated project with
#'
#' \code{pb$metrics$branch_ranges$values}
#'
#' containing one range value per branch.
#'
#' @export

compute_branch_ranges <- function(pb) {

  validate_pb_project(pb)

  validate_tree(pb$tree)
  validate_branches(pb$branches)
  validate_community(pb$community)
  validate_site_branch_matrix(pb$site_branch_matrix)

  if (!pb$branches$prepared) {
    cli::cli_abort(
      "Branch table has not been built. Run {.fn build_branch_table} first."
    )
  }

  if (is.null(pb$site_branch_matrix)) {
    cli::cli_abort(
      "Site-branch matrix has not been built. Run {.fn build_site_branch_matrix} first."
    )
  }

  SBM <- pb$site_branch_matrix$matrix

  ranges <- Matrix::colSums(SBM)

  names(ranges) <- pb$site_branch_matrix$branches

  pb$metrics$branch_ranges$values <- ranges
  pb$metrics$branch_ranges$computed <- TRUE

  pb$history <- c(
    pb$history,
    list(
      list(
        step = "compute_branch_ranges",
        timestamp = timestamp()
      )
    )
  )

  pb

}
