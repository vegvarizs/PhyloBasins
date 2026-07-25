# =============================================================================
# PhyloBasins
#
# Compute Faith's Phylogenetic Diversity (PD)
# =============================================================================

#' Compute Faith's Phylogenetic Diversity
#'
#' Calculates Faith's Phylogenetic Diversity (PD) for every site in the
#' community matrix.
#'
#' PD is defined as the sum of branch lengths represented by the species
#' occurring in each site.
#'
#' Requires a prepared project containing:
#'
#' * branch table
#' * site × branch matrix
#'
#' @param pb A PhyloBasins project.
#'
#' @return
#' Updated project with
#'
#' \code{pb$metrics$pd$values}
#'
#' containing one PD value per site.
#'
#' @export
#'

compute_pd <- function(pb) {

  validate_pb_project(pb)

  validate_tree(pb$tree)
  validate_branches(pb$branches)
  validate_community(pb$community)
  validate_site_branch_matrix(pb$site_branch_matrix)

  branch_table <- pb$branches$table
  SBM <- pb$site_branch_matrix$matrix

  branch_lengths <- branch_table$length

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

branch_table <- pb$branches$table
SBM <- pb$site_branch_matrix$matrix

  if (length(branch_lengths) != ncol(SBM)) {

    cli::cli_abort(
      "Branch table and site-branch matrix are inconsistent."
    )

  }

  pd <- as.numeric(SBM %*% branch_lengths)

  names(pd) <- pb$site_branch_matrix$sites

  pb$metrics$pd$values <- pd

  pb$metrics$pd$computed <- TRUE

  pb$history <- c(
    pb$history,
    list(
      list(
        step = "compute_pd",
        timestamp = timestamp()
      )
    )
  )

  pb

}
