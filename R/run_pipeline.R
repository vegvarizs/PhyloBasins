# =============================================================================
# PhyloBasins
#
# Run complete analysis pipeline
# =============================================================================

#' Run the complete PhyloBasins pipeline
#'
#' Executes all preparation and metric-computation steps required to analyse a
#' community dataset.
#'
#' Already completed steps are skipped automatically unless
#' \code{overwrite = TRUE}.
#'
#' @param pb
#' A validated \code{pb_project}.
#'
#' @param overwrite
#' Logical. Recompute already completed steps?
#'
#' @param verbose
#' Logical. Print progress messages?
#'
#' @return
#' Updated \code{pb_project}.
#'
#' @export

run_pipeline <- function(
    pb,
    overwrite = FALSE,
    verbose = TRUE
) {

  validate_pb_project(pb)

  # ---------------------------------------------------------------------------
  # Tree preparation
  # ---------------------------------------------------------------------------

  if (!pb$tree$prepared || overwrite) {

    pb <- prepare_tree(
      pb,
      verbose = verbose
    )

  }

  # ---------------------------------------------------------------------------
  # Branch table
  # ---------------------------------------------------------------------------

  if (!pb$branches$prepared || overwrite) {

    pb <- build_branch_table(
      pb,
      overwrite = overwrite,
      verbose = verbose
    )

  }

  # ---------------------------------------------------------------------------
  # Site by Branch Matrix
  # ---------------------------------------------------------------------------

  if (!pb$site_branch_matrix$built || overwrite) {

    pb <- build_site_branch_matrix(
      pb,
      overwrite = overwrite,
      verbose = verbose
    )

  }

  # ---------------------------------------------------------------------------
  # Diversity metrics
  # ---------------------------------------------------------------------------

  pb <- compute_all_metrics(
    pb,
    overwrite = overwrite,
    verbose = verbose
  )

  if (verbose) {

    message(
      "PhyloBasins pipeline completed successfully."
    )

  }

  pb

}
