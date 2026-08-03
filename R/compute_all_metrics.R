# =============================================================================
# PhyloBasins
#
# Compute all implemented diversity metrics
# =============================================================================

#' Compute all phylogenetic diversity metrics
#'
#' Computes all currently implemented diversity metrics in the correct order.
#'
#' The function automatically skips metrics that have already been computed,
#' unless \code{overwrite = TRUE}.
#'
#' @param pb
#' A validated \code{pb_project}.
#'
#' @param overwrite
#' Logical. Recompute existing metrics?
#'
#' @param verbose
#' Logical. Print progress messages?
#'
#' @return
#' Updated \code{pb_project}.
#'
#' @export

compute_all_metrics <- function(
    pb,
    overwrite = FALSE,
    verbose = TRUE
) {

  validate_pb_project(pb)

  if (!pb$branch_ranges$computed || overwrite) {

    pb <- compute_branch_ranges(
      pb,
      overwrite = overwrite,
      verbose = verbose
    )

  }

  if (!isTRUE(pb$metrics$pd$computed) || overwrite) {

    pb <- compute_pd(
      pb,
      overwrite = overwrite,
      verbose = verbose
    )

  }

  if (!isTRUE(pb$metrics$pe$computed) || overwrite) {

    pb <- compute_pe(
      pb,
      overwrite = overwrite,
      verbose = verbose
    )

  }

  if (!isTRUE(pb$metrics$rpe$computed) || overwrite) {

    pb <- compute_rpe(
      pb,
      overwrite = overwrite,
      verbose = verbose
    )

  }

  if (verbose) {
    message("All implemented metrics have been computed.")
  }

  pb

}


