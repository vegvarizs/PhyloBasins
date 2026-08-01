# =============================================================================
# Compute Faith's Phylogenetic Diversity
# =============================================================================

#' Compute Faith's Phylogenetic Diversity
#'
#' @param pb
#' A validated \code{pb_project}.
#'
#' @param overwrite
#' Logical. Recompute existing values?
#'
#' @param verbose
#' Logical. Print progress messages?
#'
#' @return
#' Updated \code{pb_project}.
#'
#' @export

compute_pd <- function(
    pb,
    overwrite = FALSE,
    verbose = TRUE
) {

  validate_pb_project(pb)

  if (!pb$branches$prepared) {
    stop(
      "Branch table has not been prepared.",
      call. = FALSE
    )
  }

  if (!pb$site_branch_matrix$built) {
    stop(
      "Site-branch matrix has not been built.",
      call. = FALSE
    )
  }

  if (isTRUE(pb$metrics$pd$computed) && !overwrite) {

    if (verbose)
      message("PD has already been computed.")

    return(pb)

  }

  branch_table <- pb$branches$table
  SBM <- pb$site_branch_matrix$matrix

  branch_lengths <- branch_table$length

  if (length(branch_lengths) != ncol(SBM)) {

    stop(
      "Branch table and site-branch matrix are inconsistent.",
      call. = FALSE
    )

  }

  pd <- as.numeric(SBM %*% branch_lengths)

  names(pd) <- pb$site_branch_matrix$sites

  pb$metrics$pd$values <- pd
  pb$metrics$pd$computed <- TRUE

  if (verbose) {

    message(

      sprintf(
        "Computed PD for %d sites.",
        length(pd)
      )

    )

  }

  pb$history <- rbind(

    pb$history,

    data.frame(

      timestamp = timestamp(),

      action = "pd_computed",

      stringsAsFactors = FALSE

    )

  )

  pb

}
