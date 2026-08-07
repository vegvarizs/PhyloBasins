# =============================================================================
# Compute Faith's Phylogenetic Diversity
# =============================================================================

#' Compute Faith's Phylogenetic Diversity
#'
#' Computes Faith's phylogenetic diversity (PD) for all communities.
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

  # -------------------------------------------------------------------------
  # Input checks
  # -------------------------------------------------------------------------

  if (!isTRUE(pb$branches$prepared)) {

    stop(
      "Branch table has not been prepared.",
      call. = FALSE
    )

  }

  if (!isTRUE(pb$site_branch_matrix$built)) {

    stop(
      "Site-branch matrix has not been built.",
      call. = FALSE
    )

  }

  # -------------------------------------------------------------------------
  # Already computed
  # -------------------------------------------------------------------------

  if (isTRUE(pb$metrics$pd$computed) && !overwrite) {

    if (verbose) {

      message(
        "PD has already been computed."
      )

    }

    return(pb)

  }

  # -------------------------------------------------------------------------
  # Compute PD
  # -------------------------------------------------------------------------

  branch_table <- pb$branches$table
  sbm <- pb$site_branch_matrix$matrix

  branch_lengths <- branch_table$length

  if (length(branch_lengths) != ncol(sbm)) {

    stop(
      "Branch table and site-branch matrix are inconsistent.",
      call. = FALSE
    )

  }

  pd <- as.numeric(
    sbm %*% branch_lengths
  )

  pd_table <- data.frame(

    HYBAS_ID = pb$site_branch_matrix$sites,

    pd = pd,

    stringsAsFactors = FALSE

  )

  # -------------------------------------------------------------------------
  # Store results
  # -------------------------------------------------------------------------

  pb$metrics$pd <- list(

    values = pd_table,

    computed = TRUE

  )

  # -------------------------------------------------------------------------
  # Update history
  # -------------------------------------------------------------------------

  pb$history <- rbind(

    pb$history,

    data.frame(

      timestamp = timestamp(),

      action = "pd_computed",

      stringsAsFactors = FALSE

    )

  )

  # -------------------------------------------------------------------------
  # Finish
  # -------------------------------------------------------------------------

  if (verbose) {

    message(

      sprintf(
        "Computed PD for %d sites.",
        nrow(pd_table)
      )

    )

  }

  pb

}
