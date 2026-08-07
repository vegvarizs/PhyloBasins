# =============================================================================
# Compute Relative Phylogenetic Endemism
# =============================================================================

#' Compute Relative Phylogenetic Endemism
#'
#' Computes Relative Phylogenetic Endemism (RPE) as the ratio between
#' observed PE and PE calculated on an equal-branch-length tree having the
#' same total tree length.
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

compute_rpe <- function(
    pb,
    overwrite = FALSE,
    verbose = TRUE
) {

  validate_pb_project(pb)

  # -------------------------------------------------------------------------
  # Input checks
  # -------------------------------------------------------------------------

  if (!isTRUE(pb$site_branch_matrix$built)) {

    stop(
      "Site-branch matrix has not been built.",
      call. = FALSE
    )

  }

  if (!isTRUE(pb$branch_ranges$computed)) {

    stop(
      "Branch ranges have not been computed.",
      call. = FALSE
    )

  }

  # -------------------------------------------------------------------------
  # Already computed
  # -------------------------------------------------------------------------

  if (isTRUE(pb$metrics$rpe$computed) && !overwrite) {

    if (verbose) {

      message(
        "RPE has already been computed."
      )

    }

    return(pb)

  }

  if (verbose) {

    message(
      "Computing relative phylogenetic endemism..."
    )

  }

  # -------------------------------------------------------------------------
  # Observed PE
  # -------------------------------------------------------------------------

  pe_observed <- compute_pe_engine(

    site_branch_matrix =
      pb$site_branch_matrix$matrix,

    weighted_length =
      pb$branch_ranges$table$weighted_length

  )

  # -------------------------------------------------------------------------
  # Equal-branch-length tree
  # -------------------------------------------------------------------------

  occurrence <- pb$branch_ranges$table$n_sites

  branch_length <- pb$branches$table$length

  total_tree_length <- sum(branch_length)

  equal_branch_length <-
    total_tree_length / length(branch_length)

  weighted_length_equal <-
    numeric(length(occurrence))

  positive <- occurrence > 0

  weighted_length_equal[positive] <-
    equal_branch_length / occurrence[positive]

  # -------------------------------------------------------------------------
  # Equal-tree PE
  # -------------------------------------------------------------------------

  pe_equal <- compute_pe_engine(

    site_branch_matrix =
      pb$site_branch_matrix$matrix,

    weighted_length =
      weighted_length_equal

  )

  # -------------------------------------------------------------------------
  # Relative PE
  # -------------------------------------------------------------------------

  rpe <- numeric(length(pe_observed))

  valid <- pe_equal > 0

  rpe[valid] <-
    pe_observed[valid] /
    pe_equal[valid]

  rpe_table <- data.frame(

    HYBAS_ID = pb$site_branch_matrix$sites,

    rpe = rpe,

    stringsAsFactors = FALSE

  )

  # -------------------------------------------------------------------------
  # Store results
  # -------------------------------------------------------------------------

  pb$metrics$rpe <- list(

    values = rpe_table,

    computed = TRUE

  )

  # -------------------------------------------------------------------------
  # Update history
  # -------------------------------------------------------------------------

  pb$history <- rbind(

    pb$history,

    data.frame(

      timestamp = timestamp(),

      action = "rpe_computed",

      stringsAsFactors = FALSE

    )

  )

  # -------------------------------------------------------------------------
  # Finish
  # -------------------------------------------------------------------------

  if (verbose) {

    message(

      sprintf(
        "Computed RPE for %d sites.",
        nrow(rpe_table)
      )

    )

  }

  pb

}
