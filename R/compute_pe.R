# =============================================================================
# PhyloBasins
#
# Compute Phylogenetic Endemism
# =============================================================================

#' Compute Phylogenetic Endemism
#'
#' Computes Faith's Phylogenetic Endemism (PE) for every site.
#'
#' @param pb
#' A \code{pb_project} object.
#'
#' @param overwrite
#' Logical. Overwrite an existing result?
#'
#' @param verbose
#' Logical. Print progress messages?
#'
#' @return
#' Updated \code{pb_project}.
#'
#' @export

compute_pe <- function(
    pb,
    overwrite = FALSE,
    verbose = TRUE
) {

  validate_pb_project(pb)

  if (!pb$site_branch_matrix$built)
    stop(
      "Site-branch matrix has not been built.",
      call. = FALSE
    )

  if (!pb$branch_ranges$computed)
    stop(
      "Branch ranges have not been computed.",
      call. = FALSE
    )

  if (isTRUE(pb$metrics$pe$computed) && !overwrite)
    stop(
      "PE has already been computed.",
      call. = FALSE
    )

  if (verbose)
    message("Computing phylogenetic endemism...")

  pe <- compute_pe_engine(

    site_branch_matrix =
      pb$site_branch_matrix$matrix,

    weighted_length =
      pb$branch_ranges$table$weighted_length

  )

  pb$metrics$pe$values <- pe
  pb$metrics$pe$computed <- TRUE

  pb$history <- rbind(

    pb$history,

    data.frame(

      timestamp = timestamp(),

      action = "pe_computed",

      stringsAsFactors = FALSE

    )

  )

  if (verbose)
    message("Done.")

  pb

}
