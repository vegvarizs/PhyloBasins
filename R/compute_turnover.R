# =============================================================================
# Compute turnover
# =============================================================================

#' Compute community turnover
#'
#' Computes the mean Jaccard dissimilarity (community turnover) for every site.
#' Turnover is calculated as the mean binary Jaccard distance between each site
#' and all other sites.
#'
#' @param pb
#' A PhyloBasins project.
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

compute_turnover <- function(
    pb,
    overwrite = FALSE,
    verbose = TRUE
) {

  # -------------------------------------------------------------------------
  # Input checks
  # -------------------------------------------------------------------------

  if (!inherits(pb, "pb_project")) {

    stop(
      "Input must be a PhyloBasins project.",
      call. = FALSE
    )

  }

  if (is.null(pb$community)) {

    stop(
      "Community has not been loaded.",
      call. = FALSE
    )

  }

  if (!isTRUE(pb$community$loaded)) {

    stop(
      "Community has not been loaded.",
      call. = FALSE
    )

  }

  if (is.null(pb$community$matrix)) {

    stop(
      "Community matrix is missing.",
      call. = FALSE
    )

  }

  # -------------------------------------------------------------------------
  # Already computed
  # -------------------------------------------------------------------------

  if (isTRUE(pb$metrics$turnover$computed) &&
      !overwrite) {

    if (verbose) {

      message(
        "Community turnover already computed."
      )

    }

    return(pb)

  }

  # -------------------------------------------------------------------------
  # Compute turnover
  # -------------------------------------------------------------------------

  if (verbose) {

    message(
      "Computing community turnover..."
    )

  }

  comm <- pb$community$matrix > 0

  jac <- vegan::vegdist(

    comm,

    method = "jaccard",

    binary = TRUE

  )

  jac <- as.matrix(jac)

  diag(jac) <- NA_real_

  turnover <- rowMeans(

    jac,

    na.rm = TRUE

  )

  # -------------------------------------------------------------------------
  # Store results
  # -------------------------------------------------------------------------

  turnover_table <- data.frame(

    HYBAS_ID = pb$community$sites,

    turnover = turnover,

    stringsAsFactors = FALSE

  )

  pb$metrics$turnover <- list(

    values = turnover_table,

    computed = TRUE

  )

  # -------------------------------------------------------------------------
  # Update history
  # -------------------------------------------------------------------------

  pb$history <- rbind(

    pb$history,

    data.frame(

      timestamp = timestamp(),

      action = "turnover_computed",

      stringsAsFactors = FALSE

    )

  )

  # -------------------------------------------------------------------------
  # Finish
  # -------------------------------------------------------------------------

  if (verbose) {

    message(

      sprintf(

        "Computed turnover for %d sites.",

        nrow(turnover_table)

      )

    )

  }

  pb

}
