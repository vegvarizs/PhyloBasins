# =============================================================================
# Compute species richness
# =============================================================================

#' Compute species richness
#'
#' Computes species richness (number of taxa present) for every community.
#'
#' @param pb A PhyloBasins project.
#' @param overwrite Logical. Recompute richness if it already exists.
#' @param verbose Logical. Print progress messages.
#'
#' @return
#' Updated PhyloBasins project.
#'
#' @export

compute_richness <- function(
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

  if (!overwrite &&
      !is.null(pb$metrics$richness)) {

    if (verbose) {

      message(
        "Species richness already computed."
      )

    }

    return(pb)

  }

  # -------------------------------------------------------------------------
  # Compute richness
  # -------------------------------------------------------------------------

  if (verbose) {

    message(
      "Computing species richness..."
    )

  }

  richness <- rowSums(
    pb$community$matrix > 0,
    na.rm = TRUE
  )

  # -------------------------------------------------------------------------
  # Store results
  # -------------------------------------------------------------------------

  pb$metrics$richness <- list(

    values = data.frame(

      HYBAS_ID = pb$community$sites,

      richness = richness,

      stringsAsFactors = FALSE

    ),

    computed = TRUE

  )

  # -------------------------------------------------------------------------
  # Update history
  # -------------------------------------------------------------------------

  pb$history <- rbind(

    pb$history,

    data.frame(

      timestamp = Sys.time(),

      action = "richness_computed",

      stringsAsFactors = FALSE

    )

  )

  # -------------------------------------------------------------------------
  # Finish
  # -------------------------------------------------------------------------

  if (verbose) {

    message(
      sprintf(
        "Computed species richness for %d sites.",
        length(richness)
      )
    )

  }

  pb

}
