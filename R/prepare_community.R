# =============================================================================
# PhyloBasins
#
# Prepare community data
#
# Builds reusable lookup structures for downstream analyses.
# =============================================================================

#' Prepare community data
#'
#' Builds internal lookup tables used by downstream biodiversity metrics.
#'
#' @param pb
#' A validated \code{pb_project}.
#'
#' @return
#' Updated \code{pb_project}.
#'
#' @export
prepare_community <- function(pb) {

  validate_pb_project(pb)

  if (!pb$community$loaded)
    stop(
      "No community matrix has been loaded.",
      call. = FALSE
    )

  if (!isTRUE(pb$community$validation$valid))
    stop(
      "Community data have not passed validation.",
      call. = FALSE
    )

  comm <- pb$community$matrix

  n_sites <- nrow(comm)
  n_species <- ncol(comm)

  ## ------------------------------------------------------------
  ## species -> sites
  ## ------------------------------------------------------------

  species_sites <- vector("list", n_species)

  for (i in seq_len(n_species)) {

    species_sites[[i]] <- which(comm[, i] == 1)

  }

  names(species_sites) <- colnames(comm)

  ## ------------------------------------------------------------
  ## site -> species
  ## ------------------------------------------------------------

  site_species <- vector("list", n_sites)

  for (i in seq_len(n_sites)) {

    site_species[[i]] <- which(comm[i, ] == 1)

  }

  names(site_species) <- rownames(comm)

  ## ------------------------------------------------------------
  ## summary vectors
  ## ------------------------------------------------------------

  species_range_size <- colSums(comm)

  names(species_range_size) <- colnames(comm)

  site_richness <- rowSums(comm)

  names(site_richness) <- rownames(comm)

  ## ------------------------------------------------------------
  ## cache
  ## ------------------------------------------------------------

  pb$community$cache <- list(

    species_sites = species_sites,

    site_species = site_species,

    species_range_size = species_range_size,

    site_richness = site_richness

  )

  pb$community$prepared <- TRUE

  pb$history <- rbind(

    pb$history,

    data.frame(

      timestamp = timestamp(),

      action = "community_prepared",

      stringsAsFactors = FALSE

    )

  )

  pb

}
