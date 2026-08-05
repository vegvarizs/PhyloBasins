# =============================================================================
# prepare_geometry.R
#
# Prepare geometry for downstream analyses.
# =============================================================================

#' Prepare geometry
#'
#' Validate, index and link geometry for plotting and spatial analyses.
#'
#' @param pb A pb_project object.
#' @param geometry_id Identifier column in the geometry.
#' @param community_id Optional identifier stored in the community object.
#'   If NULL, row names of the community matrix are used.
#' @param overwrite Rebuild existing caches.
#' @param verbose Print progress messages.
#'
#' @return Updated pb_project.
#'
#' @export

prepare_geometry <- function(
    pb,
    geometry_id,
    community_id = NULL,
    overwrite = FALSE,
    verbose = TRUE
) {

  check_pb_project(pb)

  check_component_exists(pb, "geometry")
  check_component_exists(pb, "community")

  ## -------------------------------------------------------------------------
  ## Validate geometry
  ## -------------------------------------------------------------------------

  if (verbose)
    message("Validating geometry...")

  pb <- validate_geometry(
    pb,
    verbose = FALSE
  )

  ## -------------------------------------------------------------------------
  ## Build geometry index
  ## -------------------------------------------------------------------------

  needs_index <-
    overwrite ||
    is.null(pb$geometry$cache$id_index)

  if (needs_index) {

    if (verbose)
      message("Building geometry index...")

    pb <- build_geometry_index(
      pb,
      id_column = geometry_id,
      overwrite = overwrite,
      verbose = FALSE
    )

  }

  ## -------------------------------------------------------------------------
  ## Link geometry to community
  ## -------------------------------------------------------------------------

  needs_link <-
    overwrite ||
    is.null(pb$geometry$cache$link)

  if (needs_link) {

    if (verbose)
      message("Linking community and geometry...")

    pb <- link_geometry(
      pb,
      community_id = community_id,
      geometry_id = geometry_id,
      overwrite = overwrite,
      verbose = FALSE
    )

  }

  ## -------------------------------------------------------------------------
  ## Metadata
  ## -------------------------------------------------------------------------

  pb$geometry$metadata$prepared <- TRUE

  pb$geometry$metadata$prepared_time <- Sys.time()

  pb$geometry$metadata$geometry_id <- geometry_id

  pb$geometry$metadata$community_id <- community_id

  if (verbose) {

    message(
      "Geometry successfully prepared."
    )

  }

  pb

}
