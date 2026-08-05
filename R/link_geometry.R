# =============================================================================
# link_geometry.R
#
# Link community sites to spatial geometry.
# =============================================================================

#' Link community and geometry
#'
#' Creates a one-to-one mapping between community sites and geometry
#' features using a common identifier.
#'
#' @param pb A pb_project object.
#' @param community_id Column (or row identifier) used by the community.
#' @param geometry_id Attribute column in the geometry.
#' @param overwrite Rebuild an existing mapping.
#' @param verbose Print progress messages.
#'
#' @return Updated pb_project.
#'
#' @export

link_geometry <- function(
    pb,
    community_id = NULL,
    geometry_id,
    overwrite = FALSE,
    verbose = TRUE
) {

  check_pb_project(pb)

  check_component_exists(pb, "community")
  check_component_exists(pb, "geometry")

  check_loaded(pb$community, "community")
  check_loaded(pb$geometry, "geometry")

  geom <- geometry_sf(pb)

  ## -------------------------------------------------------------------------
  ## Community identifiers
  ## -------------------------------------------------------------------------

  if (is.null(community_id)) {

    ids_comm <- rownames(pb$community$matrix)

    if (is.null(ids_comm)) {

      stop(
        "Community matrix has no row names.",
        call. = FALSE
      )

    }

  } else {

    if (!community_id %in% names(pb$community$metadata)) {

      stop(
        sprintf(
          "Community identifier '%s' is unavailable.",
          community_id
        ),
        call. = FALSE
      )

    }

    ids_comm <- pb$community$metadata[[community_id]]

  }

  ## -------------------------------------------------------------------------
  ## Geometry identifiers
  ## -------------------------------------------------------------------------

  if (!geometry_id %in% names(geom)) {

    stop(
      sprintf(
        "Geometry column '%s' not found.",
        geometry_id
      ),
      call. = FALSE
    )

  }

  ids_geom <- as.character(geom[[geometry_id]])

  ## -------------------------------------------------------------------------
  ## Matching
  ## -------------------------------------------------------------------------

  idx <- match(
    ids_comm,
    ids_geom
  )

  if (anyNA(idx)) {

    missing <- ids_comm[is.na(idx)]

    stop(
      sprintf(
        "%d community sites have no matching geometry.\nFirst missing IDs: %s",
        length(missing),
        paste(utils::head(missing, 10), collapse = ", ")
      ),
      call. = FALSE
    )

  }

  if (length(unique(idx)) != length(idx)) {

    stop(
      "Geometry identifiers are not unique.",
      call. = FALSE
    )

  }

  ## -------------------------------------------------------------------------
  ## Store mapping
  ## -------------------------------------------------------------------------

  if (is.null(pb$geometry$cache))
    pb$geometry$cache <- list()

  if (!overwrite &&
      !is.null(pb$geometry$cache$community_to_geometry)) {

    if (verbose)
      message("Geometry mapping already exists.")

    return(pb)

  }

  pb$geometry$cache$link <- list(

    community_id = community_id,

    geometry_id = geometry_id,

    community_to_geometry = idx,

    geometry_to_community = match(
      ids_geom,
      ids_comm
    )

  )

  if (verbose) {

    message(
      "Linked ",
      length(idx),
      " community sites to geometry."
    )

  }

  pb

}
