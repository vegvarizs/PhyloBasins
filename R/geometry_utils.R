# =============================================================================
# geometry_utils.R
#
# Utility functions for pb_geometry objects.
# =============================================================================

# -----------------------------------------------------------------------------
# Return geometry
# -----------------------------------------------------------------------------

geometry_sf <- function(pb) {

  check_pb_project(pb)
  check_component_exists(pb, "geometry")
  check_loaded(pb$geometry, "geometry")

  pb$geometry$sf

}

# -----------------------------------------------------------------------------
# Geometry identifiers
# -----------------------------------------------------------------------------

geometry_ids <- function(pb) {

  check_pb_project(pb)
  check_component_exists(pb, "geometry")

  idx <- pb$geometry$cache$id_index

  if (is.null(idx)) {

    stop(
      "Geometry index has not been built. Run build_geometry_index().",
      call. = FALSE
    )

  }

  names(idx)

}

# -----------------------------------------------------------------------------
# Geometry row lookup
# -----------------------------------------------------------------------------

geometry_row <- function(pb, ids) {

  check_pb_project(pb)
  check_component_exists(pb, "geometry")

  idx <- pb$geometry$cache$id_index

  if (is.null(idx)) {

    stop(
      "Geometry index has not been built. Run build_geometry_index().",
      call. = FALSE
    )

  }

  unname(idx[as.character(ids)])

}

# -----------------------------------------------------------------------------
# Geometry bounding box
# -----------------------------------------------------------------------------

geometry_bbox <- function(pb) {

  check_pb_project(pb)

  sf::st_bbox(
    geometry_sf(pb)
  )

}

# -----------------------------------------------------------------------------
# Coordinate reference system
# -----------------------------------------------------------------------------

geometry_crs <- function(pb) {

  check_pb_project(pb)

  sf::st_crs(
    geometry_sf(pb)
  )

}

# -----------------------------------------------------------------------------
# Number of features
# -----------------------------------------------------------------------------

geometry_n_features <- function(pb) {

  nrow(
    geometry_sf(pb)
  )

}

# -----------------------------------------------------------------------------
# Geometry type
# -----------------------------------------------------------------------------

geometry_type <- function(pb) {

  unique(

    as.character(

      sf::st_geometry_type(

        geometry_sf(pb),

        by_geometry = FALSE

      )

    )

  )

}

# -----------------------------------------------------------------------------
# Cached centroids
# -----------------------------------------------------------------------------

geometry_centroids <- function(pb) {

  check_pb_project(pb)

  cent <- pb$geometry$cache$centroids

  if (is.null(cent)) {

    stop(
      "Centroid cache has not been built.",
      call. = FALSE
    )

  }

  cent

}

# -----------------------------------------------------------------------------
# Cached neighbours
# -----------------------------------------------------------------------------

geometry_neighbours <- function(pb) {

  check_pb_project(pb)

  nb <- pb$geometry$cache$neighbours

  if (is.null(nb)) {

    stop(
      "Neighbour cache has not been built.",
      call. = FALSE
    )

  }

  nb

}

# -----------------------------------------------------------------------------
# Cached spatial index
# -----------------------------------------------------------------------------

geometry_spatial_index <- function(pb) {

  check_pb_project(pb)

  idx <- pb$geometry$cache$spatial_index

  if (is.null(idx)) {

    stop(
      "Spatial index has not been built.",
      call. = FALSE
    )

  }

  idx

}
