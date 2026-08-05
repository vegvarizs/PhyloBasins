# =============================================================================
# build_geometry_index.R
#
# Build geometry cache for fast spatial lookup.
# =============================================================================

#' Build geometry index
#'
#' Build an internal lookup index for basin geometries.
#'
#' @param pb A pb_project object.
#' @param id_column Name of the unique feature identifier.
#' @param overwrite Rebuild an existing cache.
#' @param verbose Print progress messages.
#'
#' @return Updated pb_project.
#'
#' @export

build_geometry_index <- function(
    pb,
    id_column,
    overwrite = FALSE,
    verbose = TRUE
) {

  check_pb_project(pb)

  check_component_exists(pb, "geometry")

  check_component_class(
    pb$geometry,
    "pb_geometry",
    "geometry"
  )

  check_loaded(
    pb$geometry,
    "geometry"
  )

  geom <- pb$geometry$sf

  check_sf(geom)

  if (!id_column %in% names(geom)) {

    stop(
      sprintf(
        "Column '%s' was not found in the geometry.",
        id_column
      ),
      call. = FALSE
    )

  }

  ids <- geom[[id_column]]

  if (anyNA(ids)) {

    stop(
      sprintf(
        "Identifier column '%s' contains missing values.",
        id_column
      ),
      call. = FALSE
    )

  }

  if (anyDuplicated(ids)) {

    stop(
      sprintf(
        "Identifier column '%s' contains duplicated values.",
        id_column
      ),
      call. = FALSE
    )

  }

  if (!overwrite &&
      !is.null(pb$geometry$cache$id_index)) {

    if (verbose) {

      message(
        "Geometry index already exists."
      )

    }

    return(pb)

  }

  id_index <- stats::setNames(
    seq_len(nrow(geom)),
    as.character(ids)
  )

  pb$geometry$cache$id_column <- id_column

  pb$geometry$cache$id_index <- id_index

  pb$geometry$cache$n_features <- nrow(geom)

  pb$geometry$cache$bbox <- sf::st_bbox(geom)

  ## Lazy caches (built only when needed)

  pb$geometry$cache$centroids <- NULL

  pb$geometry$cache$neighbours <- NULL

  pb$geometry$cache$spatial_index <- NULL

  if (verbose) {

    message(
      "Geometry index created for ",
      nrow(geom),
      " features using '",
      id_column,
      "'."
    )

  }

  pb

}
