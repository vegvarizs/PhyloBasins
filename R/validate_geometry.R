# =============================================================================
# validate_geometry.R
#
# Validate geometry stored in a PhyloBasins project.
# =============================================================================

#' Validate project geometry
#'
#' Validate the geometry component of a PhyloBasins project.
#'
#' @param pb A pb_project object.
#' @param require_polygons Require polygon geometries.
#' @param verbose Print validation summary.
#'
#' @return Updated pb_project.
#'
#' @export

validate_geometry <- function(
    pb,
    require_polygons = TRUE,
    verbose = TRUE
) {

  stopifnot(inherits(pb, "pb_project"))

  if (is.null(pb$geometry)) {

    stop(
      "Geometry has not been loaded.",
      call. = FALSE
    )

  }

  if (!inherits(pb$geometry, "pb_geometry")) {

    stop(
      "Invalid geometry object.",
      call. = FALSE
    )

  }

  geom <- pb$geometry$sf

  if (is.null(geom)) {

    stop(
      "Geometry object is empty.",
      call. = FALSE
    )

  }

  if (!inherits(geom, "sf")) {

    stop(
      "Geometry must be an sf object.",
      call. = FALSE
    )

  }

  if (nrow(geom) == 0) {

    stop(
      "Geometry contains no features.",
      call. = FALSE
    )

  }

  if (is.na(sf::st_crs(geom))) {

    warning(
      "Geometry has no coordinate reference system.",
      call. = FALSE
    )

  }

  if (require_polygons) {

    types <- unique(
      as.character(
        sf::st_geometry_type(
          geom,
          by_geometry = FALSE
        )
      )
    )

    allowed <- c(
      "POLYGON",
      "MULTIPOLYGON"
    )

    if (!all(types %in% allowed)) {

      stop(
        paste(
          "Geometry must contain only polygon features.",
          "Found:",
          paste(types, collapse = ", ")
        ),
        call. = FALSE
      )

    }

  }

  empty_geom <- sf::st_is_empty(geom)

  if (any(empty_geom)) {

    stop(
      sprintf(
        "%d empty geometries detected.",
        sum(empty_geom)
      ),
      call. = FALSE
    )

  }

  valid_geom <- sf::st_is_valid(geom)

  if (any(!valid_geom)) {

    warning(
      sprintf(
        "%d invalid geometries detected.",
        sum(!valid_geom)
      ),
      call. = FALSE
    )

  }

  pb$geometry$validation <- list(

    valid = TRUE,

    n_features = nrow(geom),

    geometry_types = unique(
      as.character(
        sf::st_geometry_type(
          geom,
          by_geometry = FALSE
        )
      )
    ),

    has_crs = !is.na(sf::st_crs(geom)),

    all_valid = all(valid_geom),

    checked = Sys.time()

  )

  if (verbose) {

    message(
      "Geometry validation completed successfully."
    )

  }

  pb

}
