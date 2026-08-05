# =============================================================================
# read_geometry.R
#
# Read spatial geometry into a PhyloBasins project.
# =============================================================================

#' Read basin geometry
#'
#' Import spatial geometry into a PhyloBasins project.
#'
#' @param pb A pb_project object.
#' @param file Path to a vector file supported by sf::st_read().
#' @param data An sf object.
#' @param quiet Suppress sf messages.
#' @param verbose Print progress messages.
#'
#' @return Updated pb_project.
#'
#' @export

read_geometry <- function(
    pb,
    file = NULL,
    data = NULL,
    quiet = TRUE,
    verbose = TRUE
) {

  stopifnot(inherits(pb, "pb_project"))

  if (is.null(file) && is.null(data)) {

    stop(
      "Either 'file' or 'data' must be supplied.",
      call. = FALSE
    )

  }

  if (!is.null(file) && !is.null(data)) {

    stop(
      "Specify only one of 'file' or 'data'.",
      call. = FALSE
    )

  }

  geom <- if (!is.null(file)) {

    sf::st_read(
      file,
      quiet = quiet
    )

  } else {

    data

  }

  if (!inherits(geom, "sf")) {

    stop(
      "'data' must be an sf object.",
      call. = FALSE
    )

  }

  pb$geometry <- new_geometry(

    sf = geom,

    file = if (is.null(file)) {
      NA_character_
    } else {
      normalizePath(
        file,
        winslash = "/",
        mustWork = FALSE
      )
    },

    loaded = TRUE,

    validation = list(
      valid = TRUE
    ),

    metadata = list(

      n_features = nrow(geom),

      n_attributes = ncol(geom),

      geometry_type = unique(
        as.character(
          sf::st_geometry_type(
            geom,
            by_geometry = FALSE
          )
        )
      )

    )

  )

  if (verbose) {

    message(

      "Imported ",
      nrow(geom),
      " geometries."

    )

  }

  pb

}
