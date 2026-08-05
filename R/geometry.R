# =============================================================================
# geometry.R
#
# Geometry object for PhyloBasins
# =============================================================================

# -----------------------------------------------------------------------------
# Constructor
# -----------------------------------------------------------------------------

#' Create a geometry object
#'
#' Internal constructor for spatial geometry.
#'
#' @param sf An sf object or NULL.
#' @param file Source filename.
#' @param loaded Logical.
#' @param validation Validation information.
#' @param metadata Metadata.
#' @param cache Internal cache.
#'
#' @return A pb_geometry object.
#'
#' @keywords internal

new_geometry <- function(

  sf = NULL,

  file = NA_character_,

  loaded = FALSE,

  validation = list(
    valid = FALSE
  ),

  metadata = list(),

  cache = list(

    id_index = NULL,

    centroids = NULL,

    neighbours = NULL,

    spatial_index = NULL

  )

) {

  structure(

    list(

      sf = sf,

      file = file,

      loaded = loaded,

      validation = validation,

      metadata = metadata,

      cache = cache

    ),

    class = "pb_geometry"

  )

}

# -----------------------------------------------------------------------------
# Validator
# -----------------------------------------------------------------------------

#' Test for a geometry object
#'
#' @param x Object.
#'
#' @return Logical.
#'
#' @export

is.pb_geometry <- function(x) {

  inherits(
    x,
    "pb_geometry"
  )

}

# -----------------------------------------------------------------------------
# Print
# -----------------------------------------------------------------------------

#' @export

print.pb_geometry <- function(x, ...) {

  cat("<pb_geometry>\n\n")

  cat(
    "Loaded: ",
    x$loaded,
    "\n",
    sep = ""
  )

  if (!x$loaded || is.null(x$sf)) {

    return(invisible(x))

  }

  cat(
    "Features: ",
    nrow(x$sf),
    "\n",
    sep = ""
  )

  cat(
    "Attributes: ",
    ncol(x$sf),
    "\n",
    sep = ""
  )

  geom_type <- unique(

    as.character(

      sf::st_geometry_type(

        x$sf,

        by_geometry = FALSE

      )

    )

  )

  cat(
    "Geometry: ",
    paste(geom_type, collapse = ", "),
    "\n",
    sep = ""
  )

  crs <- sf::st_crs(x$sf)

  if (!is.na(crs$epsg)) {

    cat(
      "CRS: EPSG:",
      crs$epsg,
      "\n",
      sep = ""
    )

  } else if (!is.null(crs$input)) {

    cat(
      "CRS: ",
      crs$input,
      "\n",
      sep = ""
    )

  }

  bbox <- sf::st_bbox(x$sf)

  cat(
    "Extent:\n"
  )

  print(bbox)

  invisible(x)

}
