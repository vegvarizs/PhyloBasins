# =============================================================================
# build_community_from_geometry.R
#
# Build a community matrix directly from geometry attributes.
# =============================================================================

#' Build community from geometry
#'
#' Creates a presence/absence community matrix from the attribute table of
#' a geometry object stored in a \code{pb_project}. This function provides
#' an alternative workflow to \code{read_community()} and is intended for
#' polygon datasets where species occurrences are stored as binary columns.
#'
#' @param pb A \code{pb_project} object.
#' @param species_columns Character vector containing species column names.
#' If \code{NULL}, the columns are detected automatically.
#' @param first_species Optional name of the first species column.
#' @param last_species Optional name of the last species column.
#' @param site_id Name of the site identifier column.
#' @param verbose Logical.
#'
#' @return A modified \code{pb_project}.
#'
#' @export

build_community_from_geometry <- function(
    pb,
    species_columns = NULL,
    first_species = NULL,
    last_species = NULL,
    site_id = "HYBAS_ID",
    verbose = interactive()
) {

  check_pb_project(pb)

  check_component_exists(pb, "geometry")

  geometry <- pb$geometry

  if (is.null(geometry$data)) {

    stop(
      "Geometry attribute table is missing.",
      call. = FALSE
    )

  }

  attrs <- geometry$data

  if (!site_id %in% names(attrs)) {

    stop(
      sprintf(
        "Site identifier column '%s' not found.",
        site_id
      ),
      call. = FALSE
    )

  }

  if (is.null(species_columns)) {

    species_columns <-

      detect_species_columns(
        attrs,
        first_species = first_species,
        last_species = last_species
      )

  }

  validate_species_columns(
    attrs,
    species_columns
  )

  if (verbose) {

    message(
      "Building community matrix from ",
      length(species_columns),
      " species columns."
    )

  }

  community <-

    community_from_geometry_engine(

      data = attrs,

      site_id = site_id,

      species_columns = species_columns

    )

  pb$community <- new_community(

    matrix = community,

    metadata = list(

      source = "geometry",

      site_id = site_id,

      species_columns = species_columns

    )

  )

  if (verbose) {

    message(
      nrow(community),
      " communities created."
    )

  }

  pb

}
