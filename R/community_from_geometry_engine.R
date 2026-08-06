# =============================================================================
# community_from_geometry_engine.R
#
# Internal engine for building community matrices from geometry attributes.
# =============================================================================

#' Build a community matrix from geometry attributes
#'
#' Internal engine used by build_community_from_geometry().
#'
#' @param data A data.frame containing geometry attributes.
#' @param site_id Name of the site identifier column.
#' @param species_columns Character vector of species columns.
#'
#' @return A numeric presence/absence matrix with site identifiers as row names.
#'
#' @keywords internal

community_from_geometry_engine <- function(
    data,
    site_id,
    species_columns
) {

  stopifnot(is.data.frame(data))
  stopifnot(is.character(site_id))
  stopifnot(length(site_id) == 1)
  stopifnot(is.character(species_columns))

  community <-

    as.matrix(
      data[, species_columns, drop = FALSE]
    )

  storage.mode(community) <- "numeric"

  rownames(community) <-

    as.character(
      data[[site_id]]
    )

  colnames(community) <- species_columns

  # -------------------------------------------------------------------------
  # Convert logical values to 0/1
  # -------------------------------------------------------------------------

  if (is.logical(community)) {

    community <- community * 1

  }

  # -------------------------------------------------------------------------
  # Replace missing values with absence
  # -------------------------------------------------------------------------

  community[is.na(community)] <- 0

  # -------------------------------------------------------------------------
  # Force presence/absence coding
  # -------------------------------------------------------------------------

  community[community != 0] <- 1

  community

}
