# =============================================================================
# detect_species_columns.R
#
# Detect species columns in a geometry attribute table.
# =============================================================================

#' Detect species columns
#'
#' Determines which columns of a geometry attribute table represent species.
#'
#' Species columns may be supplied explicitly or defined by the first and
#' last species column names.
#'
#' @param data A data.frame.
#' @param species_columns Optional character vector of species columns.
#' @param first_species Optional name of the first species column.
#' @param last_species Optional name of the last species column.
#'
#' @return Character vector of species column names.
#'
#' @keywords internal

detect_species_columns <- function(
    data,
    species_columns = NULL,
    first_species = NULL,
    last_species = NULL
) {

  stopifnot(is.data.frame(data))

  column_names <- names(data)

  # -------------------------------------------------------------------------
  # Explicit species columns
  # -------------------------------------------------------------------------

  if (!is.null(species_columns)) {

    return(as.character(species_columns))

  }

  # -------------------------------------------------------------------------
  # First / last species column
  # -------------------------------------------------------------------------

  if (!is.null(first_species) &&
      !is.null(last_species)) {

    if (!first_species %in% column_names) {

      stop(
        sprintf(
          "First species column '%s' not found.",
          first_species
        ),
        call. = FALSE
      )

    }

    if (!last_species %in% column_names) {

      stop(
        sprintf(
          "Last species column '%s' not found.",
          last_species
        ),
        call. = FALSE
      )

    }

    first_index <- match(first_species, column_names)
    last_index  <- match(last_species, column_names)

    if (first_index > last_index) {

      stop(
        "first_species occurs after last_species.",
        call. = FALSE
      )

    }

    return(column_names[first_index:last_index])

  }

  # -------------------------------------------------------------------------
  # Automatic detection (future implementation)
  # -------------------------------------------------------------------------

  stop(
    paste(
      "Species columns could not be determined.",
      "Supply either 'species_columns' or both",
      "'first_species' and 'last_species'."
    ),
    call. = FALSE
  )

}
