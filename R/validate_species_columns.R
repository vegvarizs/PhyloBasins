# =============================================================================
# validate_species_columns.R
#
# Validate species columns used for community construction.
# =============================================================================

#' Validate species columns
#'
#' Checks that the selected species columns exist and contain valid
#' presence/absence data.
#'
#' @param data A data.frame.
#' @param species_columns Character vector of species column names.
#'
#' @return Invisibly returns TRUE.
#'
#' @keywords internal

validate_species_columns <- function(
    data,
    species_columns
) {

  stopifnot(is.data.frame(data))
  stopifnot(is.character(species_columns))

  # -------------------------------------------------------------------------
  # Column existence
  # -------------------------------------------------------------------------

  missing_columns <-

    setdiff(
      species_columns,
      names(data)
    )

  if (length(missing_columns) > 0) {

    stop(

      sprintf(

        "Unknown species columns: %s",

        paste(
          missing_columns,
          collapse = ", "
        )

      ),

      call. = FALSE

    )

  }

  # -------------------------------------------------------------------------
  # Duplicate column names
  # -------------------------------------------------------------------------

  duplicated_columns <-

    unique(
      species_columns[
        duplicated(species_columns)
      ]
    )

  if (length(duplicated_columns) > 0) {

    stop(

      sprintf(

        "Duplicated species columns: %s",

        paste(
          duplicated_columns,
          collapse = ", "
        )

      ),

      call. = FALSE

    )

  }

  # -------------------------------------------------------------------------
  # Validate contents
  # -------------------------------------------------------------------------

  for (column in species_columns) {

    values <- data[[column]]

    # Logical values are acceptable

    if (is.logical(values)) {

      next

    }

    # Numeric / integer values

    if (!is.numeric(values)) {

      stop(

        sprintf(

          "Species column '%s' is not numeric or logical.",

          column

        ),

        call. = FALSE

      )

    }

    unique_values <-

      sort(
        unique(
          values[
            !is.na(values)
          ]
        )
      )

    if (!all(unique_values %in% c(0, 1))) {

      stop(

        sprintf(

          paste(

            "Species column '%s' contains",

            "values other than 0/1."

          ),

          column

        ),

        call. = FALSE

      )

    }

  }

  invisible(TRUE)

}
