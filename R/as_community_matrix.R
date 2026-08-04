# =============================================================================
# as_community_matrix.R
#
# Construct a community matrix from an sf object or data.frame.
# =============================================================================

#' Construct a community matrix
#'
#' Converts an \code{sf} object or \code{data.frame} into a community
#' presence/absence table suitable for \code{read_community()}.
#'
#' Geometry columns are removed automatically.
#'
#' @param x An \code{sf} object or a \code{data.frame}.
#' @param species Optional character vector of species columns.
#' @param id_col Optional identifier column to retain.
#' @param metadata Optional character vector of non-species columns.
#' @param logical Should values > 0 be converted to presence/absence?
#' @param verbose Logical.
#'
#' @return
#' A data.frame containing the identifier column (if requested) followed by
#' the community matrix.
#'
#' @export

as_community_matrix <- function(
    x,
    species = NULL,
    id_col = NULL,
    metadata = NULL,
    logical = TRUE,
    verbose = TRUE
) {

  ## -------------------------------------------------------------------------
  ## Input
  ## -------------------------------------------------------------------------

  if (!inherits(x, c("sf", "data.frame"))) {

    stop(
      "'x' must be an sf object or data.frame.",
      call. = FALSE
    )

  }

  if (inherits(x, "sf")) {

    x <- sf::st_drop_geometry(x)

  }

  ## -------------------------------------------------------------------------
  ## Identifier column
  ## -------------------------------------------------------------------------

  if (!is.null(id_col) && !id_col %in% names(x)) {

    stop(
      "Unknown identifier column: ",
      id_col,
      call. = FALSE
    )

  }

  ## -------------------------------------------------------------------------
  ## Automatic species detection
  ## -------------------------------------------------------------------------

  if (is.null(species)) {

    excluded <- unique(c(id_col, metadata))

    candidates <- setdiff(
      names(x),
      excluded
    )

    is_species <- vapply(

      candidates,

      function(nm) {

        z <- x[[nm]]

        if (!(is.numeric(z) ||
              is.integer(z) ||
              is.logical(z))) {

          return(FALSE)

        }

        vals <- unique(stats::na.omit(z))

        all(vals %in% c(0, 1))

      },

      logical(1)

    )

    species <- candidates[is_species]

    if (length(species) == 0) {

      stop(
        paste(
          "No species columns could be detected automatically.",
          "Please supply 'species='."
        ),
        call. = FALSE
      )

    }

    if (verbose) {

      message(
        "Detected ",
        length(species),
        " species columns."
      )

    }

  }

  ## -------------------------------------------------------------------------
  ## Checks
  ## -------------------------------------------------------------------------

  if (anyNA(species)) {

    stop(
      "'species' contains NA values.",
      call. = FALSE
    )

  }

  missing <- setdiff(
    species,
    names(x)
  )

  if (length(missing) > 0) {

    stop(
      "Unknown species columns: ",
      paste(missing, collapse = ", "),
      call. = FALSE
    )

  }

  if (anyDuplicated(species)) {

    stop(
      "Duplicated species names detected.",
      call. = FALSE
    )

  }

  ## -------------------------------------------------------------------------
  ## Extract community matrix
  ## -------------------------------------------------------------------------

  comm <- x[, species, drop = FALSE]

  if (logical) {

    comm[] <- lapply(

      comm,

      function(z) {

        as.integer(z > 0)

      }

    )

  }

  ## -------------------------------------------------------------------------
  ## Add identifier column
  ## -------------------------------------------------------------------------

  if (!is.null(id_col)) {

    comm <- cbind(

      x[id_col],

      comm,

      row.names = NULL

    )

  }

  rownames(comm) <- NULL

  comm

}
