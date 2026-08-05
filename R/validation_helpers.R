# =============================================================================
# validation_helpers.R
#
# Internal validation helper functions.
# =============================================================================

# -----------------------------------------------------------------------------
# Check project class
# -----------------------------------------------------------------------------

check_pb_project <- function(pb) {

  if (!inherits(pb, "pb_project")) {

    stop(
      "Object must inherit from 'pb_project'.",
      call. = FALSE
    )

  }

  invisible(TRUE)

}

# -----------------------------------------------------------------------------
# Check component exists
# -----------------------------------------------------------------------------

check_component_exists <- function(pb, component) {

  if (is.null(pb[[component]])) {

    stop(

      sprintf(
        "Project component '%s' is missing.",
        component
      ),

      call. = FALSE

    )

  }

  invisible(TRUE)

}

# -----------------------------------------------------------------------------
# Check object class
# -----------------------------------------------------------------------------

check_component_class <- function(object,
                                  class_name,
                                  component = NULL) {

  if (!inherits(object, class_name)) {

    if (is.null(component)) {

      stop(

        sprintf(
          "Object must inherit from '%s'.",
          class_name
        ),

        call. = FALSE

      )

    } else {

      stop(

        sprintf(
          "Component '%s' must inherit from '%s'.",
          component,
          class_name
        ),

        call. = FALSE

      )

    }

  }

  invisible(TRUE)

}

# -----------------------------------------------------------------------------
# Check loaded
# -----------------------------------------------------------------------------

check_loaded <- function(object,
                         component = NULL) {

  if (is.null(object$loaded) || !isTRUE(object$loaded)) {

    if (is.null(component)) {

      stop(
        "Object has not been loaded.",
        call. = FALSE
      )

    }

    stop(

      sprintf(
        "Component '%s' has not been loaded.",
        component
      ),

      call. = FALSE

    )

  }

  invisible(TRUE)

}

# -----------------------------------------------------------------------------
# Check non-empty object
# -----------------------------------------------------------------------------

check_not_empty <- function(x,
                            name = deparse(substitute(x))) {

  if (length(x) == 0 || is.null(x)) {

    stop(

      sprintf(
        "'%s' is empty.",
        name
      ),

      call. = FALSE

    )

  }

  invisible(TRUE)

}

# -----------------------------------------------------------------------------
# Check matrix
# -----------------------------------------------------------------------------

check_matrix <- function(x,
                         name = deparse(substitute(x))) {

  if (!is.matrix(x)) {

    stop(

      sprintf(
        "'%s' must be a matrix.",
        name
      ),

      call. = FALSE

    )

  }

  invisible(TRUE)

}

# -----------------------------------------------------------------------------
# Check sparse matrix
# -----------------------------------------------------------------------------

check_sparse_matrix <- function(x,
                                name = deparse(substitute(x))) {

  if (!inherits(x, "Matrix")) {

    stop(

      sprintf(
        "'%s' must inherit from Matrix.",
        name
      ),

      call. = FALSE

    )

  }

  invisible(TRUE)

}

# -----------------------------------------------------------------------------
# Check sf object
# -----------------------------------------------------------------------------

check_sf <- function(x,
                     name = deparse(substitute(x))) {

  if (!inherits(x, "sf")) {

    stop(

      sprintf(
        "'%s' must be an sf object.",
        name
      ),

      call. = FALSE

    )

  }

  invisible(TRUE)

}

# -----------------------------------------------------------------------------
# Check CRS
# -----------------------------------------------------------------------------

check_crs <- function(sf_object) {

  if (is.na(sf::st_crs(sf_object))) {

    warning(

      "Geometry has no coordinate reference system.",

      call. = FALSE

    )

  }

  invisible(TRUE)

}

# -----------------------------------------------------------------------------
# Check feature count
# -----------------------------------------------------------------------------

check_features <- function(sf_object) {

  if (nrow(sf_object) == 0) {

    stop(

      "Geometry contains no features.",

      call. = FALSE

    )

  }

  invisible(TRUE)

}

# -----------------------------------------------------------------------------
# Check empty geometries
# -----------------------------------------------------------------------------

check_empty_geometries <- function(sf_object) {

  empty <- sf::st_is_empty(sf_object)

  if (any(empty)) {

    stop(

      sprintf(
        "%d empty geometries detected.",
        sum(empty)
      ),

      call. = FALSE

    )

  }

  invisible(TRUE)

}

# -----------------------------------------------------------------------------
# Check geometry validity
# -----------------------------------------------------------------------------

check_valid_geometries <- function(sf_object) {

  valid <- sf::st_is_valid(sf_object)

  if (any(!valid)) {

    warning(

      sprintf(
        "%d invalid geometries detected.",
        sum(!valid)
      ),

      call. = FALSE

    )

  }

  invisible(all(valid))

}
