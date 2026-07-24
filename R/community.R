# =============================================================================
# PhyloBasins
#
# Community object
#
# Defines the community (site × species) data structure used throughout
# the package.
# =============================================================================

# -----------------------------------------------------------------------------
# Internal constructor
# -----------------------------------------------------------------------------

new_community <- function(

  matrix = NULL,
  sites = character(),
  species = character(),
  metadata = list(),
  loaded = FALSE,
  prepared = FALSE,
  validation = list(valid = FALSE),
  cache = list()

) {

  x <- list(

    matrix = matrix,

    sites = sites,

    species = species,

    metadata = metadata,

    loaded = loaded,

    prepared = prepared,

    validation = validation,

    cache = cache

  )

  class(x) <- "pb_community"

  validate_community(x)

}

# -----------------------------------------------------------------------------
# Validator
# -----------------------------------------------------------------------------

validate_community <- function(x) {

  if (!inherits(x, "pb_community")) {

    stop(
      "'x' must inherit from class 'pb_community'.",
      call. = FALSE
    )

  }

  required <- c(

    "matrix",
    "sites",
    "species",
    "metadata",
    "loaded",
    "prepared",
    "validation",
    "cache"

  )

  missing <- setdiff(required, names(x))

  if (length(missing) > 0) {

    stop(

      sprintf(
        "Missing community component(s): %s",
        paste(missing, collapse = ", ")
      ),

      call. = FALSE

    )

  }

  if (!is.null(x$matrix) &&
      !is.matrix(x$matrix)) {

    stop(
      "'matrix' must be a matrix or NULL.",
      call. = FALSE
    )

  }

  if (!is.character(x$sites)) {

    stop(
      "'sites' must be a character vector.",
      call. = FALSE
    )

  }

  if (!is.character(x$species)) {

    stop(
      "'species' must be a character vector.",
      call. = FALSE
    )

  }

  if (!is.list(x$metadata)) {

    stop(
      "'metadata' must be a list.",
      call. = FALSE
    )

  }

  if (!is.logical(x$loaded) ||
      length(x$loaded) != 1) {

    stop(
      "'loaded' must be TRUE or FALSE.",
      call. = FALSE
    )

  }

  if (!is.logical(x$prepared) ||
      length(x$prepared) != 1) {

    stop(
      "'prepared' must be TRUE or FALSE.",
      call. = FALSE
    )

  }

  if (!is.list(x$validation)) {

    stop(
      "'validation' must be a list.",
      call. = FALSE
    )

  }

  if (!is.list(x$cache)) {

    stop(
      "'cache' must be a list.",
      call. = FALSE
    )

  }

  invisible(x)

}

# -----------------------------------------------------------------------------
# Print method
# -----------------------------------------------------------------------------

#' @export
print.pb_community <- function(x, ...) {

  cat("\n")
  cat("PhyloBasins community object\n")
  cat("----------------------------\n")

  cat("Loaded:    ", x$loaded, "\n", sep = "")
  cat("Prepared:  ", x$prepared, "\n", sep = "")

  if (x$loaded && !is.null(x$matrix)) {

    cat("Sites:     ", nrow(x$matrix), "\n", sep = "")
    cat("Species:   ", ncol(x$matrix), "\n", sep = "")

  }

  invisible(x)

}

# -----------------------------------------------------------------------------
# Summary
# -----------------------------------------------------------------------------

#' @export
summary.pb_community <- function(object, ...) {

  print(object)

  invisible(object)

}
