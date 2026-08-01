# =============================================================================
# PhyloBasins
#
# Community object
#
# Defines the internal community matrix representation.
# =============================================================================

# -----------------------------------------------------------------------------
# Constructor
# -----------------------------------------------------------------------------

#' Create a community object
#'
#' Creates a \code{pb_community} object.
#'
#' @param matrix
#' Community matrix.
#'
#' @param sites
#' Character vector of site names.
#'
#' @param taxa
#' Character vector of taxon names.
#'
#' @param file
#' Source file.
#'
#' @param loaded
#' Logical. Has the community matrix been loaded?
#'
#' @param validation
#' Validation information.
#'
#' @param metadata
#' Metadata associated with the community.
#'
#' @param cache
#' Internal cache.
#'
#' @return
#' A \code{pb_community} object.
#'
#' @export
pb_community <- function(

  matrix = NULL,

  sites = character(),

  taxa = character(),

  file = NA_character_,

  loaded = FALSE,

  validation = list(
    valid = FALSE
  ),

  metadata = list(),

  cache = list(

    taxa_index = NULL,

    site_index = NULL

  )

) {

  x <- list(

    matrix = matrix,

    sites = sites,

    taxa = taxa,

    file = file,

    loaded = loaded,

    validation = validation,

    metadata = metadata,

    cache = cache

  )

  class(x) <- "pb_community"

  validate_community(x)

  x

}

# -----------------------------------------------------------------------------
# Validator
# -----------------------------------------------------------------------------

validate_community <- function(x) {

  if (!inherits(x, "pb_community")) {

    stop(
      "Object is not a pb_community.",
      call. = FALSE
    )

  }

  required <- c(

    "matrix",

    "sites",

    "taxa",

    "file",

    "loaded",

    "validation",

    "metadata",

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

  if (!is.logical(x$loaded) || length(x$loaded) != 1) {

    stop(
      "'loaded' must be a single logical value.",
      call. = FALSE
    )

  }

  if (!is.character(x$sites)) {

    stop(
      "'sites' must be a character vector.",
      call. = FALSE
    )

  }

  if (!is.character(x$taxa)) {

    stop(
      "'taxa' must be a character vector.",
      call. = FALSE
    )

  }

  if (!is.character(x$file) || length(x$file) != 1) {

    stop(
      "'file' must be a character string.",
      call. = FALSE
    )

  }

  if (!is.list(x$validation)) {

    stop(
      "'validation' must be a list.",
      call. = FALSE
    )

  }

  if (!is.list(x$metadata)) {

    stop(
      "'metadata' must be a list.",
      call. = FALSE
    )

  }

  if (!is.list(x$cache)) {

    stop(
      "'cache' must be a list.",
      call. = FALSE
    )

  }

  required_cache <- c(

    "taxa_index",

    "site_index"

  )

  missing_cache <- setdiff(required_cache, names(x$cache))

  if (length(missing_cache) > 0) {

    stop(

      sprintf(
        "Missing cache component(s): %s",
        paste(missing_cache, collapse = ", ")
      ),

      call. = FALSE

    )

  }

  invisible(x)

}

# -----------------------------------------------------------------------------
# Print
# -----------------------------------------------------------------------------

#' @export
print.pb_community <- function(x, ...) {

  validate_community(x)

  cat("\n")
  cat("PhyloBasins community\n")
  cat("---------------------\n")

  cat("Loaded: ", x$loaded, "\n", sep = "")

  if (is.null(x$matrix)) {

    cat("Community matrix: <not available>\n")

  } else {

    cat("Sites: ", length(x$sites), "\n", sep = "")
    cat("Taxa:  ", length(x$taxa), "\n", sep = "")

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

# -----------------------------------------------------------------------------
# Predicate
# -----------------------------------------------------------------------------

#' Test whether an object is a pb_community
#'
#' @param x
#' An object.
#'
#' @return
#' Logical.
#'
#' @export
is.pb_community <- function(x) {

  inherits(x, "pb_community")

}

# -----------------------------------------------------------------------------
# Backward compatibility
# -----------------------------------------------------------------------------

new_community <- pb_community
