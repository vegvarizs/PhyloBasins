# =============================================================================
# PhyloBasins
#
# General utilities
#
# Small helper functions used throughout the package.
# =============================================================================

# -----------------------------------------------------------------------------
# Null-coalescing operator
# -----------------------------------------------------------------------------

#' Null-coalescing operator
#'
#' Returns the left-hand side unless it is NULL, otherwise returns the
#' right-hand side.
#'
#' @param x Left-hand side.
#' @param y Right-hand side.
#'
#' @return
#' Either \code{x} or \code{y}.
#'
#' @keywords internal
#'
#' @examples
#' NULL %||% 1
#' 5 %||% 1
`%||%` <- function(x, y) {

  if (is.null(x)) {
    y
  } else {
    x
  }

}


# -----------------------------------------------------------------------------
# Safe match
# -----------------------------------------------------------------------------

#' Safe match
#'
#' A wrapper around \code{match()} that always returns integer indices.
#'
#' @param x Values to match.
#' @param table Lookup table.
#'
#' @return
#' Integer vector.
#'
#' @keywords internal
safe_match <- function(x, table) {

  as.integer(match(x, table))

}


# -----------------------------------------------------------------------------
# Timestamp
# -----------------------------------------------------------------------------

#' Current timestamp
#'
#' Returns the current system time.
#'
#' Exists mainly to centralise timestamp creation.
#'
#' @return
#' POSIXct object.
#'
#' @keywords internal
timestamp <- function() {

  Sys.time()
}
