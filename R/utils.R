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
#' Returns `x` unless it is `NULL`, otherwise returns `y`.
#'
#' @param x Left-hand side.
#' @param y Right-hand side.
#'
#' @return
#' Either `x` or `y`.
#'
#' @name null-coalescing
#' @aliases %||%
#' @keywords internal
NULL

#' @rdname null-coalescing
#' @export
`%||%` <- function(x, y) {

  if (is.null(x)) y else x

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

# =============================================================================
# Global variable declarations for R CMD check
# =============================================================================

utils::globalVariables(".data")
