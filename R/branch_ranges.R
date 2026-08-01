# =============================================================================
# PhyloBasins
#
# Branch ranges object
#
# Stores geographic range information for every branch.
# =============================================================================

# -----------------------------------------------------------------------------
# Constructor
# -----------------------------------------------------------------------------

#' Create a branch ranges object
#'
#' Creates a \code{pb_branch_ranges} object.
#'
#' @param table
#' Data frame containing branch range statistics.
#'
#' @param computed
#' Logical indicating whether branch ranges have been computed.
#'
#' @param metadata
#' Optional metadata associated with the computation.
#'
#' @param cache
#' Internal cache used by downstream analyses.
#'
#' @return
#' A \code{pb_branch_ranges} object.
#'
#' @export

pb_branch_ranges <- function(
    table = NULL,
    computed = FALSE,
    metadata = list(),
    cache = list()
) {

  x <- list(

    table = table,

    computed = computed,

    metadata = metadata,

    cache = list(

      inverse_range = NULL,

      weighted_length = NULL

    )

  )

  class(x) <- "pb_branch_ranges"

  validate_branch_ranges(x)

  x

}

# -----------------------------------------------------------------------------
# Validator
# -----------------------------------------------------------------------------

validate_branch_ranges <- function(x) {

  if (!inherits(x, "pb_branch_ranges")) {

    stop(
      "Object is not a pb_branch_ranges.",
      call. = FALSE
    )

  }

  required <- c(

    "table",
    "computed",
    "metadata",
    "cache"

  )

  missing <- setdiff(required, names(x))

  if (length(missing) > 0) {

    stop(

      sprintf(
        "Missing branch range component(s): %s",
        paste(missing, collapse = ", ")
      ),

      call. = FALSE

    )

  }

  if (!is.logical(x$computed) || length(x$computed) != 1) {

    stop(
      "'computed' must be a single logical value.",
      call. = FALSE
    )

  }

  if (!is.null(x$table) && !is.data.frame(x$table)) {

    stop(
      "'table' must be NULL or a data.frame.",
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

  invisible(x)

}

# -----------------------------------------------------------------------------
# Print
# -----------------------------------------------------------------------------

#' @export

print.pb_branch_ranges <- function(x, ...) {

  validate_branch_ranges(x)

  cat("\n")
  cat("PhyloBasins branch ranges\n")
  cat("-------------------------\n")

  cat("Computed: ", x$computed, "\n", sep = "")

  if (is.null(x$table)) {

    cat("Table: <not available>\n")

  } else {

    cat("Branches: ", nrow(x$table), "\n", sep = "")

  }

  invisible(x)

}

# -----------------------------------------------------------------------------
# Summary
# -----------------------------------------------------------------------------

#' @export

summary.pb_branch_ranges <- function(object, ...) {

  validate_branch_ranges(object)

  out <- list(

    computed = object$computed,

    n_branches = if (is.null(object$table)) 0L else nrow(object$table)

  )

  class(out) <- "summary.pb_branch_ranges"

  out

}

#' @export

print.summary.pb_branch_ranges <- function(x, ...) {

  cat("\n")

  cat("Branch Range Summary\n")

  cat("--------------------\n")

  cat("Computed: ", x$computed, "\n", sep = "")

  cat("Branches: ", x$n_branches, "\n", sep = "")

  invisible(x)

}
