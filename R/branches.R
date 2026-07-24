# =============================================================================
# PhyloBasins
#
# Branch object
#
# Defines the internal branch representation used throughout the package.
# =============================================================================

# -----------------------------------------------------------------------------
# Internal constructor
# -----------------------------------------------------------------------------

new_branches <- function() {

  structure(

    list(

      table = NULL,

      prepared = FALSE,

      metadata = list(),

      cache = list()

    ),

    class = "pb_branches"

  )

}
# -----------------------------------------------------------------------------
# Validator
# -----------------------------------------------------------------------------

validate_branches <- function(x) {

  if (!inherits(x, "pb_branches")) {

    stop(
      "'x' must inherit from class 'pb_branches'.",
      call. = FALSE
    )

  }

  required <- c(
    "table",
    "prepared",
    "metadata",
    "cache"
  )
  missing <- setdiff(required, names(x))

  if (length(missing) > 0) {

    stop(
      sprintf(
        "Missing branch component(s): %s",
        paste(missing, collapse = ", ")
      ),
      call. = FALSE
    )

  }

  if (!is.logical(x$prepared) || length(x$prepared) != 1) {

    stop(
      "'prepared' must be a single logical value.",
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
# Print method
# -----------------------------------------------------------------------------

#' @export
print.pb_branches <- function(x, ...) {

  cat("\n")
  cat("PhyloBasins branches\n")
  cat("--------------------\n")

  cat("Prepared: ", x$prepared, "\n", sep = "")

  if (is.null(x$table)) {

    cat("Branch table: <not available>\n")

  } else {

    cat("Branches: ", nrow(x$table), "\n", sep = "")

  }

  invisible(x)

}

# -----------------------------------------------------------------------------
# Summary
# -----------------------------------------------------------------------------

#' @export
summary.pb_branches <- function(object, ...) {

  print(object)

  invisible(object)

}
