# =============================================================================
# PhyloBasins
#
# PE object
# =============================================================================

#' Create a PE object
#'
#' Creates an empty container for phylogenetic endemism results.
#'
#' @return
#' A \code{pb_pe} object.
#'
#' @export

new_pe <- function() {

  x <- list(

    values = NULL,

    computed = FALSE

  )

  class(x) <- "pb_pe"

  x

}


#' @export

print.pb_pe <- function(x, ...) {

  cat("Phylogenetic Endemism object\n")

  cat("Computed:", x$computed, "\n")

  invisible(x)

}


validate_pe <- function(x) {

  if (!inherits(x, "pb_pe")) {

    stop(
      "Object is not a pb_pe.",
      call. = FALSE
    )

  }

  invisible(TRUE)

}
