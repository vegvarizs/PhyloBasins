# =============================================================================
# PhyloBasins
#
# Branch ranges object
# =============================================================================

#' Create an empty branch ranges object
#'
#' @return A new \code{pb_branch_ranges} object.
#'
#' @keywords internal

new_branch_ranges <- function() {

  structure(

    list(

      values = NULL,
      computed = FALSE

    ),

    class = "pb_branch_ranges"

  )

}

#' @export

print.pb_branch_ranges <- function(x, ...) {

  cat("<pb_branch_ranges>\n")

  cat("Computed :", x$computed, "\n")

  if (is.null(x$values)) {

    cat("Values   : none\n")

  } else {

    cat("Branches :", length(x$values), "\n")

  }

  invisible(x)

}
