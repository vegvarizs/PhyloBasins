# =============================================================================
# PhyloBasins
#
# Faith's Phylogenetic Diversity object
# =============================================================================

#' Create an empty PD object
#'
#' Internal constructor for storing Faith's phylogenetic diversity.
#'
#' @return
#' An object of class \code{"pb_pd"}.
new_pd <- function() {

  structure(

    list(

      values = NULL,

      computed = FALSE

    ),

    class = "pb_pd"

  )

}

validate_pd <- function(x) {

  if (!inherits(x, "pb_pd")) {
    stop("'x' is not a pb_pd.", call. = FALSE)
  }

  invisible(x)

}


#' @export
print.pb_pd <- function(x, ...) {

  if (!x$computed) {

    cat("Faith's PD: not computed\n")

  } else {

    cat(
      "Faith's PD computed for",
      length(x$values),
      "sites\n"
    )

  }

  invisible(x)

}
