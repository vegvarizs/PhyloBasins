# =============================================================================
# PhyloBasins
#
# Site-Branch Matrix object
#
# Defines the S3 class used to store site × branch incidence matrices.
# =============================================================================
#' Create a site-branch matrix object
#'
#' Creates a \code{pb_site_branch_matrix} object.
#'
#' @param matrix
#' Site-by-branch matrix.
#'
#' @param sites
#' Character vector of site names.
#'
#' @param branches
#' Character vector of branch identifiers.
#'
#' @param sparse
#' Logical indicating whether a sparse matrix representation is used.
#'
#' @param built
#' Logical indicating whether the matrix has been constructed.
#'
#' @param validation
#' Validation information.
#'
#' @param cache
#' Internal cache used during downstream computations.
#'
#' @return
#' A \code{pb_site_branch_matrix} object.
#'
#' @export
pb_site_branch_matrix <- function(
    matrix = NULL,
    sites = character(),
    branches = character(),
    sparse = TRUE,
    built = FALSE,
    validation = list(
      valid = FALSE,
      reference = FALSE
    ),
    cache = list(
      branch_index = NULL,
      site_index = NULL,
      transpose = NULL
    )
) {

  x <- list(

    matrix = matrix,

    sites = sites,

    branches = branches,

    sparse = sparse,

    built = built,

    validation = validation,

    cache = cache

  )

  class(x) <- "pb_site_branch_matrix"

  x

}
# -----------------------------------------------------------------------------
# Validation
# -----------------------------------------------------------------------------

#' Validate a site-branch matrix
#'
#' @param x
#' A \code{pb_site_branch_matrix}.
#'
#' @return
#' Invisibly returns TRUE.
#'
#' @keywords internal
validate_site_branch_matrix <- function(x){

  if(!inherits(x, "pb_site_branch_matrix"))
    stop(
      "Object is not a pb_site_branch_matrix.",
      call. = FALSE
    )

  required <- c(

    "matrix",
    "sites",
    "branches",
    "sparse",
    "built",
    "validation",
    "cache"

  )

  missing <- setdiff(required, names(x))

  if(length(missing)>0)
    stop(
      "Missing fields: ",
      paste(missing, collapse=", "),
      call. = FALSE
    )

  if (!is.list(x$cache)) {
    stop(
      "'cache' must be a list.",
      call. = FALSE
    )
  }

  required_cache <- c(
    "branch_index",
    "site_index",
    "transpose"
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

  invisible(TRUE)

}

# -----------------------------------------------------------------------------
# Print
# -----------------------------------------------------------------------------

#' @export
print.pb_site_branch_matrix <- function(x, ...){

  cat("\n")

  cat("Site-Branch Matrix\n")

  cat("------------------\n")

  cat("Sites:    ", length(x$sites), "\n")

  cat("Branches: ", length(x$branches), "\n")

  cat("Sparse:   ", x$sparse, "\n")

  cat("Built:    ", x$built, "\n")

  invisible(x)

}

# -----------------------------------------------------------------------------
# Summary
# -----------------------------------------------------------------------------

#' @export
summary.pb_site_branch_matrix <- function(object, ...){

  validate_site_branch_matrix(object)

  out <- list(

    n_sites = length(object$sites),

    n_branches = length(object$branches),

    sparse = object$sparse,

    built = object$built

  )

  class(out) <- "summary.pb_site_branch_matrix"

  out

}

#' @export
print.summary.pb_site_branch_matrix <- function(x, ...){

  cat("\n")

  cat("Site-Branch Matrix Summary\n")

  cat("--------------------------\n")

  cat("Number of sites:    ", x$n_sites, "\n")

  cat("Number of branches: ", x$n_branches, "\n")

  cat("Sparse matrix:      ", x$sparse, "\n")

  cat("Built:              ", x$built, "\n")

  invisible(x)

}
