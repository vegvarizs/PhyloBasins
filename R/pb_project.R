# =============================================================================
# PhyloBasins
#
# Project object
#
# Core S3 class used throughout the package.
# =============================================================================

#' Create a new PhyloBasins project
#'
#' Creates an empty PhyloBasins project object.
#'
#' The returned object stores all data, metadata and intermediate
#' results generated during a PhyloBasins workflow.
#'
#' @return
#' An object of class \code{"pb_project"}.
#'
#' @examples
#' pb <- pb_project()
#'
#' @export
pb_project <- function() {

  timestamp <- timestamp()

  x <- list(

    config = list(
      verbose = TRUE,
      cache = TRUE,
      check = TRUE
    ),

    metadata = list(
      version = utils::packageVersion("PhyloBasins"),
      created = timestamp,
      R_version = getRversion(),
      platform = R.version$platform
    ),

    history = data.frame(
      timestamp = timestamp,
      action = "project_created",
      stringsAsFactors = FALSE
    ),

    tree = new_tree(),

    branches = new_branches(),

    community = new_community(),

    site_branch_matrix = new_site_branch_matrix(),


    metrics = list(),

    maps = list(),

    cache = list(
      enabled = TRUE,
      objects = list()
    )

  )

  class(x) <- "pb_project"

  validate_pb_project(x)
}


# -----------------------------------------------------------------------------
# Internal validator
# -----------------------------------------------------------------------------

validate_pb_project <- function(x) {

  if (!inherits(x, "pb_project")) {
    stop("'x' is not a pb_project.", call. = FALSE)
  }

  required <- c(
    "config",
    "metadata",
    "history",
    "tree",
    "branches",
    "community",
    "site_branch_matrix",
    "metrics",
    "maps",
    "cache"
  )
  missing <- setdiff(required, names(x))

  if (length(missing) > 0) {
    stop(
      sprintf(
        "Missing component(s): %s",
        paste(missing, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  invisible(x)
}


# -----------------------------------------------------------------------------
# Print method
# -----------------------------------------------------------------------------

#' @export
print.pb_project <- function(x, ...) {

  cat("\n")
  cat("PhyloBasins project\n")
  cat("-------------------\n")

  cat("Created: ",
      format(x$metadata$created),
      "\n",
      sep = "")

  cat("Package version: ",
      as.character(x$metadata$version),
      "\n",
      sep = "")

  cat("Tree loaded: ",
      x$tree$loaded,
      "\n",
      sep = "")

  cat("Tree prepared: ",
      x$tree$prepared,
      "\n",
      sep = "")

  cat("Branch table: ")

  if (is.null(x$branches$table)) {
    cat("not available\n")
  } else {
    cat(nrow(x$branches$table), "branches\n")
  }

  cat("Community loaded: ",
      x$community$loaded,
      "\n",
      sep = "")

  cat("Community prepared: ",
      x$community$prepared,
      "\n",
      sep = "")

  cat("Site-branch matrix: ")

  if (!x$site_branch_matrix$built) {

    cat("not built\n")

  } else {

    cat(length(x$site_branch_matrix$sites),
        " sites × ",
        length(x$site_branch_matrix$branches),
        " branches\n",
        sep = "")

  }

  invisible(x)
}


# -----------------------------------------------------------------------------
# Summary method
# -----------------------------------------------------------------------------

#' @export
summary.pb_project <- function(object, ...) {

  print(object)

  invisible(object)

}
