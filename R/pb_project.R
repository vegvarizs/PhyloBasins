# =============================================================================
# PhyloBasins
#
# Project object
#
# Core S3 class used throughout the package.
# =============================================================================

# -----------------------------------------------------------------------------
# Constructor
# -----------------------------------------------------------------------------

#' Create a new PhyloBasins project
#'
#' Creates an empty PhyloBasins project object.
#'
#' @return
#' A \code{pb_project} object.
#'
#' @export

pb_project <- function() {

  structure(

    list(

      config = list(),

      metadata = list(

        created = Sys.time(),

        version = utils::packageVersion("PhyloBasins")

      ),

      history = data.frame(

        timestamp = as.POSIXct(character()),

        action = character(),

        stringsAsFactors = FALSE

      ),

      tree = pb_tree(),

      branches = pb_branches(),

      branch_ranges = pb_branch_ranges(),

      community = pb_community(),

      site_branch_matrix = pb_site_branch_matrix(),

      metrics = list(

        pd = list(

          values = NULL,

          computed = FALSE

        ),

        pe = list(

          values = NULL,

          computed = FALSE

        ),

        rpe = list(

          values = NULL,

          computed = FALSE

        )

      ),

      maps = list(),

      cache = list()

    ),

    class = "pb_project"

  )

}

# -----------------------------------------------------------------------------
# Validator
# -----------------------------------------------------------------------------

validate_pb_project <- function(x) {

  if (!inherits(x, "pb_project")) {

    stop(
      "'x' is not a pb_project.",
      call. = FALSE
    )

  }

  required <- c(

    "config",

    "metadata",

    "history",

    "tree",

    "branches",

    "branch_ranges",

    "community",

    "site_branch_matrix",

    "metrics",

    "maps",

    "cache"

  )

  missing <- setdiff(
    required,
    names(x)
  )

  if (length(missing) > 0) {

    stop(

      sprintf(

        "Missing component(s): %s",

        paste(
          missing,
          collapse = ", "
        )

      ),

      call. = FALSE

    )

  }

  validate_tree(x$tree)
  validate_branches(x$branches)
  validate_branch_ranges(x$branch_ranges)
  validate_community(x$community)
  validate_site_branch_matrix(x$site_branch_matrix)

  invisible(x)

}

# -----------------------------------------------------------------------------
# Print
# -----------------------------------------------------------------------------

#' @export

print.pb_project <- function(x, ...) {

  validate_pb_project(x)

  cat("\n")
  cat("PhyloBasins project\n")
  cat("-------------------\n")

  cat(
    "Created: ",
    format(x$metadata$created),
    "\n",
    sep = ""
  )

  cat(
    "Package version: ",
    as.character(x$metadata$version),
    "\n",
    sep = ""
  )

  cat(
    "Tree loaded: ",
    x$tree$loaded,
    "\n",
    sep = ""
  )

  cat(
    "Tree prepared: ",
    x$tree$prepared,
    "\n",
    sep = ""
  )

  cat(
    "Branches prepared: ",
    x$branches$prepared,
    "\n",
    sep = ""
  )

  cat(
    "Community loaded: ",
    x$community$loaded,
    "\n",
    sep = ""
  )

  cat(
    "Site-branch matrix built: ",
    x$site_branch_matrix$built,
    "\n",
    sep = ""
  )

  invisible(x)

}

# -----------------------------------------------------------------------------
# Summary
# -----------------------------------------------------------------------------

#' @export

summary.pb_project <- function(object, ...) {

  print(object)

  invisible(object)

}
