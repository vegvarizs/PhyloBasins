# =============================================================================
# PhyloBasins
#
# Tree object
#
# Defines the internal tree representation used throughout the package.
# =============================================================================

# -----------------------------------------------------------------------------
# Internal constructor
# -----------------------------------------------------------------------------

new_tree <- function(
    phy = NULL,
    file = NULL,
    format = NULL,
    loaded = FALSE,
    prepared = FALSE,
    validation = NULL,
    index = new_tree_index()
) {

  if (is.null(validation)) {

    validation <- list(
      valid = FALSE,
      rooted = NA,
      binary = NA,
      ultrametric = NA
    )

  }

  x <- list(

    phy = phy,

    file = file,

    format = format,

    loaded = loaded,

    prepared = prepared,

    validation = validation,

    index = index

  )

  class(x) <- "pb_tree"

  validate_tree(x)

}

# -----------------------------------------------------------------------------
# Validator
# -----------------------------------------------------------------------------

validate_tree <- function(x) {

  if (!inherits(x, "pb_tree")) {
    stop("'x' must inherit from class 'pb_tree'.",
         call. = FALSE)
  }

  required <- c(
    "phy",
    "file",
    "format",
    "loaded",
    "prepared",
    "validation",
    "index"
  )

  missing <- setdiff(required, names(x))

  if (length(missing) > 0) {

    stop(
      sprintf(
        "Missing tree component(s): %s",
        paste(missing, collapse = ", ")
      ),
      call. = FALSE
    )

  }

  if (!is.logical(x$loaded) || length(x$loaded) != 1) {
    stop("'loaded' must be a single logical value.",
         call. = FALSE)
  }

  if (!is.logical(x$prepared) || length(x$prepared) != 1) {
    stop("'prepared' must be a single logical value.",
         call. = FALSE)
  }

  if (!is.list(x$validation)) {
    stop("'validation' must be a list.",
         call. = FALSE)
  }

  if (!inherits(x$index, "pb_tree_index")) {

    stop(
      "'index' must inherit from class 'pb_tree_index'.",
      call. = FALSE
    )

  }

  invisible(x)

}

# -----------------------------------------------------------------------------
# Print method
# -----------------------------------------------------------------------------

#' @export
print.pb_tree <- function(x, ...) {

  cat("\n")
  cat("PhyloBasins tree\n")
  cat("----------------\n")

  cat("Loaded:   ", x$loaded, "\n", sep = "")
  cat("Prepared: ", x$prepared, "\n", sep = "")

  cat(
    "Indexed:  ",
    !is.na(x$index$n_nodes),
    "\n",
    sep = ""
  )

  if (is.null(x$file)) {
    cat("File:     <none>\n")
  } else {
    cat("File:     ", x$file, "\n", sep = "")
  }

  if (!is.null(x$phy)) {

    cat("Tips:      ", length(x$phy$tip.label), "\n", sep = "")
    cat("Edges:     ", nrow(x$phy$edge), "\n", sep = "")

  }

  if (!is.na(x$index$n_nodes)) {

    cat("Indexed nodes: ", x$index$n_nodes, "\n", sep = "")
    cat("Indexed edges: ", x$index$n_edges, "\n", sep = "")

  }

  invisible(x)

}

# -----------------------------------------------------------------------------
# Summary
# -----------------------------------------------------------------------------

#' @export
summary.pb_tree <- function(object, ...) {

  print(object)

  invisible(object)

}
