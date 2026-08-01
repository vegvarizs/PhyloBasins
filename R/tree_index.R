# =============================================================================
# PhyloBasins
#
# Tree index object
#
# Defines the internal index representation used throughout the package.
# =============================================================================

# -----------------------------------------------------------------------------
# Internal constructor
# -----------------------------------------------------------------------------

pb_tree_index <- function(

  root = NA_integer_,
  parent = integer(),
  children = list(),
  tip_nodes = integer(),
  internal_nodes = integer(),
  postorder = integer(),
  n_tips = NA_integer_,
  n_nodes = NA_integer_,
  n_edges = NA_integer_

) {

  x <- list(

    root = root,

    parent = parent,

    children = children,

    tip_nodes = tip_nodes,

    internal_nodes = internal_nodes,

    postorder = postorder,

    n_tips = n_tips,

    n_nodes = n_nodes,

    n_edges = n_edges

  )


  class(x) <- "pb_tree_index"

  validate_tree_index(x)

  x

}

# -----------------------------------------------------------------------------
# Validator
# -----------------------------------------------------------------------------

validate_tree_index <- function(x) {

  if (!inherits(x, "pb_tree_index")) {

    stop(
      "'x' must inherit from class 'pb_tree_index'.",
      call. = FALSE
    )

  }

  required <- c(

    "root",
    "parent",
    "children",
    "tip_nodes",
    "internal_nodes",
    "postorder",
    "n_tips",
    "n_nodes",
    "n_edges"

  )

  missing <- setdiff(required, names(x))

  if (length(missing) > 0) {

    stop(

      sprintf(
        "Missing tree index component(s): %s",
        paste(missing, collapse = ", ")
      ),

      call. = FALSE

    )

  }

  # ---------------------------------------------------------------------------
  # Basic type checks (always required)
  # ---------------------------------------------------------------------------

  if (!is.integer(x$root)) {

    stop(
      "'root' must be an integer value.",
      call. = FALSE
    )

  }

  if (!is.integer(x$parent)) {

    stop(
      "'parent' must be an integer vector.",
      call. = FALSE
    )

  }

  if (!is.list(x$children)) {

    stop(
      "'children' must be a list.",
      call. = FALSE
    )

  }

  if (!is.integer(x$tip_nodes)) {

    stop(
      "'tip_nodes' must be an integer vector.",
      call. = FALSE
    )

  }

  if (!is.integer(x$internal_nodes)) {

    stop(
      "'internal_nodes' must be an integer vector.",
      call. = FALSE
    )

  }

  if (!is.integer(x$postorder)) {

    stop(
      "'postorder' must be an integer vector.",
      call. = FALSE
    )

  }

  if (!is.numeric(x$n_tips) || length(x$n_tips) != 1) {

    stop(
      "'n_tips' must be a single numeric value.",
      call. = FALSE
    )

  }

  if (!is.numeric(x$n_nodes) || length(x$n_nodes) != 1) {

    stop(
      "'n_nodes' must be a single numeric value.",
      call. = FALSE
    )

  }

  if (!is.numeric(x$n_edges) || length(x$n_edges) != 1) {

    stop(
      "'n_edges' must be a single numeric value.",
      call. = FALSE
    )

  }

  # ---------------------------------------------------------------------------
  # If the tree has not yet been indexed, stop here.
  # ---------------------------------------------------------------------------

  if (is.na(x$n_nodes)) {

    return(invisible(x))

  }

  # ---------------------------------------------------------------------------
  # Consistency checks for a completed index
  # ---------------------------------------------------------------------------

  if (length(x$postorder) != x$n_nodes) {

    stop(
      "'postorder' must contain exactly one entry for each internal node.",
      call. = FALSE
    )

  }

  if (length(x$children) != x$n_nodes) {

    stop(
      "'children' must contain one element for each internal node.",
      call. = FALSE
    )

  }

  invisible(x)

}
# -----------------------------------------------------------------------------
# Print method
# -----------------------------------------------------------------------------

#' @export
print.pb_tree_index <- function(x, ...) {

  cat("\n")
  cat("PhyloBasins tree index\n")
  cat("----------------------\n")

  cat("Root node: ", x$root, "\n", sep = "")
  cat("Tips:      ", x$n_tips, "\n", sep = "")
  cat("Nodes:     ", x$n_nodes, "\n", sep = "")
  cat("Edges:     ", x$n_edges, "\n", sep = "")
  cat("Postorder: ", length(x$postorder), " nodes\n", sep = "")
  cat("Prepared:  ", length(x$postorder) > 0, "\n", sep = "")

  invisible(x)

}

# -----------------------------------------------------------------------------
# Summary
# -----------------------------------------------------------------------------

#' @export
summary.pb_tree_index <- function(object, ...) {

  print(object)

  invisible(object)

}

new_tree_index <- pb_tree_index
