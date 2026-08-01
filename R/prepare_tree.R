# =============================================================================
# Prepare tree
# =============================================================================

#' Prepare a phylogenetic tree
#'
#' Builds internal index structures used by downstream PhyloBasins algorithms.
#'
#' @param pb A validated pb_project.
#' @param verbose Logical. Print progress messages?
#'
#' @return Updated pb_project.
#'
#' @export

prepare_tree <- function(
    pb,
    verbose = TRUE
) {

  validate_pb_project(pb)

  if (!pb$tree$loaded) {
    stop(
      "No tree has been loaded.",
      call. = FALSE
    )
  }

  if (!isTRUE(pb$tree$validation$valid)) {
    stop(
      "Tree has not passed validation.",
      call. = FALSE
    )
  }

  if (isTRUE(pb$tree$prepared)) {

    if (verbose)
      message("Tree already prepared.")

    return(pb)

  }

  phy <- pb$tree$phy
  edge <- phy$edge

  n_tips <- ape::Ntip(phy)
  n_nodes <- phy$Nnode
  total_nodes <- n_tips + n_nodes

  # ---------------------------------------------------------------------------
  # Parent lookup
  # ---------------------------------------------------------------------------

  parent <- rep.int(
    NA_integer_,
    total_nodes
  )

  parent[edge[, 2]] <- edge[, 1]

  # ---------------------------------------------------------------------------
  # Node sets
  # ---------------------------------------------------------------------------

  tip_nodes <- seq_len(n_tips)

  internal_nodes <- seq.int(
    n_tips + 1L,
    total_nodes
  )

  # ---------------------------------------------------------------------------
  # Children lookup
  # ---------------------------------------------------------------------------

  children <- vector(
    "list",
    length(internal_nodes)
  )

  for (i in seq_along(internal_nodes)) {

    node <- internal_nodes[i]

    children[[i]] <- edge[
      edge[, 1] == node,
      2
    ]

  }

  # ---------------------------------------------------------------------------
  # Root
  # ---------------------------------------------------------------------------

  root <- setdiff(
    edge[, 1],
    edge[, 2]
  )

  root <- as.integer(root)

  if (length(root) != 1) {
    stop(
      "Unable to identify unique root.",
      call. = FALSE
    )
  }

  # ---------------------------------------------------------------------------
  # Postorder traversal
  # ---------------------------------------------------------------------------

  phy_post <- ape::reorder.phylo(
    phy,
    order = "postorder"
  )

  postorder <- unique(
    phy_post$edge[, 1]
  )

  postorder <- postorder[
    postorder %in% internal_nodes
  ]

  postorder <- as.integer(postorder)

  # ---------------------------------------------------------------------------
  # Build index
  # ---------------------------------------------------------------------------

  pb$tree$index <- new_tree_index(

    root = root,

    parent = as.integer(parent),

    children = children,

    tip_nodes = as.integer(tip_nodes),

    internal_nodes = as.integer(internal_nodes),

    postorder = postorder,

    n_tips = n_tips,

    n_nodes = n_nodes,

    n_edges = nrow(edge)

  )

  pb$tree$prepared <- TRUE

  if (verbose) {

    message(

      sprintf(

        "Prepared tree (%d tips, %d internal nodes, %d edges).",

        n_tips,
        n_nodes,
        nrow(edge)

      )

    )

  }

  pb$history <- rbind(

    pb$history,

    data.frame(

      timestamp = timestamp(),

      action = "tree_prepared",

      stringsAsFactors = FALSE

    )

  )

  pb

}
