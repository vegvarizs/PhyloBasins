# =============================================================================
# PhyloBasins
#
# Tree preparation
#
# Build reusable index structures for downstream analyses.
# =============================================================================

#' Prepare a phylogenetic tree
#'
#' Builds internal index structures used by downstream PhyloBasins
#' algorithms. No biodiversity metrics are calculated.
#'
#' @param pb
#' A validated \code{pb_project}.
#'
#' @return
#' Updated \code{pb_project}.
#'
#' @export
prepare_tree <- function(pb) {

  validate_pb_project(pb)

  if (!pb$tree$loaded)
    stop("No tree has been loaded.", call. = FALSE)

  if (!isTRUE(pb$tree$validation$valid))
    stop("Tree has not passed validation.", call. = FALSE)

  phy <- pb$tree$phy
  edge <- phy$edge

  n_tips  <- length(phy$tip.label)
  n_nodes <- phy$Nnode
  total_nodes <- n_tips + n_nodes

  ## ------------------------------------------------------------
  ## parent lookup
  ## ------------------------------------------------------------

  parent <- rep.int(NA_integer_, total_nodes)

  parent[edge[, 2]] <- edge[, 1]

  ## ------------------------------------------------------------
  ## children lookup
  ## ------------------------------------------------------------

  children <- vector("list", total_nodes)

  for (i in seq_len(nrow(edge))) {

    p <- edge[i, 1]
    ch <- edge[i, 2]

    children[[p]] <- c(children[[p]], ch)

  }

  ## ------------------------------------------------------------
  ## node sets
  ## ------------------------------------------------------------

  tip_nodes <- seq_len(n_tips)

  internal_nodes <- seq.int(n_tips + 1L, total_nodes)

  root <- setdiff(edge[, 1], edge[, 2])

  if (length(root) != 1)
    stop("Unable to identify unique root.", call. = FALSE)

  ## ------------------------------------------------------------
  ## postorder traversal
  ## ------------------------------------------------------------

  phy_post <- ape::reorder.phylo(
    phy,
    order = "postorder"
  )

  postorder <- unique(phy_post$edge[, 1])

  ## ------------------------------------------------------------
  ## store
  ## ------------------------------------------------------------

  pb$tree$index <- new_tree_index(

    root = root,

    parent = parent,

    children = children,

    tip_nodes = tip_nodes,

    internal_nodes = internal_nodes,

    postorder = postorder,

    n_tips = n_tips,

    n_nodes = n_nodes,

    n_edges = nrow(edge)

  )

  pb$tree$prepared <- TRUE

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
