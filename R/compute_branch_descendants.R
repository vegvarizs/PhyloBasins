# =============================================================================
# PhyloBasins
#
# Compute descendant information for all branches
# =============================================================================

# -----------------------------------------------------------------------------
# Compute descendant counts
# -----------------------------------------------------------------------------

#' Compute branch descendants
#'
#' Computes the number of descendant tips subtended by every branch in the tree.
#' Optionally stores the complete descendant tip lists in the branch cache.
#'
#' @param pb A \code{pb_project} object.
#' @param store_descendants Logical; should complete descendant-tip lists be
#'   stored in \code{pb$branches$cache$descendants}?
#'
#' @return Updated \code{pb_project}.
#'
#' @export

compute_branch_descendants <- function(
    pb,
    store_descendants = FALSE
){

  validate_pb_project(pb)

  if (!pb$tree$prepared)
    stop("Tree has not been prepared.", call. = FALSE)

  if (!pb$branches$prepared)
    stop("Branch table has not been built.", call. = FALSE)

  phy <- pb$tree$phy

  n_tip  <- length(phy$tip.label)
  n_node <- phy$Nnode
  n_all  <- n_tip + n_node

  ############################################################
  ## children list
  ############################################################

  children <- pb$tree$index$children

  for(i in seq_len(nrow(phy$edge))){

    parent <- phy$edge[i,1]
    child  <- phy$edge[i,2]

    children[[parent]] <-
      c(children[[parent]], child)

  }

  ############################################################
  ## postorder traversal
  ############################################################

  phy_post <- ape::reorder.phylo(
    phy,
    order = "postorder"
  )

  postorder_nodes <- unique(phy_post$edge[,1])

  ############################################################
  ## descendant counts
  ############################################################

  descendant_count <- integer(n_all)

  descendant_count[seq_len(n_tip)] <- 1L

  if(store_descendants){

    descendants <- vector("list", n_all)

    for(i in seq_len(n_tip))
      descendants[[i]] <- i

  }

  for(node in postorder_nodes){

    ch <- children[[node]]

    if(length(ch) == 0)
      next

    descendant_count[node] <-
      sum(descendant_count[ch])

    if(store_descendants){

      descendants[[node]] <-
        unlist(
          descendants[ch],
          recursive = FALSE,
          use.names = FALSE
        )

    }

  }

  ############################################################
  ## map node values onto branches
  ############################################################

  edge_child <- phy$edge[,2]

  pb$branches$table$descendant_count <-
    descendant_count[edge_child]

  ############################################################
  ## optional cache
  ############################################################

  if(store_descendants){

    pb$branches$cache$descendants <-
      descendants[edge_child]

  }

  ############################################################
  ## metadata
  ############################################################

  pb$branches$metadata$descendants_computed <- TRUE

  pb$history <- rbind(

    pb$history,

    data.frame(

      timestamp = timestamp(),

      action = "branch_descendants_computed",

      stringsAsFactors = FALSE

    )

  )

  pb

}
