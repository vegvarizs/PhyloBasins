# =============================================================================
# PhyloBasins
#
# Compute subtree lengths
#
# Calculates the total branch length descending from every branch.
# The subtree length includes the focal branch itself.
# =============================================================================

#' Compute subtree lengths
#'
#' Calculates the cumulative branch length for every branch in the tree.
#' Each subtree length equals the length of the focal branch plus the
#' lengths of all descendant branches.
#'
#' @param pb
#' A prepared \code{pb_project}.
#'
#' @return
#' Updated \code{pb_project}.
#'
#' @export
compute_subtree_lengths <- function(pb) {

  validate_pb_project(pb)

  if (!pb$tree$prepared)
    stop(
      "Tree has not been prepared.",
      call. = FALSE
    )

  if (!pb$branches$prepared)
    stop(
      "Branch table has not been prepared.",
      call. = FALSE
    )

  branches <- pb$branches$table

  children <- pb$tree$index$children
  postorder <- pb$tree$index$postorder

  edge_lookup <- integer(max(branches$child))

  edge_lookup[] <- NA_integer_

  edge_lookup[branches$child] <- seq_len(nrow(branches))

  subtree <- branches$length

  for (node in postorder) {

    parent_edge <- edge_lookup[node]

    if (is.na(parent_edge))
      next

    child_nodes <- children[[node]]

    if (length(child_nodes) == 0)
      next

    child_edges <- edge_lookup[child_nodes]

    child_edges <- child_edges[!is.na(child_edges)]

    if (length(child_edges) == 0)
      next

    subtree[parent_edge] <-
      subtree[parent_edge] +
      sum(subtree[child_edges])

  }

  branches$subtree_length <- subtree

  pb$branches$table <- branches

  pb$history <- rbind(

    pb$history,

    data.frame(

      timestamp = timestamp(),

      action = "subtree_lengths_computed",

      stringsAsFactors = FALSE

    )

  )

  pb

}
