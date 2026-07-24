# =============================================================================
# PhyloBasins
#
# Build branch table
#
# Creates the canonical branch table used throughout the package.
# =============================================================================

#' Build branch table
#'
#' Converts the edge matrix of a prepared phylogenetic tree into the canonical
#' branch table used throughout PhyloBasins.
#'
#' The resulting table contains only primary information that is directly
#' available from the phylogenetic tree. Derived quantities (e.g. descendant
#' counts, branch ranges or branch weights) are computed later by dedicated
#' functions.
#'
#' @param pb
#' A prepared \code{pb_project}.
#'
#' @return
#' Updated \code{pb_project}.
#'
#' @examples
#' \dontrun{
#' pb <- pb_project()
#' pb <- read_tree(pb, "tree.nwk")
#' pb <- validate_tree_data(pb)
#' pb <- prepare_tree(pb)
#' pb <- build_branch_table(pb)
#' }
#'
#' @export
build_branch_table <- function(pb) {

  validate_pb_project(pb)

  if (!inherits(pb$branches, "pb_branches")) {
    stop(
      "'pb$branches' is not a valid pb_branches object.",
      call. = FALSE
    )
  }

  if (!pb$tree$prepared) {
    stop(
      "Tree has not been prepared.",
      call. = FALSE
    )
  }

  phy <- pb$tree$phy

  edge <- phy$edge
  edge_length <- phy$edge.length

  n_tips <- length(phy$tip.label)

  parent <- edge[, 1]
  child  <- edge[, 2]

  child_is_tip <- child <= n_tips

  root_node <- pb$tree$index$root

  is_root_branch <- parent == root_node

  tip_name <- rep(NA_character_, length(child))

  tip_name[child_is_tip] <-
    phy$tip.label[child[child_is_tip]]

  branch_table <- data.frame(

    branch_id = seq_len(nrow(edge)),

    parent = parent,

    child = child,

    length = edge_length,

    is_root_branch = is_root_branch,

    child_is_tip = child_is_tip,

    tip_name = tip_name,

    stringsAsFactors = FALSE

  )

  pb$branches$table <- branch_table

  pb$branches$prepared <- TRUE

  pb$branches$metadata$n_branches <-
    nrow(branch_table)

  pb$branches$metadata$total_length <-
    sum(edge_length, na.rm = TRUE)

  pb$history <- rbind(

    pb$history,

    data.frame(

      timestamp = timestamp(),

      action = "branch_table_built",

      stringsAsFactors = FALSE

    )

  )

  pb

}
