# =============================================================================
# PhyloBasins
#
# Build branch table
#
# Creates the branch table used by all downstream phylogenetic metrics.
# =============================================================================

#' Build branch table
#'
#' Builds the internal branch representation from a prepared phylogenetic tree.
#'
#' @param pb
#' A validated \code{pb_project}.
#'
#' @param overwrite
#' Logical. Rebuild an existing branch table?
#'
#' @param verbose
#' Logical. Print progress messages?
#'
#' @return
#' Updated \code{pb_project}.
#'
#' @export
branches <- function(
    pb,
    overwrite = FALSE,
    verbose = TRUE
) {

  validate_pb_project(pb)

  if (!pb$tree$prepared) {
    stop(
      "Tree has not been prepared.",
      call. = FALSE
    )
  }

  if (isTRUE(pb$branches$prepared) && !overwrite) {

    if (verbose) {
      message("Branch table already available.")
    }

    return(pb)

  }

  phy <- pb$tree$phy
  edge <- phy$edge
  edge.length <- phy$edge.length

  if (is.null(edge.length)) {

    edge.length <- rep(
      1,
      nrow(edge)
    )

  }

  n_tips <- ape::Ntip(phy)

  parent <- edge[, 1]
  child <- edge[, 2]

  branch_label <- character(length(child))

  is_tip <- child <= n_tips

  branch_label[is_tip] <-
    phy$tip.label[child[is_tip]]

  branch_label[!is_tip] <-
    paste0(
      "Node",
      child[!is_tip]
    )

  branch_table <- data.frame(

    branch_id = seq_len(nrow(edge)),

    parent = parent,

    child = child,

    label = branch_label,

    length = edge.length,

    is_tip = is_tip,

    stringsAsFactors = FALSE

  )

  pb$branches <- pb_branches()

  pb$branches$table <- branch_table

  pb$branches$prepared <- TRUE

  pb$branches$metadata$n_branches <-
    nrow(branch_table)

  pb$branches$cache$tip_branches <-
    which(is_tip)

  pb$branches$cache$internal_branches <-
    which(!is_tip)

  # ---------------------------------------------------------------------------
  # Descendant species cache
  # ---------------------------------------------------------------------------

  descendant_species <- vector(
    "list",
    nrow(branch_table)
  )

  children <- pb$tree$index$children

  tip_nodes <- pb$tree$index$tip_nodes

  collect_descendants <- function(node) {

    if (node %in% tip_nodes) {
      return(node)
    }

    idx <- match(
      node,
      pb$tree$index$internal_nodes
    )

    kids <- children[[idx]]

    unlist(
      lapply(
        kids,
        collect_descendants
      ),
      use.names = FALSE
    )

  }

  for (i in seq_len(nrow(branch_table))) {

    node <- branch_table$child[i]

    tips <- collect_descendants(node)

    descendant_species[[i]] <-
      phy$tip.label[tips]

  }

  pb$branches$cache$descendant_species <-
    descendant_species

  if (verbose) {

    message(

      sprintf(

        "Built branch table (%d branches).",

        nrow(branch_table)

      )

    )

  }

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
