# =============================================================================
# PhyloBasins
#
# Tree data validation
#
# Validates the biological contents of a phylogenetic tree.
# =============================================================================

#' Validate tree data
#'
#' Performs a series of biological and structural checks on the tree stored
#' in a \code{pb_project}. The function updates the validation information
#' contained in \code{pb$tree$validation}.
#'
#' The tree itself is not modified.
#'
#' @param pb
#' A \code{pb_project} object with a loaded tree.
#'
#' @return
#' Updated \code{pb_project}.
#'
#' @examples
#' \dontrun{
#' pb <- pb_project()
#' pb <- read_tree(pb, "tree.nwk")
#' pb <- validate_tree_data(pb)
#' }
#'
#' @export
validate_tree_data <- function(pb) {

  validate_pb_project(pb)

  if (!pb$tree$loaded) {
    stop(
      "No tree has been loaded.",
      call. = FALSE
    )
  }

  phy <- pb$tree$phy

  if (!inherits(phy, "phylo")) {
    stop(
      "Tree is not a 'phylo' object.",
      call. = FALSE
    )
  }

  ## -------------------------------------------------------------------------
  ## Basic statistics
  ## -------------------------------------------------------------------------

  n_tips <- length(phy$tip.label)
  n_edges <- nrow(phy$edge)

  ## -------------------------------------------------------------------------
  ## Tip labels
  ## -------------------------------------------------------------------------

  has_tip_labels <-
    !is.null(phy$tip.label)

  duplicate_labels <-
    if (has_tip_labels)
      anyDuplicated(phy$tip.label) > 0
  else
    TRUE

  ## -------------------------------------------------------------------------
  ## Edge lengths
  ## -------------------------------------------------------------------------

  has_edge_lengths <-
    !is.null(phy$edge.length)

  missing_edge_lengths <-
    if (has_edge_lengths)
      any(is.na(phy$edge.length))
  else
    TRUE

  negative_edge_lengths <-
    if (has_edge_lengths)
      any(phy$edge.length < 0)
  else
    FALSE

  zero_edge_lengths <-
    if (has_edge_lengths)
      any(phy$edge.length == 0)
  else
    FALSE

  ## -------------------------------------------------------------------------
  ## Tree topology
  ## -------------------------------------------------------------------------

  rooted <- ape::is.rooted(phy)

  binary <- ape::is.binary.tree(phy)

  ultrametric <-
    tryCatch(
      ape::is.ultrametric(phy),
      error = function(e) NA
    )

  ## -------------------------------------------------------------------------
  ## Overall validity
  ## -------------------------------------------------------------------------

  valid <-

    has_tip_labels &&
    !duplicate_labels &&
    has_edge_lengths &&
    !missing_edge_lengths &&
    !negative_edge_lengths &&
    (n_tips >= 2)

  ## -------------------------------------------------------------------------
  ## Store validation
  ## -------------------------------------------------------------------------

  pb$tree$validation <- list(
    valid = FALSE,
    rooted = NA,
    binary = NA,
    ultrametric = NA,
    tips = NA_integer_,
    edges = NA_integer_,
    duplicate_labels = NA,
    missing_edge_lengths = NA,
    negative_edge_lengths = NA,
    zero_edge_lengths = NA
  )

  ## -------------------------------------------------------------------------
  ## History
  ## -------------------------------------------------------------------------

  pb$history <- rbind(

    pb$history,

    data.frame(

      timestamp = timestamp(),

      action = "tree_validated",

      stringsAsFactors = FALSE

    )

  )

  pb

}
