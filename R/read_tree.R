# =============================================================================
# PhyloBasins
#
# Tree import
#
# Read a phylogenetic tree from file and attach it to a pb_project.
# =============================================================================

#' Read a phylogenetic tree
#'
#' Imports a phylogenetic tree from a Newick or NEXUS file and stores it
#' in a \code{pb_project} object.
#'
#' Supported file extensions are:
#'
#' * .nwk
#' * .newick
#' * .tre
#' * .tree
#' * .nex
#' * .nexus
#'
#' @param pb
#' A \code{pb_project} object.
#'
#' @param file
#' Path to the tree file.
#'
#' @return
#' Updated \code{pb_project}.
#'
#' @examples
#' \dontrun{
#' pb <- pb_project()
#' pb <- read_tree(pb, "tree.nwk")
#' }
#'
#' @export
read_tree <- function(pb, file) {

  validate_pb_project(pb)

  if (!is.character(file) || length(file) != 1) {
    stop("'file' must be a character string.",
         call. = FALSE)
  }

  if (!file.exists(file)) {
    stop(
      sprintf("Tree file not found:\n%s", file),
      call. = FALSE
    )
  }

  ext <- tolower(tools::file_ext(file))

  phy <- switch(

    ext,

    nwk = ape::read.tree(file),
    newick = ape::read.tree(file),
    tre = ape::read.tree(file),
    tree = ape::read.tree(file),

    nex = ape::read.nexus(file),
    nexus = ape::read.nexus(file),

    stop(
      sprintf(
        "Unsupported tree format: '.%s'",
        ext
      ),
      call. = FALSE
    )

  )

  if (!inherits(phy, "phylo")) {
    stop(
      "Imported object is not of class 'phylo'.",
      call. = FALSE
    )
  }

  pb$tree <- new_tree(

    phy = phy,

    file = normalizePath(
      file,
      winslash = "/",
      mustWork = TRUE
    ),

    format = ext,

    loaded = TRUE,

    prepared = FALSE

  )

  pb$history <- rbind(

    pb$history,

    data.frame(

      timestamp = timestamp(),

      action = "tree_loaded",

      stringsAsFactors = FALSE

    )

  )

  pb

}
