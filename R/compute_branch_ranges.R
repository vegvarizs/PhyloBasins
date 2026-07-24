# =============================================================================
# PhyloBasins
#
# compute_branch_ranges.R
#
# Computes branch range sizes (number of occupied sites) for every branch.
# NOTE:
#  - branch_sites are computed only as a temporary object.
#  - They are NOT stored in the project cache.
# =============================================================================

#' Compute branch ranges
#'
#' Computes the number of sites occupied by each branch (the union of the
#' occupied sites of all descendant species).
#'
#' The resulting range sizes are stored in
#' \code{pb$branches$table$branch_range}.
#'
#' @param pb A prepared \code{pb_project}.
#'
#' @return Updated \code{pb_project}.
#'
#' @export
compute_branch_ranges <- function(pb){

  validate_pb_project(pb)

  if (!pb$tree$prepared)
    stop("Tree must be prepared.", call. = FALSE)

  if (!pb$community$prepared)
    stop("Community must be prepared.", call. = FALSE)

  branches      <- pb$branches$table
  descendants   <- pb$branches$cache$descendants
  species_sites <- pb$community$cache$species_sites

  branch_sites <- vector("list", length(descendants))

  for(i in seq_along(descendants)){

    tip_ids <- descendants[[i]]
    sp_names <- pb$tree$phy$tip.label[tip_ids]

    branch_sites[[i]] <-
      sort(unique(
        unlist(species_sites[sp_names], use.names = FALSE)
      ))

  }

  branches$branch_range <-
    vapply(branch_sites, length, integer(1))

  pb$branches$table <- branches

  ## branch_sites intentionally NOT cached.
  ## The canonical representation of branch occurrences will be the
  ## site × branch sparse matrix created by compute_site_branch_matrix().

  pb$history <- rbind(
    pb$history,
    data.frame(
      timestamp = timestamp(),
      action = "branch_ranges_computed",
      stringsAsFactors = FALSE
    )
  )

  pb
}
