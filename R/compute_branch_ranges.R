# =============================================================================
# PhyloBasins
#
# Compute branch geographic ranges
# =============================================================================

# -----------------------------------------------------------------------------
# Compute branch ranges
# -----------------------------------------------------------------------------

#' Compute branch geographic ranges
#'
#' Calculates the geographic range of every branch from the site-by-branch
#' matrix.
#'
#' @param pb
#' A \code{pb_project} object.
#'
#' @param overwrite
#' Logical. Overwrite an existing result?
#'
#' @param verbose
#' Logical. Print progress messages?
#'
#' @return
#' Updated \code{pb_project}.
#'
#' @export

compute_branch_ranges <- function(
    pb,
    overwrite = FALSE,
    verbose = TRUE
) {

  validate_pb_project(pb)

  if (!pb$tree$prepared)
    stop("Tree has not been prepared.", call. = FALSE)

  if (!pb$branches$prepared)
    stop("Branches have not been prepared.", call. = FALSE)

  if (!pb$site_branch_matrix$built)
    stop("Site-branch matrix has not been built.", call. = FALSE)

  if (!overwrite &&
      pb$branch_ranges$computed)
    stop(
      "Branch ranges have already been computed.",
      call. = FALSE
    )

  if (verbose)
    message("Computing branch ranges...")

  M <- pb$site_branch_matrix$matrix

  branch_table <- pb$branches$table

  ## -------------------------------------------------------------------------
  ## Number of sites occupied by each branch
  ## -------------------------------------------------------------------------

  branch_site_count <-

    if (inherits(M, "Matrix")) {

      Matrix::colSums(M)

    } else {

      colSums(M)

    }

  n_sites <- nrow(M)

  proportion <- branch_site_count / n_sites

  ## -------------------------------------------------------------------------
  ## Inverse range
  ## -------------------------------------------------------------------------

  inverse_range <- numeric(length(branch_site_count))

  positive <- branch_site_count > 0

  inverse_range[positive] <- 1 / branch_site_count[positive]

  ## -------------------------------------------------------------------------
  ## Weighted branch lengths
  ## -------------------------------------------------------------------------

  weighted_length <-
    branch_table$length * inverse_range

  ## -------------------------------------------------------------------------
  ## Equal-branch-length tree
  ## -------------------------------------------------------------------------

  total_tree_length <-
    sum(branch_table$length)

  equal_branch_length <-
    total_tree_length / nrow(branch_table)

  ## -------------------------------------------------------------------------
  ## Output table
  ## -------------------------------------------------------------------------

  out <- data.frame(

    branch_id = branch_table$branch_id,

    length = branch_table$length,

    n_sites = as.integer(branch_site_count),

    proportion = proportion,

    inverse_range = inverse_range,

    weighted_length = weighted_length,

    stringsAsFactors = FALSE

  )

  ## -------------------------------------------------------------------------
  ## Branch range object
  ## -------------------------------------------------------------------------

  br <- pb_branch_ranges(

    table = out,

    computed = TRUE

  )

  ## -------------------------------------------------------------------------
  ## Cache
  ## -------------------------------------------------------------------------

  br$cache$branch_site_count <-
    branch_site_count

  br$cache$inverse_range <-
    inverse_range

  br$cache$weighted_length <-
    weighted_length

  br$cache$total_tree_length <-
    total_tree_length

  br$cache$equal_branch_length <-
    equal_branch_length

  pb$branch_ranges <- br

  if (verbose)
    message("Done.")

  pb

}
