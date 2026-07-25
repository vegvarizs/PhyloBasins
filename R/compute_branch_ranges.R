# =============================================================================
# Compute branch ranges
# =============================================================================

#' Compute branch ranges
#'
#' Computes the number of sites in which every branch occurs.
#'
#' @param pb A PhyloBasins project.
#'
#' @return
#' Updated project.
#'
#' @export

compute_branch_ranges <- function(pb) {

  validate_pb_project(pb)

  validate_site_branch_matrix(
    pb$site_branch_matrix
  )

  range_size <-
    Matrix::colSums(
      pb$site_branch_matrix$matrix
    )

  if (length(range_size) != nrow(pb$branches$table)) {
    stop(
      "Branch table and site-branch matrix are inconsistent.",
      call. = FALSE
    )
  }

  pb$branches$table$range_size <-
    as.integer(range_size)

  pb

}
