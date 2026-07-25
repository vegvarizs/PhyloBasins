# =============================================================================
# Compute phylogenetic endemism
# =============================================================================

#' Compute phylogenetic endemism
#'
#' Computes phylogenetic endemism (PE) for every site.
#'
#' @param pb
#' A prepared PhyloBasins project.
#'
#' @return
#' Updated project.
#'
#' @export

compute_pe <- function(pb) {

  validate_pb_project(pb)

  validate_site_branch_matrix(
    pb$site_branch_matrix
  )

  branch_table <- pb$branches$table

  if (is.null(branch_table$length)) {

    stop(
      "Branch lengths are missing.",
      call. = FALSE
    )

  }

  if (is.null(branch_table$range_size)) {

    stop(
      "Branch ranges have not been computed.",
      call. = FALSE
    )

  }

  if (any(branch_table$range_size <= 0)) {

    stop(
      "Branch range sizes must be positive.",
      call. = FALSE
    )

  }

  weights <-
    branch_table$length /
    branch_table$range_size

  M <- pb$site_branch_matrix$matrix

  PE <- as.numeric(M %*% weights)

  pb$metrics$pe$values <-
    data.frame(

      site = rownames(M),

      pe = PE,

      stringsAsFactors = FALSE

    )

  pb$metrics$pe$computed <- TRUE

  pb

}
