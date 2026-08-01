# =============================================================================
# PhyloBasins
#
# Build Site by Branch Matrix
# =============================================================================

#' Build the site by branch matrix
#'
#' Builds the sparse site by branch incidence matrix used by all downstream
#' phylogenetic diversity calculations.
#'
#' @param pb
#' A validated \code{pb_project}.
#'
#' @param overwrite
#' Logical. Rebuild an existing matrix?
#'
#' @param verbose
#' Logical. Print progress messages?
#'
#' @return
#' Updated \code{pb_project}.
#'
#' @export

build_site_branch_matrix <- function(

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

  if (!pb$branches$prepared) {

    stop(

      "Branch table has not been prepared.",

      call. = FALSE

    )

  }

  if (!pb$community$loaded) {

    stop(

      "Community matrix has not been loaded.",

      call. = FALSE

    )

  }

  if (isTRUE(pb$site_branch_matrix$built) &&
      !overwrite) {

    if (verbose) {

      message(

        "Site-branch matrix already available."

      )

    }

    return(pb)

  }

  sbm <- reference_branch_engine(

    tree = pb$tree,

    branches = pb$branches,

    community = pb$community

  )

  pb$site_branch_matrix <- sbm

  pb$site_branch_matrix$built <- TRUE

  if (verbose) {

    message(

      sprintf(

        "Built site by branch matrix (%d sites by %d branches).",

        nrow(sbm$matrix),

        ncol(sbm$matrix)

      )

    )

  }

  pb$history <- rbind(

    pb$history,

    data.frame(

      timestamp = timestamp(),

      action = "site_branch_matrix_built",

      stringsAsFactors = FALSE

    )

  )

  pb

}
