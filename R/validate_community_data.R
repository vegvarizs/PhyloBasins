# =============================================================================
# PhyloBasins
#
# Validate community data
# =============================================================================

#' Validate community data
#'
#' Performs structural validation of the community matrix.
#'
#' @param pb
#' A \code{pb_project}.
#'
#' @return
#' Updated \code{pb_project}.
#'
#' @export
validate_community_data <- function(pb) {

  validate_pb_project(pb)

  if (!pb$community$loaded) {

    stop(
      "No community matrix has been loaded.",
      call. = FALSE
    )

  }

  comm <- pb$community$matrix

  ## ------------------------------------------------------------
  ## matrix
  ## ------------------------------------------------------------

  if (!is.matrix(comm)) {

    stop(
      "Community data must be stored as a matrix.",
      call. = FALSE
    )

  }

  if (nrow(comm) == 0 || ncol(comm) == 0) {

    stop(
      "Community matrix is empty.",
      call. = FALSE
    )

  }

  ## ------------------------------------------------------------
  ## names
  ## ------------------------------------------------------------

  if (is.null(rownames(comm))) {

    stop(
      "Community matrix has no site names.",
      call. = FALSE
    )

  }

  if (is.null(colnames(comm))) {

    stop(
      "Community matrix has no species names.",
      call. = FALSE
    )

  }

  if (anyDuplicated(rownames(comm))) {

    stop(
      "Duplicate site names detected.",
      call. = FALSE
    )

  }

  if (anyDuplicated(colnames(comm))) {

    stop(
      "Duplicate species names detected.",
      call. = FALSE
    )

  }

  ## ------------------------------------------------------------
  ## values
  ## ------------------------------------------------------------

  if (anyNA(comm)) {

    stop(
      "Community matrix contains missing values.",
      call. = FALSE
    )

  }

  values <- unique(as.vector(comm))

  if (!all(values %in% c(0, 1))) {

    stop(
      "Community matrix must contain only 0 and 1 values.",
      call. = FALSE
    )

  }

  ## ------------------------------------------------------------
  ## compare with tree
  ## ------------------------------------------------------------

  if (pb$tree$loaded) {

    tree_species <- pb$tree$phy$tip.label

    missing_tree <- setdiff(colnames(comm), tree_species)

    missing_comm <- setdiff(tree_species, colnames(comm))

    if (length(missing_tree) > 0) {

      stop(

        sprintf(
          "%d community species are absent from the tree.",
          length(missing_tree)
        ),

        call. = FALSE

      )

    }

    if (length(missing_comm) > 0) {

      stop(

        sprintf(
          "%d tree species are absent from the community matrix.",
          length(missing_comm)
        ),

        call. = FALSE

      )

    }

  }

  pb$community$validation <- list(

    valid = TRUE,

    n_sites = nrow(comm),

    n_species = ncol(comm)

  )

  pb$history <- rbind(

    pb$history,

    data.frame(

      timestamp = timestamp(),

      action = "community_validated",

      stringsAsFactors = FALSE

    )

  )

  pb

}
