# =============================================================================
# PhyloBasins
#
# Community preparation
#
# Validate and prepare a community matrix for downstream analyses.
# =============================================================================

#' Prepare a community matrix
#'
#' Validates a site × species community matrix and builds internal metadata
#' required by downstream PhyloBasins algorithms.
#'
#' No biodiversity metrics are calculated.
#'
#' @param pb
#' A validated \code{pb_project}.
#'
#' @return
#' Updated \code{pb_project}.
#'
#' @export
prepare_community <- function(pb) {

  validate_pb_project(pb)

  if (!pb$community$loaded) {

    stop(
      "No community matrix has been loaded.",
      call. = FALSE
    )

  }

  comm <- pb$community$matrix

  if (is.null(comm)) {

    stop(
      "Community matrix is NULL.",
      call. = FALSE
    )

  }

  ## ------------------------------------------------------------
  ## dimensions
  ## ------------------------------------------------------------

  if (nrow(comm) == 0L) {

    stop(
      "Community matrix has no sites.",
      call. = FALSE
    )

  }

  if (ncol(comm) == 0L) {

    stop(
      "Community matrix has no species.",
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

  ## ------------------------------------------------------------
  ## duplicated names
  ## ------------------------------------------------------------

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
  ## missing values
  ## ------------------------------------------------------------

  if (anyNA(comm)) {

    stop(
      "Community matrix contains missing values.",
      call. = FALSE
    )

  }

  ## ------------------------------------------------------------
  ## numeric
  ## ------------------------------------------------------------

  if (!is.numeric(comm)) {

    stop(
      "Community matrix must be numeric.",
      call. = FALSE
    )

  }

  ## ------------------------------------------------------------
  ## binary check
  ## ------------------------------------------------------------

  values <- unique(as.vector(comm))

  if (!all(values %in% c(0, 1))) {

    stop(
      "Community matrix must contain only 0 and 1.",
      call. = FALSE
    )

  }

  ## ------------------------------------------------------------
  ## metadata
  ## ------------------------------------------------------------

  pb$community$metadata <- list(

    n_sites = nrow(comm),

    n_species = ncol(comm),

    occupancy =
      Matrix::colSums(comm),

    richness =
      Matrix::rowSums(comm)

  )

  pb$community$validation <- list(

    valid = TRUE

  )

  pb$community$prepared <- TRUE

  ## ------------------------------------------------------------
  ## history
  ## ------------------------------------------------------------

  pb$history <- rbind(

    pb$history,

    data.frame(

      timestamp = timestamp(),

      action = "community_prepared",

      stringsAsFactors = FALSE

    )

  )

  pb

}
