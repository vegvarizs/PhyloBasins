# =============================================================================
# PhyloBasins
#
# Read community matrix
#
# Reads a community (site × species) matrix into a pb_project.
# =============================================================================

#' Read a community matrix
#'
#' Reads a site × species community matrix from a matrix,
#' data.frame or csv file.
#'
#' @param pb
#' A \code{pb_project}.
#'
#' @param x
#' Community matrix. May be
#'
#' * a matrix
#' * a data.frame
#' * a character string giving the path to a csv file.
#'
#' @param row_names
#' Should the first column be interpreted as site names?
#'
#' @return
#' Updated \code{pb_project}.
#'
#' @export
read_community <- function(
    pb,
    x,
    row_names = TRUE
) {

  validate_pb_project(pb)

  ## ------------------------------------------------------------
  ## read object
  ## ------------------------------------------------------------

  if (is.character(x) && length(x) == 1) {

    comm <- utils::read.csv(

      x,

      row.names = if (row_names) 1 else NULL,

      check.names = FALSE

    )

    comm <- as.matrix(comm)

  } else if (is.data.frame(x)) {

    comm <- as.matrix(x)

  } else if (is.matrix(x)) {

    comm <- x

  } else {

    stop(
      "'x' must be a matrix, data.frame or csv filename.",
      call. = FALSE
    )

  }

  ## ------------------------------------------------------------
  ## store
  ## ------------------------------------------------------------

  pb$community$matrix <- comm

  pb$community$sites <- rownames(comm)

  pb$community$species <- colnames(comm)

  pb$community$loaded <- TRUE

  pb$community$prepared <- FALSE

  pb$community$validation <- list(valid = FALSE)

  pb$history <- rbind(

    pb$history,

    data.frame(

      timestamp = timestamp(),

      action = "community_loaded",

      stringsAsFactors = FALSE

    )

  )

  pb

}
