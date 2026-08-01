# =============================================================================
# PhyloBasins
#
# Community import
#
# Read a community matrix from file.
# =============================================================================

#' Read community matrix
#'
#' Imports a community (presence/absence or abundance) matrix and stores it in
#' a \code{pb_project}.
#'
#' The first column must contain site names.
#' Remaining columns correspond to taxa.
#'
#' @param pb
#' A validated \code{pb_project}.
#'
#' @param file
#' Path to the community matrix.
#'
#' @param verbose
#' Logical. Should progress messages be printed?
#'
#' @return
#' Updated \code{pb_project}.
#'
#' @export

read_community <- function(
    pb,
    file,
    verbose = TRUE
) {

  validate_pb_project(pb)

  if (!is.character(file) || length(file) != 1) {

    stop(
      "'file' must be a character string.",
      call. = FALSE
    )

  }

  if (!file.exists(file)) {

    stop(
      sprintf(
        "Community file not found:\n%s",
        file
      ),
      call. = FALSE
    )

  }

  ext <- tolower(tools::file_ext(file))

  dat <- switch(

    ext,

    csv = utils::read.csv(
      file,
      check.names = FALSE,
      stringsAsFactors = FALSE
    ),

    txt = utils::read.table(
      file,
      header = TRUE,
      sep = "\t",
      check.names = FALSE,
      stringsAsFactors = FALSE
    ),

    tsv = utils::read.table(
      file,
      header = TRUE,
      sep = "\t",
      check.names = FALSE,
      stringsAsFactors = FALSE
    ),

    stop(
      sprintf(
        "Unsupported community format: '.%s'",
        ext
      ),
      call. = FALSE
    )

  )

  if (ncol(dat) < 2) {

    stop(
      "Community matrix must contain at least one species column.",
      call. = FALSE
    )

  }

  rownames(dat) <- dat[[1]]

  mat <- as.matrix(dat[, -1, drop = FALSE])

  storage.mode(mat) <- "numeric"

  pb$community <- pb_community(

    matrix = mat,

    sites = rownames(mat),

    taxa = colnames(mat),

    file = normalizePath(
      file,
      winslash = "/",
      mustWork = TRUE
    ),

    loaded = TRUE

  )

  if (verbose) {

    message(

      sprintf(

        "Loaded community matrix (%d sites by %d taxa).",

        nrow(mat),
        ncol(mat)

      )

    )

  }

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
