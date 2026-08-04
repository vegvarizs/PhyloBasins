# =============================================================================
# read_community.R
#
# Read a community matrix from a file or an R object.
# =============================================================================

#' Read a community matrix
#'
#' Imports a community matrix into a \code{pb_project}.
#'
#' Exactly one of \code{file} or \code{data} must be supplied.
#'
#' @param pb A pb_project object.
#' @param file Path to a community matrix.
#' @param data A data.frame or matrix.
#' @param id_col Optional identifier column (name or index).
#' @param verbose Logical.
#'
#' @return
#' Updated \code{pb_project}.
#'
#' @export

read_community <- function(
    pb,
    file = NULL,
    data = NULL,
    id_col = NULL,
    verbose = TRUE
) {

  ## -------------------------------------------------------------------------
  ## Check project
  ## -------------------------------------------------------------------------

  if (!inherits(pb, "pb_project")) {

    stop(
      "'pb' must be a pb_project.",
      call. = FALSE
    )

  }

  ## -------------------------------------------------------------------------
  ## Exactly one input
  ## -------------------------------------------------------------------------

  supplied <- sum(
    !vapply(
      list(file, data),
      is.null,
      logical(1)
    )
  )

  if (supplied != 1) {

    stop(
      "Exactly one of 'file' or 'data' must be supplied.",
      call. = FALSE
    )

  }

  ## -------------------------------------------------------------------------
  ## Read from file
  ## -------------------------------------------------------------------------

  if (!is.null(file)) {

    if (!file.exists(file)) {

      stop(
        "Cannot find file:\n",
        file,
        call. = FALSE
      )

    }

    comm <- utils::read.csv(
      file,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )

  }

  ## -------------------------------------------------------------------------
  ## Read from R object
  ## -------------------------------------------------------------------------

  if (!is.null(data)) {

    if (!(is.data.frame(data) || is.matrix(data))) {

      stop(
        "'data' must be a data.frame or matrix.",
        call. = FALSE
      )

    }

    comm <- as.data.frame(
      data,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )

  }

  ## -------------------------------------------------------------------------
  ## Identifier column
  ## -------------------------------------------------------------------------

  if (!is.null(id_col)) {

    if (is.numeric(id_col)) {

      if (length(id_col) != 1 ||
          id_col < 1 ||
          id_col > ncol(comm)) {

        stop(
          "Invalid 'id_col' index.",
          call. = FALSE
        )

      }

      rownames(comm) <- comm[[id_col]]
      comm[[id_col]] <- NULL

    } else {

      if (!id_col %in% names(comm)) {

        stop(
          "Unknown id column: ",
          id_col,
          call. = FALSE
        )

      }

      rownames(comm) <- comm[[id_col]]
      comm[[id_col]] <- NULL

    }

  }


  ## -------------------------------------------------------------------------
  ## Validation
  ## -------------------------------------------------------------------------

  if (ncol(comm) == 0) {

    stop(
      "Community matrix has no species columns.",
      call. = FALSE
    )

  }

  if (is.null(colnames(comm))) {

    stop(
      "Community matrix has no column names.",
      call. = FALSE
    )

  }

  if (anyDuplicated(colnames(comm))) {

    stop(
      "Duplicated species names detected.",
      call. = FALSE
    )

  }

  ## -------------------------------------------------------------------------
  ## Convert to integer
  ## -------------------------------------------------------------------------

  comm[] <- lapply(
    seq_along(comm),
    function(i) {

      z <- comm[[i]]

      x <- suppressWarnings(as.integer(z))

      if (any(is.na(x) & !is.na(z))) {

        stop(
          sprintf(
            "Community matrix contains non-numeric values in column '%s'.",
            names(comm)[i]
          ),
          call. = FALSE
        )

      }

      x
    }
  )

  ## -------------------------------------------------------------------------
  ## Matrix
  ## -------------------------------------------------------------------------

  comm <- as.matrix(comm)

  storage.mode(comm) <- "integer"

  ## -------------------------------------------------------------------------
  ## Store
  ## -------------------------------------------------------------------------

  ## -------------------------------------------------------------------------
  ## Store
  ## -------------------------------------------------------------------------

  pb$community <- new_community(

    matrix = comm,

    sites = rownames(comm),

    taxa = colnames(comm),

    file = if (is.null(file)) {
      NA_character_
    } else {
      normalizePath(
        file,
        winslash = "/",
        mustWork = FALSE
      )
    },

    loaded = TRUE,

    validation = list(
      valid = TRUE
    ),

    metadata = list(),

    cache = list(
      taxa_index = NULL,
      site_index = NULL
    )

  )

  if (verbose) {

    message(
      "Imported ",
      nrow(comm),
      " communities and ",
      ncol(comm),
      " species."
    )

  }

  pb
}
