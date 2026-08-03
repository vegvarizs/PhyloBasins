# =============================================================================
# PhyloBasins
#
# Extract a metric table from a pb_project
# =============================================================================

#' Extract a metric table
#'
#' Returns a standardised data frame containing the values of a computed
#' diversity metric.
#'
#' The returned table can be used by plotting, exporting and summary
#' functions without accessing the internal structure of a
#' \code{pb_project}.
#'
#' @param pb
#' A validated \code{pb_project}.
#'
#' @param metric
#' Character string specifying the metric.
#' Currently one of
#' \code{"pd"},
#' \code{"pe"} or
#' \code{"rpe"}.
#'
#' @return
#' A data frame with two columns:
#'
#' \describe{
#'   \item{site}{Site names.}
#'   \item{value}{Metric values.}
#' }
#'
#' @examples
#' \dontrun{
#'
#' tab <- metric_table(
#'   pb,
#'   metric = "pd"
#' )
#'
#' head(tab)
#'
#' }
#'
#' @export

metric_table <- function(
    pb,
    metric
) {

  validate_pb_project(pb)

  metric <-
    tolower(metric)

  supported <-
    c(
      "pd",
      "pe",
      "rpe"
    )

  if (!metric %in% supported) {

    stop(

      sprintf(
        "Unknown metric '%s'. Supported metrics are: %s.",
        metric,
        paste(supported, collapse = ", ")
      ),

      call. = FALSE

    )

  }

  metric_object <-
    pb$metrics[[metric]]

  if (is.null(metric_object)) {

    stop(

      sprintf(
        "Metric '%s' is not available.",
        metric
      ),

      call. = FALSE

    )

  }

  if (!isTRUE(metric_object$computed)) {

    stop(

      sprintf(
        "Metric '%s' has not been computed.",
        metric
      ),

      call. = FALSE

    )

  }

  values <-
    metric_object$values

  if (is.null(values)) {

    stop(

      sprintf(
        "Metric '%s' contains no values.",
        metric
      ),

      call. = FALSE

    )

  }

  if (is.null(names(values))) {

    stop(

      sprintf(
        "Metric '%s' has no site names.",
        metric
      ),

      call. = FALSE

    )

  }

  data.frame(

    site = names(values),

    value = as.numeric(values),

    stringsAsFactors = FALSE

  )

}
