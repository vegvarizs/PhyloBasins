# =============================================================================
# PhyloBasins
#
# Extract a metric table from a pb_project
# =============================================================================

#' Extract a metric table
#'
#' Returns a standardised table containing the values of a computed metric.
#'
#' @param pb
#' A validated \code{pb_project}.
#'
#' @param metric
#' Character string specifying the metric.
#'
#' @return
#' A data frame containing the requested metric.
#'
#' @export

metric_table <- function(
    pb,
    metric
) {

  validate_pb_project(pb)

  metric <- tolower(metric)

  supported <- c(
    "richness",
    "turnover",
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

  metric_object <- pb$metrics[[metric]]

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

  values <- metric_object$values

  if (!is.data.frame(values)) {

    stop(

      sprintf(
        "Metric '%s' does not contain a valid table.",
        metric
      ),

      call. = FALSE

    )

  }

  if (nrow(values) == 0) {

    stop(

      sprintf(
        "Metric '%s' contains no values.",
        metric
      ),

      call. = FALSE

    )

  }

  values

}
