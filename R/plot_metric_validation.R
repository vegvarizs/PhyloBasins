# =============================================================================
# plot_metric_validation.R
#
# Validation utilities for plot_metric().
# =============================================================================

#' Validate inputs for plot_metric()
#'
#' Performs all validation required before plotting a phylogenetic
#' diversity metric.
#'
#' Checks:
#'
#' * pb_project
#' * requested metric
#' * metric availability
#' * sf object
#' * identifier column
#'
#' @param pb A `pb_project`.
#' @param shape An sf object.
#' @param metric Character scalar.
#' @param id_col Character scalar identifying the join column.
#'
#' @return Invisibly returns TRUE.
#'
#' @keywords internal
#' @noRd
validate_plot_metric <- function(
    pb,
    shape,
    metric,
    id_col
) {

  validate_pb_project(pb)

  # ---------------------------------------------------------------------------
  # metric
  # ---------------------------------------------------------------------------

  if (!is.character(metric) || length(metric) != 1L) {

    stop(
      "'metric' must be a single character string.",
      call. = FALSE
    )

  }

  metric <- tolower(metric)

  allowed_metrics <- pb_metric_names()

  if (!metric %in% allowed_metrics) {

    stop(
      sprintf(
        "Unknown metric '%s'. Available metrics are: %s.",
        metric,
        paste(allowed_metrics, collapse = ", ")
      ),
      call. = FALSE
    )

  }

  # ---------------------------------------------------------------------------
  # metric availability
  # ---------------------------------------------------------------------------

  metric_object <- pb$metrics[[metric]]

  if (is.null(metric_object)) {

    stop(
      sprintf(
        "Metric '%s' has not been initialised.",
        metric
      ),
      call. = FALSE
    )

  }

  if (!isTRUE(metric_object$computed)) {

    stop(
      sprintf(
        "Metric '%s' has not been computed.\nRun run_pipeline() first.",
        metric
      ),
      call. = FALSE
    )

  }

  # ---------------------------------------------------------------------------
  # shape
  # ---------------------------------------------------------------------------

  if (!inherits(shape, "sf")) {

    stop(
      "'shape' must be an sf object.",
      call. = FALSE
    )

  }

  if (nrow(shape) == 0L) {

    stop(
      "'shape' contains no features.",
      call. = FALSE
    )

  }

  # ---------------------------------------------------------------------------
  # identifier column
  # ---------------------------------------------------------------------------

  if (!is.character(id_col) || length(id_col) != 1L) {

    stop(
      "'id_col' must be a single character string.",
      call. = FALSE
    )

  }

  if (!id_col %in% names(shape)) {

    stop(
      sprintf(
        "Column '%s' was not found in 'shape'.",
        id_col
      ),
      call. = FALSE
    )

  }

  invisible(TRUE)

}
