# =============================================================================
# plot_metric.R
#
# Generic plotting function for site-level metrics.
# =============================================================================

#' Plot a metric
#'
#' Generic plotting function used by the metric wrappers.
#'
#' @param pb
#' A `pb_project` object.
#'
#' @param metric
#' Metric name.
#'
#' @param palette
#' Colour palette.
#'
#' @param border
#' Border colour.
#'
#' @param linewidth
#' Polygon border width.
#'
#' @param na_colour
#' Colour for missing values.
#'
#' @param legend_title
#' Legend title.
#'
#' @param ...
#' Additional arguments passed to [plot_geometry()].
#'
#' @return
#' A ggplot object.
#'
#' @export

plot_metric <- function(
    pb,
    metric,
    palette = "viridis",
    border = "grey60",
    linewidth = 0.15,
    na_colour = "grey90",
    legend_title = NULL,
    ...
) {

  validate_pb_project(pb)

  check_component_exists(pb, "metrics")

  if (!metric %in% names(pb$metrics)) {

    stop(
      sprintf(
        "Metric '%s' is not available.",
        metric
      ),
      call. = FALSE
    )

  }

  metric_obj <- pb$metrics[[metric]]

  if (!is.list(metric_obj) ||
      !isTRUE(metric_obj$computed) ||
      is.null(metric_obj$values)) {

    stop(
      sprintf(
        "Metric '%s' has not been computed.",
        metric
      ),
      call. = FALSE
    )

  }

  values <- metric_obj$values

  if (!is.data.frame(values)) {

    stop(
      sprintf(
        "Metric '%s' must be stored as a data frame.",
        metric
      ),
      call. = FALSE
    )

  }

  if (!all(c("HYBAS_ID", metric) %in% names(values))) {

    stop(
      sprintf(
        "Metric '%s' has incorrect table structure.",
        metric
      ),
      call. = FALSE
    )

  }

  fill_values <- values[[metric]]

  if (!is.numeric(fill_values)) {

    stop(
      sprintf(
        "Metric '%s' is not numeric.",
        metric
      ),
      call. = FALSE
    )

  }

  if (is.null(legend_title)) {

    legend_title <- toupper(metric)

  }

  plot_geometry(

    pb = pb,

    fill = fill_values,

    border = border,

    linewidth = linewidth,

    palette = palette,

    legend_title = legend_title,

    na_colour = na_colour,

    ...

  )

}
