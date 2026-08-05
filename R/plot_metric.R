# =============================================================================
# plot_metric.R
#
# Generic plotting function for site-level metrics.
# =============================================================================

#' Plot a metric
#'
#' Generic plotting function used by the metric wrappers.
#'
#' @param pb A `pb_project` object.
#' @param metric Metric name.
#' @param palette Colour palette.
#' @param border Border colour.
#' @param linewidth Polygon border width.
#' @param na_colour Colour for missing values.
#' @param legend_title Legend title.
#' @param ... Additional arguments passed to [plot_geometry()].
#'
#' @return A ggplot object.
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

  check_pb_project(pb)

  check_component_exists(pb, "metrics")

  if (!metric %in% names(pb$metrics)) {

    stop(
      sprintf("Metric '%s' is not available.", metric),
      call. = FALSE
    )

  }

  metric_obj <- pb$metrics[[metric]]

  if (!is.list(metric_obj) || is.null(metric_obj$values)) {

    stop(
      sprintf("Metric '%s' has no values.", metric),
      call. = FALSE
    )

  }

  values <- metric_obj$values

  if (is.null(legend_title)) {

    legend_title <- toupper(metric)

  }

  plot_geometry(
    pb = pb,
    fill = values,
    border = border,
    linewidth = linewidth,
    palette = palette,
    legend_title = legend_title,
    na_colour = na_colour,
    ...
  )

}
