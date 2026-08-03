# =============================================================================
# plot_metric.R
#
# Generic plotting function for phylogenetic diversity metrics.
# =============================================================================

#' Plot a phylogenetic diversity metric
#'
#' Creates a publication-quality map of a phylogenetic diversity metric.
#'
#' @param pb A `pb_project`.
#' @param shape An sf object.
#' @param metric Character scalar specifying the metric.
#' @param id_col Identifier column used for joining metric values.
#' @param palette Viridis palette.
#' @param border_colour Polygon border colour.
#' @param border_size Polygon border linewidth.
#' @param na_colour Colour used for missing values.
#' @param legend_title Optional legend title. If NULL, the metric name
#'   is used.
#'
#' @return A ggplot object.
#'
#' @export
plot_metric <- function(
    pb,
    shape,
    metric,
    id_col = "HYBAS_ID",
    palette = "viridis",
    border_colour = "grey60",
    border_size = 0.15,
    na_colour = "grey90",
    legend_title = NULL
) {

  metric <- tolower(metric)

  if (is.null(legend_title)) {

    legend_title <- pb_metric_label(metric)

  }

  data <- prepare_plot_metric_data(
    pb = pb,
    shape = shape,
    metric = metric,
    id_col = id_col
  )

  p <- ggplot2::ggplot(data)

  p <- p +
    ggplot2::geom_sf(
      ggplot2::aes(fill = .data[[metric]]),
      colour = border_colour,
      linewidth = border_size
    )

  p <- p +
    plot_metric_scale(
      palette = palette,
      na_colour = na_colour,
      legend_title = legend_title
    )

  p <- p +
    plot_metric_theme()

  p

}
