# =============================================================================
# plot_pe.R
#
# Plot Phylogenetic Endemism.
# =============================================================================
#
#' Plot Faith's Phylogenetic Endemism.
#'
#' Convenience wrapper around [plot_metric()] with `metric = "pe"`.
#'
#' @param pb A `pb_project`.
#' @param palette Viridis colour palette.
#' @param border Polygon border colour.
#' @param linewidth Polygon border linewidth.
#' @param na_colour Colour used for missing values.
#' @param legend_title Optional legend title.
#'
#' @return
#' A ggplot object.
#'
#' @export

plot_pe <- function(
    pb,
    palette = "viridis",
    border = "grey60",
    linewidth = 0.15,
    na_colour = "grey90",
    legend_title = "Phylogenetic Endemism"
) {

  plot_metric(
    pb = pb,
    metric = "pe",
    palette = palette,
    border = border,
    linewidth = linewidth,
    na_colour = na_colour,
    legend_title = legend_title
  )

}
