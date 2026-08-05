# =============================================================================
# plot_pd.R
# =============================================================================

#' Plot Faith's Phylogenetic Diversity
#'
#' Convenience wrapper around [plot_metric()] with `metric = "pd"`.
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

plot_pd <- function(
    pb,
    palette = "viridis",
    border = "grey60",
    linewidth = 0.15,
    na_colour = "grey90",
    legend_title = "Faith's PD"
) {

  plot_metric(
    pb = pb,
    metric = "pd",
    palette = palette,
    border = border,
    linewidth = linewidth,
    na_colour = na_colour,
    legend_title = legend_title
  )

}
