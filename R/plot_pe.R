# =============================================================================
# plot_pe.R
#
# Plot Phylogenetic Endemism.
# =============================================================================

#' Plot Phylogenetic Endemism
#'
#' Creates a publication-quality map of phylogenetic endemism.
#'
#' This is a convenience wrapper around [plot_metric()] with
#' `metric = "pe"`.
#'
#' @param pb A `pb_project`.
#' @param shape An sf object.
#' @param id_col Identifier column used for joining metric values.
#' @param palette Viridis colour palette.
#' @param border_colour Polygon border colour.
#' @param border_size Polygon border linewidth.
#' @param na_colour Colour used for missing values.
#' @param legend_title Optional legend title.
#'
#' @return
#' A ggplot object.
#'
#' @seealso
#' [plot_metric()]
#'
#' @export
plot_pe <- function(
    pb,
    shape,
    id_col = "HYBAS_ID",
    palette = "viridis",
    border_colour = "grey60",
    border_size = 0.15,
    na_colour = "grey90",
    legend_title = "Phylogenetic Endemism"
) {

  plot_metric(

    pb = pb,

    shape = shape,

    metric = "pe",

    id_col = id_col,

    palette = palette,

    border_colour = border_colour,

    border_size = border_size,

    na_colour = na_colour,

    legend_title = legend_title

  )

}
