# =============================================================================
# plot_metric_scales.R
#
# Colour scales for the Visualization Module.
# =============================================================================

#' Colour scale for phylogenetic diversity maps
#'
#' Returns a ggplot2 colour scale used by plot_metric().
#'
#' @param palette Character scalar specifying the viridis palette.
#' @param na_colour Colour used for missing values.
#' @param legend_title Optional legend title.
#'
#' @return A ggplot2 scale object.
#'
#' @keywords internal
#' @noRd
plot_metric_scale <- function(
    palette = "viridis",
    na_colour = "grey90",
    legend_title = ggplot2::waiver()
) {

  palette <- tolower(palette)

  allowed_palettes <- pb_plot_palettes()

  if (!palette %in% allowed_palettes) {

    stop(
      sprintf(
        "Unknown palette '%s'. Available palettes are: %s.",
        palette,
        paste(allowed_palettes, collapse = ", ")
      ),
      call. = FALSE
    )

  }

  option <- switch(

    palette,

    viridis = "D",
    magma   = "A",
    inferno = "B",
    plasma  = "C",
    cividis = "E",
    turbo   = "F"

  )

  ggplot2::scale_fill_viridis_c(

    option = option,

    na.value = na_colour,

    name = legend_title

  )

}
