# =============================================================================
# plot_geometry.R
#
# Base plotting function for pb_geometry objects.
# =============================================================================

#' Plot geometry
#'
#' Plot the geometry stored in a PhyloBasins project.
#'
#' @param pb A pb_project object.
#' @param fill Optional numeric vector used for colouring.
#' @param border Border colour.
#' @param palette Colour palette.
#' @param legend_title Legend title.
#' @param na_colour Colour for missing values.
#' @param linewidth Polygon border width.
#'
#' @return A ggplot object.
#'
#' @export

plot_geometry <- function(
    pb,
    fill = NULL,
    border = "grey60",
    palette = "viridis",
    legend_title = ggplot2::waiver(),
    na_colour = "grey90",
    linewidth = 0.15
) {

  check_pb_project(pb)

  check_component_exists(pb, "geometry")

  check_loaded(pb$geometry, "geometry")

  geom <- geometry_sf(pb)

  plot_data <- geom

  if (!is.null(fill)) {

    if (length(fill) != nrow(plot_data)) {

      stop(
        "'fill' must have one value per geometry feature.",
        call. = FALSE
      )

    }

    plot_data$metric <- fill
  }

  p <- ggplot2::ggplot(plot_data)

  if (is.null(fill)) {

    p <- p +
      ggplot2::geom_sf(
        fill = "grey90",
        colour = border,
        linewidth = linewidth
      )

  } else {

    p <- p +
      ggplot2::geom_sf(
        ggplot2::aes(fill = .data$metric),
        colour = border,
        linewidth = linewidth
      )

    if (palette == "viridis") {

      p <- p +
        ggplot2::scale_fill_viridis_c(
          name = legend_title,
          na.value = na_colour
        )

    } else if (palette == "magma") {

      p <- p +
        ggplot2::scale_fill_viridis_c(
          option = "magma",
          name = legend_title,
          na.value = na_colour
        )

    } else if (palette == "plasma") {

      p <- p +
        ggplot2::scale_fill_viridis_c(
          option = "plasma",
          name = legend_title,
          na.value = na_colour
        )

    } else {

      stop(
        "Unknown palette.",
        call. = FALSE
      )

    }

  }

  p +
    ggplot2::coord_sf(expand = FALSE) +
    ggplot2::theme_void() +
    ggplot2::theme(

      panel.grid = ggplot2::element_blank(),

      legend.position = "right",

      legend.title = ggplot2::element_text(size = 10),

      legend.text = ggplot2::element_text(size = 9)

    )

}
