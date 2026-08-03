# =============================================================================
# plot_metric_theme.R
#
# Graphical theme used by plot_metric().
# =============================================================================

#' Default theme for phylogenetic diversity maps
#'
#' Returns the default ggplot2 theme used throughout the
#' PhyloBasins visualization module.
#'
#' The theme is intentionally minimal and publication-oriented.
#'
#' @return A ggplot2 theme object.
#'
#' @keywords internal
#' @noRd
plot_metric_theme <- function() {

  ggplot2::theme_minimal() +

    ggplot2::theme(

      # -------------------------------------------------------------
      # Background
      # -------------------------------------------------------------

      panel.background =
        ggplot2::element_blank(),

      plot.background =
        ggplot2::element_blank(),

      # -------------------------------------------------------------
      # Grid
      # -------------------------------------------------------------

      panel.grid.major =
        ggplot2::element_blank(),

      panel.grid.minor =
        ggplot2::element_blank(),

      # -------------------------------------------------------------
      # Axes
      # -------------------------------------------------------------

      axis.title =
        ggplot2::element_blank(),

      axis.text =
        ggplot2::element_blank(),

      axis.ticks =
        ggplot2::element_blank(),

      # -------------------------------------------------------------
      # Legend
      # -------------------------------------------------------------

      legend.position = "right",

      legend.title =
        ggplot2::element_text(
          face = "bold"
        ),

      legend.text =
        ggplot2::element_text(),

      # -------------------------------------------------------------
      # Plot title
      # -------------------------------------------------------------

      plot.title =
        ggplot2::element_text(
          face = "bold",
          hjust = 0.5
        ),

      plot.subtitle =
        ggplot2::element_text(
          hjust = 0.5
        ),

      plot.caption =
        ggplot2::element_text(
          size = 8,
          colour = "grey40"
        )

    )

}
