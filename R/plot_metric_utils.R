# =============================================================================
# plot_metric_utils.R
#
# Utility functions for the Visualization Module.
# =============================================================================

#' Supported phylogenetic diversity metrics
#'
#' Returns the names of all metrics currently supported by the
#' visualization module.
#'
#' @return Character vector.
#'
#' @keywords internal
#' @noRd
pb_metric_names <- function() {

  c(
    "pd",
    "pe",
    "rpe"
  )

}


#' Supported colour palettes
#'
#' Returns the names of all colour palettes supported by
#' plot_metric().
#'
#' @return Character vector.
#'
#' @keywords internal
#' @noRd
pb_plot_palettes <- function() {

  c(
    "viridis",
    "magma",
    "inferno",
    "plasma",
    "cividis",
    "turbo"
  )

}


#' Default legend title
#'
#' Returns the default legend title for a metric.
#'
#' @param metric Character scalar.
#'
#' @return Character scalar.
#'
#' @keywords internal
#' @noRd
pb_metric_label <- function(metric) {

  metric <- tolower(metric)

  switch(

    metric,

    pd  = "Faith's PD",

    pe  = "Phylogenetic Endemism",

    rpe = "Relative Phylogenetic Endemism",

    toupper(metric)

  )

}
