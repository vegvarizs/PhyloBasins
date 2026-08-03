# =============================================================================
# plot_metric_data.R
#
# Data preparation for plot_metric().
# =============================================================================

#' Prepare plotting data
#'
#' Joins a computed diversity metric to an sf object.
#'
#' @param pb A `pb_project`.
#' @param shape An sf object.
#' @param metric Character scalar.
#' @param id_col Character scalar giving the join column.
#'
#' @return An sf object with the requested metric attached.
#'
#' @keywords internal
#' @noRd
prepare_plot_metric_data <- function(
    pb,
    shape,
    metric,
    id_col
) {

  validate_plot_metric(
    pb = pb,
    shape = shape,
    metric = metric,
    id_col = id_col
  )

  metric_df <- metric_table(
    pb,
    metric = metric
  )

  if (!id_col %in% names(metric_df)) {

    stop(
      sprintf(
        "Column '%s' not found in metric table.",
        id_col
      ),
      call. = FALSE
    )

  }

  keep <- metric_df[
    ,
    c(id_col, metric),
    drop = FALSE
  ]

  data <- dplyr::left_join(
    shape,
    keep,
    by = id_col
  )

  n_missing <- sum(is.na(data[[metric]]))

  if (n_missing > 0L) {

    warning(
      sprintf(
        "%d feature(s) have missing values for '%s'.",
        n_missing,
        metric
      ),
      call. = FALSE
    )

  }

  data

}
