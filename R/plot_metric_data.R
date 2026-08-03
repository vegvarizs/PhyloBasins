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
#' @return
#' An sf object with the requested metric attached.
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

  if (!"site" %in% names(metric_df)) {
    stop(
      "metric_table() must return a 'site' column.",
      call. = FALSE
    )
  }

  if (!"value" %in% names(metric_df)) {
    stop(
      "metric_table() must return a 'value' column.",
      call. = FALSE
    )
  }

  if (!id_col %in% names(shape)) {
    stop(
      sprintf(
        "Column '%s' not found in shape.",
        id_col
      ),
      call. = FALSE
    )
  }

  ## Rename to the names expected by plot_metric()
  names(metric_df) <- c(id_col, metric)

  ## -------------------------------------------------------------------------
  ## Harmonise join key type
  ## -------------------------------------------------------------------------
  ##
  ## metric_table() returns site identifiers as character because they
  ## originate from names(values). The shape object, however, may store
  ## identifiers as numeric (e.g. HydroBASINS HYBAS_ID). Convert both
  ## sides to character before joining.
  ##
  metric_df[[id_col]] <- as.character(metric_df[[id_col]])
  shape[[id_col]]     <- as.character(shape[[id_col]])

  data <- dplyr::left_join(
    shape,
    metric_df,
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
