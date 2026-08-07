# =============================================================================
# join_metric_to_geometry.R
#
# Join computed metrics to basin geometry.
# =============================================================================

#' Join metrics to geometry
#'
#' Attach one or more computed metrics to the geometry stored in a
#' PhyloBasins project.
#'
#' @param pb A pb_project object.
#' @param metrics Character vector of metric names.
#'   If NULL, all available metrics are joined.
#' @param copy_geometry Logical.
#'   If TRUE (default), return a copy of the geometry.
#'   If FALSE, modify the geometry stored in the project.
#' @param overwrite Replace existing attribute columns.
#'
#' @return
#' If `copy_geometry = TRUE`, an **sf** object.
#' Otherwise an updated **pb_project**.
#'
#' @export

join_metric_to_geometry <- function(
    pb,
    metrics = NULL,
    copy_geometry = TRUE,
    overwrite = FALSE
) {

  validate_pb_project(pb)

  check_component_exists(pb, "geometry")
  check_component_exists(pb, "metrics")

  check_loaded(pb$geometry, "geometry")

  if (is.null(pb$geometry$cache$link)) {

    stop(
      "Geometry has not been linked. Run prepare_geometry().",
      call. = FALSE
    )

  }

  ## -------------------------------------------------------------------------
  ## Metrics to export
  ## -------------------------------------------------------------------------

  available <- names(pb$metrics)

  if (is.null(metrics)) {

    metrics <- available

  }

  missing <- setdiff(metrics, available)

  if (length(missing) > 0) {

    stop(
      sprintf(
        "Unknown metric(s): %s",
        paste(missing, collapse = ", ")
      ),
      call. = FALSE
    )

  }

  geom <- geometry_sf(pb)

  rows <- pb$geometry$cache$link$community_to_geometry

  ## -------------------------------------------------------------------------
  ## Join metrics
  ## -------------------------------------------------------------------------

  for (metric in metrics) {

    metric_obj <- pb$metrics[[metric]]

    if (!is.list(metric_obj) ||
        !isTRUE(metric_obj$computed) ||
        is.null(metric_obj$values)) {

      stop(
        sprintf(
          "Metric '%s' has not been computed.",
          metric
        ),
        call. = FALSE
      )

    }

    values <- metric_obj$values

    if (!is.data.frame(values)) {

      stop(
        sprintf(
          "Metric '%s' must be stored as a data frame.",
          metric
        ),
        call. = FALSE
      )

    }

    if (!all(c("HYBAS_ID", metric) %in% names(values))) {

      stop(
        sprintf(
          "Metric '%s' has incorrect table structure.",
          metric
        ),
        call. = FALSE
      )

    }

    if (nrow(values) != length(rows)) {

      stop(
        sprintf(
          "Metric '%s' has incorrect number of rows.",
          metric
        ),
        call. = FALSE
      )

    }

    if (!overwrite &&
        metric %in% names(geom)) {

      stop(
        sprintf(
          "Column '%s' already exists.",
          metric
        ),
        call. = FALSE
      )

    }

    geom[[metric]] <- NA_real_

    geom[[metric]][rows] <- values[[metric]]

  }

  if (copy_geometry) {

    return(geom)

  }

  pb$geometry$sf <- geom

  pb

}
