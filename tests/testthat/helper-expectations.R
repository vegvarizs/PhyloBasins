# =============================================================================
# Test helper expectations
# =============================================================================

# -----------------------------------------------------------------------------
# pb_project
# -----------------------------------------------------------------------------

expect_valid_pb_project <- function(pb) {

  expect_s3_class(pb, "pb_project")

  required <- c(
    "config",
    "metadata",
    "history",
    "tree",
    "branches",
    "community",
    "site_branch_matrix",
    "branch_ranges",
    "metrics",
    "maps",
    "cache"
  )

  expect_true(
    all(required %in% names(pb)),
    info = paste(
      "Missing:",
      paste(setdiff(required, names(pb)), collapse = ", ")
    )
  )

  invisible(pb)

}

# -----------------------------------------------------------------------------
# Tree
# -----------------------------------------------------------------------------

expect_valid_tree <- function(pb) {

  expect_s3_class(pb$tree, "pb_tree")

  expect_true(pb$tree$loaded)

  expect_true(
    inherits(pb$tree$phy, "phylo")
  )

  invisible(pb)

}

# -----------------------------------------------------------------------------
# Branch table
# -----------------------------------------------------------------------------

expect_valid_branches <- function(pb) {

  expect_s3_class(pb$branches, "pb_branches")

  expect_true(pb$branches$prepared)

  expect_true(is.data.frame(pb$branches$table))

  expect_gt(nrow(pb$branches$table), 0)

  required <- c(
    "branch_id",
    "parent",
    "child",
    "label",
    "length",
    "is_tip"
  )

  expect_true(
    all(required %in% names(pb$branches$table))
  )

  invisible(pb)

}

# -----------------------------------------------------------------------------
# Community
# -----------------------------------------------------------------------------

expect_valid_community <- function(pb) {

  expect_s3_class(pb$community, "pb_community")

  expect_true(pb$community$loaded)

  expect_true(is.matrix(pb$community$matrix))

  expect_gt(nrow(pb$community$matrix), 0)

  expect_gt(ncol(pb$community$matrix), 0)

  invisible(pb)

}

# -----------------------------------------------------------------------------
# Site × Branch matrix
# -----------------------------------------------------------------------------

expect_valid_site_branch_matrix <- function(pb) {

  expect_s3_class(
    pb$site_branch_matrix,
    "pb_site_branch_matrix"
  )

  expect_true(pb$site_branch_matrix$built)

  expect_true(

    inherits(
      pb$site_branch_matrix$matrix,
      "Matrix"
    ) ||
      is.matrix(pb$site_branch_matrix$matrix)

  )

  invisible(pb)

}

# -----------------------------------------------------------------------------
# Branch ranges
# -----------------------------------------------------------------------------

expect_valid_branch_ranges <- function(pb) {

  expect_s3_class(
    pb$branch_ranges,
    "pb_branch_ranges"
  )

  expect_true(pb$branch_ranges$computed)

  expect_true(
    is.data.frame(pb$branch_ranges$table)
  )

  required <- c(
    "branch_id",
    "length",
    "n_sites",
    "proportion",
    "inverse_range",
    "weighted_length"
  )

  expect_true(
    all(required %in% names(pb$branch_ranges$table))
  )

  invisible(pb)

}

# -----------------------------------------------------------------------------
# Metrics
# -----------------------------------------------------------------------------

expect_valid_metrics <- function(pb) {

  expect_true(is.list(pb$metrics))

  invisible(pb)

}
