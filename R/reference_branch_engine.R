# =============================================================================
# PhyloBasins
#
# Reference Branch Engine
#
# Builds the site × branch incidence matrix.
# =============================================================================

reference_branch_engine <- function(
    tree,
    branches,
    community
) {

  # ---------------------------------------------------------------------------
  # Validate inputs
  # ---------------------------------------------------------------------------

  validate_tree(tree)
  validate_branches(branches)
  validate_community(community)

  if (!isTRUE(tree$prepared)) {
    stop(
      "Tree must be prepared.",
      call. = FALSE
    )
  }

  if (!isTRUE(branches$prepared)) {
    stop(
      "Branch table must be prepared.",
      call. = FALSE
    )
  }

  if (!isTRUE(community$loaded)) {
    stop(
      "Community must be prepared.",
      call. = FALSE
    )
  }

  branch_table <- branches$table
  community_matrix <- community$matrix

  if (is.null(branch_table)) {
    stop(
      "Branch table is missing.",
      call. = FALSE
    )
  }

  if (is.null(community_matrix)) {
    stop(
      "Community matrix is missing.",
      call. = FALSE
    )
  }

  # ---------------------------------------------------------------------------
  # Descendant cache
  # ---------------------------------------------------------------------------

  descendants <- branches$cache$descendant_species

  if (is.null(descendants)) {
    stop(
      "Branch descendant cache is missing.",
      call. = FALSE
    )
  }

  if (length(descendants) != nrow(branch_table)) {
    stop(
      "Branch descendant cache is inconsistent.",
      call. = FALSE
    )
  }

  # ---------------------------------------------------------------------------
  # Allocate matrix
  # ---------------------------------------------------------------------------

  n_sites <- nrow(community_matrix)
  n_branches <- nrow(branch_table)

  sb <- matrix(
    FALSE,
    nrow = n_sites,
    ncol = n_branches
  )

  rownames(sb) <- rownames(community_matrix)
  colnames(sb) <- branch_table$branch_id

  taxa_names <- colnames(community_matrix)

  # ---------------------------------------------------------------------------
  # Fill matrix
  # ---------------------------------------------------------------------------

  for (b in seq_len(n_branches)) {

    taxa <- descendants[[b]]

    idx <- match(
      taxa,
      taxa_names
    )

    idx <- idx[!is.na(idx)]

    if (length(idx) == 0)
      next

    sb[, b] <-
      rowSums(
        community_matrix[, idx, drop = FALSE]
      ) > 0

  }

  # ---------------------------------------------------------------------------
  # Convert to sparse matrix
  # ---------------------------------------------------------------------------

  sb <- Matrix::Matrix(
    sb,
    sparse = TRUE
  )

  # ---------------------------------------------------------------------------
  # Build object
  # ---------------------------------------------------------------------------

  out <- pb_site_branch_matrix(

    matrix = sb,

    sites = rownames(sb),

    branches = colnames(sb),

    sparse = TRUE,

    built = TRUE,

    validation = list(

      valid = TRUE,

      reference = TRUE

    )

  )

  validate_site_branch_matrix(out)

  out

}
