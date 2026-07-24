reference_branch_engine <- function(tree, branches, community) {

  validate_tree(tree)
  validate_branches(branches)
  validate_community(community)

  if (!tree$prepared)
    stop("Tree must be prepared.", call. = FALSE)

  if (!branches$prepared)
    stop("Branches must be prepared.", call. = FALSE)

  if (!community$prepared)
    stop("Community must be prepared.", call. = FALSE)

  branch_table <- branches$table

  community_matrix <- community$matrix

  n_sites <- nrow(community_matrix)
  n_branches <- nrow(branch_table)

  sb <- matrix(
    FALSE,
    nrow = n_sites,
    ncol = n_branches
  )

  rownames(sb) <- rownames(community_matrix)
  colnames(sb) <- branch_table$branch_id

  for (b in seq_len(n_branches)) {

    descendants <- branch_table$descendant_species[[b]]

    idx <- match(
      descendants,
      colnames(community_matrix)
    )

    idx <- idx[!is.na(idx)]

    if (length(idx) == 0)
      next

    sb[, b] <-
      rowSums(
        community_matrix[, idx, drop = FALSE]
      ) > 0

  }

  out <- new_site_branch_matrix(

    matrix = Matrix::Matrix(
      sb,
      sparse = TRUE
    ),

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
