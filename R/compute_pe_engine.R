# =============================================================================
# PhyloBasins
#
# Internal engine for Phylogenetic Endemism (PE)
# =============================================================================

# -----------------------------------------------------------------------------
# Compute Phylogenetic Endemism
# -----------------------------------------------------------------------------

# Internal helper.
#
# Computes Phylogenetic Endemism from a site-by-branch matrix and a vector
# of branch contributions.
#
# Not exported.

compute_pe_engine <- function(
    site_branch_matrix,
    weighted_length,
    branch_weights = NULL
) {

  # ---------------------------------------------------------------------------
  # Input
  # ---------------------------------------------------------------------------

  if (inherits(site_branch_matrix, "Matrix")) {

    M <- site_branch_matrix

  } else {

    M <- as.matrix(site_branch_matrix)

  }

  n_branches <- ncol(M)

  if (length(weighted_length) != n_branches) {

    stop(
      "'weighted_length' must have one value per branch.",
      call. = FALSE
    )

  }

  if (is.null(branch_weights)) {

    branch_weights <- rep(1, n_branches)

  }

  if (length(branch_weights) != n_branches) {

    stop(
      "'branch_weights' must have one value per branch.",
      call. = FALSE
    )

  }

  # ---------------------------------------------------------------------------
  # Branch contribution
  # ---------------------------------------------------------------------------

  contribution <-
    weighted_length *
    branch_weights

  # ---------------------------------------------------------------------------
  # PE
  # ---------------------------------------------------------------------------

  pe <-

    if (inherits(M, "Matrix")) {

      as.numeric(M %*% contribution)

    } else {

      as.vector(M %*% contribution)

    }

  names(pe) <- rownames(M)

  pe

}
