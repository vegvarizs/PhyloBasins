# =============================================================================
# PhyloBasins
#
# Reproduce Balkan fishes phylogenetic diversity analyses
#
# This script demonstrates the complete PhyloBasins workflow using the
# Balkan freshwater fish dataset.
# =============================================================================

if (!requireNamespace("PhyloBasins", quietly = TRUE)) {
  stop("Please install PhyloBasins first.")
}

library(PhyloBasins)

# =============================================================================
# Input files
# =============================================================================

tree_file <-
  "data/tree/Ranges_abbr.nex"

community_file <-
  "data/community/BalkanFishes_community.csv"

# Optional
catchment_shapefile <-
  "data/shapefiles/catchments.shp"

# =============================================================================
# Create project
# =============================================================================

pb <- pb_project()

# =============================================================================
# Read data
# =============================================================================

pb <- read_tree(
  pb,
  tree_file
)

pb <- read_community(
  pb,
  community_file
)

# =============================================================================
# Prepare tree
# =============================================================================

pb <- prepare_tree(
  pb
)

# =============================================================================
# Build branch representation
# =============================================================================

pb <- build_branch_table(
  pb
)

pb <- build_site_branch_matrix(
  pb
)

# =============================================================================
# Compute all diversity metrics
# =============================================================================

pb <- compute_all_metrics(
  pb
)

# =============================================================================
# Collect results
# =============================================================================

results <- data.frame(

  Site = rownames(pb$community$matrix),

  PD =
    pb$metrics$pd$values,

  PE =
    pb$metrics$pe$values,

  RPE =
    pb$metrics$rpe$values,

  stringsAsFactors = FALSE

)

# =============================================================================
# Export table
# =============================================================================

write.csv(

  results,

  "BalkanFishes_PD_PE_RPE.csv",

  row.names = FALSE

)

# =============================================================================
# Save project
# =============================================================================

saveRDS(

  pb,

  "BalkanFishes.pb.rds"

)

# =============================================================================
# Summary
# =============================================================================

cat("\n")
cat("=========================================\n")
cat("PhyloBasins analysis completed.\n")
cat("=========================================\n\n")

cat("Sites:    ", nrow(pb$community$matrix), "\n")
cat("Species:  ", ncol(pb$community$matrix), "\n")
cat("Branches: ", nrow(pb$branches$table), "\n\n")

summary(results)
