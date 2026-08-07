# =============================================================================
# BalkanFishes validation
#
# 01_build_project.R
#
# Build the complete PhyloBasins project from the original
# BalkanFishes tree and geometry.
# =============================================================================

source("config.R")


cat("\n")
cat("============================================================\n")
cat(" BalkanFishes validation\n")
cat(" Step 1 of 6 : Build project\n")
cat("============================================================\n\n")

start_time <- Sys.time()

# -----------------------------------------------------------------------------
# Create project
# -----------------------------------------------------------------------------

pb <- pb_project()

# -----------------------------------------------------------------------------
# Tree
# -----------------------------------------------------------------------------

pb <- read_tree(
  pb,
  file = tree_file,
  verbose = TRUE
)

pb <- validate_tree_data(
  pb,
  verbose = TRUE
)

if (isTRUE(pb$tree$validation$valid)) {
  
  pb <- prepare_tree(
    pb,
    verbose = TRUE
  )
  
} else {
  
  pb <- prepare_tree(
    pb,
    verbose = TRUE,
    force = TRUE
  )
  
}

if (isTRUE(pb$tree$validation$valid)) {
  
  pb <- prepare_tree(
    pb,
    verbose = TRUE
  )
  
} else {
  
  pb <- prepare_tree(
    pb,
    verbose = TRUE,
    force = TRUE
  )
  
}

pb <- build_branch_table(
  pb,
  verbose = TRUE
)

# -----------------------------------------------------------------------------
# Geometry
# -----------------------------------------------------------------------------

pb <- read_geometry(
  pb,
  file = shape_file,
  verbose = TRUE
)

pb <- build_community_from_geometry(
  pb,
  first_species = first_species,
  last_species = last_species,
  site_id = geometry_id_column,
  verbose = TRUE
)

pb <- prepare_geometry(
  pb,
  geometry_id = geometry_id_column,
  verbose = TRUE
)

# -----------------------------------------------------------------------------
# Site-branch matrix
# -----------------------------------------------------------------------------

pb <- build_site_branch_matrix(
  pb,
  verbose = TRUE
)

# -----------------------------------------------------------------------------
# Save project
# -----------------------------------------------------------------------------

saveRDS(
  pb,
  pb_project_file
)

# -----------------------------------------------------------------------------
# Summary
# -----------------------------------------------------------------------------

elapsed <- difftime(
  Sys.time(),
  start_time,
  units = "secs"
)

cat("\n")
cat("============================================================\n")
cat(" Project successfully created\n")
cat("============================================================\n\n")

cat(sprintf(
  "%-24s %d\n",
  "Tree tips:",
  length(pb$tree$phy$tip.label)
))

cat(sprintf(
  "%-24s %d\n",
  "Branches:",
  nrow(pb$branches$table)
))

cat(sprintf(
  "%-24s %d\n",
  "Communities:",
  nrow(pb$community$matrix)
))

cat(sprintf(
  "%-24s %d\n",
  "Species:",
  ncol(pb$community$matrix)
))

cat(sprintf(
  "%-24s %d\n",
  "Geometry features:",
  nrow(pb$geometry$sf)
))

cat(sprintf(
  "%-24s %s\n",
  "Output:",
  pb_project_file
))

cat(sprintf(
  "%-24s %.1f seconds\n",
  "Elapsed time:",
  as.numeric(elapsed)
))

cat("\nDone.\n")