# =============================================================================
# BalkanFishes validation
#
# 02_compute_metrics.R
#
# Compute all implemented diversity metrics.
# =============================================================================

# -----------------------------------------------------------------------------
# Configuration
# -----------------------------------------------------------------------------

source("config.R")

# -----------------------------------------------------------------------------
# Start
# -----------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" BalkanFishes validation\n")
cat(" Step 2 of 6 : Compute metrics\n")
cat("============================================================\n\n")

start_time <- Sys.time()

# -----------------------------------------------------------------------------
# Output directory
# -----------------------------------------------------------------------------

metrics_dir <- file.path(
  output_dir,
  "metrics"
)

if (!dir.exists(metrics_dir)) {
  
  dir.create(
    metrics_dir,
    recursive = TRUE
  )
  
}

# -----------------------------------------------------------------------------
# Load project
# -----------------------------------------------------------------------------

cat("Loading project...\n")

pb <- readRDS(pb_project_file)

# -----------------------------------------------------------------------------
# Compute richness
# -----------------------------------------------------------------------------

cat("Computing species richness...\n")

pb <- compute_richness(
  pb,
  verbose = TRUE
)

# -----------------------------------------------------------------------------
# Compute turnover
# -----------------------------------------------------------------------------

cat("Computing community turnover...\n")

pb <- compute_turnover(
  pb,
  verbose = TRUE
)

# -----------------------------------------------------------------------------
# Compute branch ranges
# -----------------------------------------------------------------------------

cat("Computing branch ranges...\n")

pb <- compute_branch_ranges(
  pb,
  verbose = TRUE
)

# -----------------------------------------------------------------------------
# Compute PD
# -----------------------------------------------------------------------------

cat("Computing Faith's PD...\n")

pb <- compute_pd(
  pb,
  verbose = TRUE
)

# -----------------------------------------------------------------------------
# Compute PE
# -----------------------------------------------------------------------------

cat("Computing Phylogenetic Endemism...\n")

pb <- compute_pe(
  pb,
  verbose = TRUE
)

# -----------------------------------------------------------------------------
# Compute RPE
# -----------------------------------------------------------------------------

cat("Computing Relative Phylogenetic Endemism...\n")

pb <- compute_rpe(
  pb,
  verbose = TRUE
)

# -----------------------------------------------------------------------------
# Update validation metadata
# -----------------------------------------------------------------------------

validation <- attr(pb, "validation")

if (is.null(validation)) {
  
  validation <- list()
  
}

validation$metrics_completed <- TRUE
validation$metrics_created <- Sys.time()

attr(pb, "validation") <- validation

# -----------------------------------------------------------------------------
# Save updated project
# -----------------------------------------------------------------------------

cat("Saving project...\n")

saveRDS(
  pb,
  pb_project_file
)

# -----------------------------------------------------------------------------
# Export metric tables
# -----------------------------------------------------------------------------

cat("Exporting metric tables...\n")

richness_table <- metric_table(
  pb,
  metric = "richness"
)

turnover_table <- metric_table(
  pb,
  metric = "turnover"
)

pd_table <- metric_table(
  pb,
  metric = "pd"
)

pe_table <- metric_table(
  pb,
  metric = "pe"
)

rpe_table <- metric_table(
  pb,
  metric = "rpe"
)

write.csv(
  richness_table,
  file.path(metrics_dir, "richness.csv"),
  row.names = FALSE
)

write.csv(
  turnover_table,
  file.path(metrics_dir, "turnover.csv"),
  row.names = FALSE
)

write.csv(
  pd_table,
  file.path(metrics_dir, "pd.csv"),
  row.names = FALSE
)

write.csv(
  pe_table,
  file.path(metrics_dir, "pe.csv"),
  row.names = FALSE
)

write.csv(
  rpe_table,
  file.path(metrics_dir, "rpe.csv"),
  row.names = FALSE
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
cat(" Metric computation completed\n")
cat("============================================================\n\n")

cat(sprintf(
  "%-30s %d\n",
  "Sites:",
  nrow(richness_table)
))

cat(sprintf(
  "%-30s %d\n",
  "Richness values:",
  nrow(richness_table)
))

cat(sprintf(
  "%-30s %d\n",
  "Turnover values:",
  nrow(turnover_table)
))

cat(sprintf(
  "%-30s %d\n",
  "PD values:",
  nrow(pd_table)
))

cat(sprintf(
  "%-30s %d\n",
  "PE values:",
  nrow(pe_table)
))

cat(sprintf(
  "%-30s %d\n",
  "RPE values:",
  nrow(rpe_table)
))

cat(sprintf(
  "%-30s %s\n",
  "Project:",
  pb_project_file
))

cat(sprintf(
  "%-30s %s\n",
  "Richness table:",
  file.path(metrics_dir, "richness.csv")
))

cat(sprintf(
  "%-30s %s\n",
  "Turnover table:",
  file.path(metrics_dir, "turnover.csv")
))

cat(sprintf(
  "%-30s %s\n",
  "PD table:",
  file.path(metrics_dir, "pd.csv")
))

cat(sprintf(
  "%-30s %s\n",
  "PE table:",
  file.path(metrics_dir, "pe.csv")
))

cat(sprintf(
  "%-30s %s\n",
  "RPE table:",
  file.path(metrics_dir, "rpe.csv")
))

cat(sprintf(
  "%-30s %.1f seconds\n",
  "Elapsed time:",
  as.numeric(elapsed)
))

cat("\nDone.\n")