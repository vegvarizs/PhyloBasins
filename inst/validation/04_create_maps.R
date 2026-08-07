# =============================================================================
# BalkanFishes validation
#
# 04_create_maps.R
#
# Create validation maps from the reproduced BalkanFishes project.
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
cat(" Step 4 of 6 : Create validation maps\n")
cat("============================================================\n\n")

start_time <- Sys.time()

# -----------------------------------------------------------------------------
# Load project
# -----------------------------------------------------------------------------

cat("Loading project...\n")

pb <- readRDS(
  pb_project_file
)

# -----------------------------------------------------------------------------
# Output directory
# -----------------------------------------------------------------------------

if (!dir.exists(plot_dir)) {
  
  dir.create(
    plot_dir,
    recursive = TRUE
  )
  
}

# -----------------------------------------------------------------------------
# Helper
# -----------------------------------------------------------------------------

save_metric_map <- function(
    metric,
    filename,
    title = NULL,
    width = 8,
    height = 6,
    dpi = 300
) {
  
  if (!isTRUE(pb$metrics[[metric]]$computed)) {
    
    stop(
      sprintf(
        "Metric '%s' has not been computed.",
        metric
      ),
      call. = FALSE
    )
    
  }
  
  cat(sprintf(
    "Creating %s map...\n",
    metric
  ))
  
  p <- plot_metric(
    pb,
    metric = metric,
    legend_title = title
  )
  
  ggplot2::ggsave(
    filename = filename,
    plot = p,
    width = width,
    height = height,
    dpi = dpi
  )
  
}

# -----------------------------------------------------------------------------
# Richness
# -----------------------------------------------------------------------------

save_metric_map(
  metric = "richness",
  filename = richness_map,
  title = "Species richness"
)

# -----------------------------------------------------------------------------
# Turnover
# -----------------------------------------------------------------------------

save_metric_map(
  metric = "turnover",
  filename = turnover_map,
  title = "Turnover"
)

# -----------------------------------------------------------------------------
# Faith's PD
# -----------------------------------------------------------------------------

save_metric_map(
  metric = "pd",
  filename = pd_map,
  title = "Faith's PD"
)

# -----------------------------------------------------------------------------
# Phylogenetic Endemism
# -----------------------------------------------------------------------------

save_metric_map(
  metric = "pe",
  filename = pe_map,
  title = "Phylogenetic Endemism"
)

# -----------------------------------------------------------------------------
# Relative Phylogenetic Endemism
# -----------------------------------------------------------------------------

save_metric_map(
  metric = "rpe",
  filename = rpe_map,
  title = "Relative Phylogenetic Endemism"
)

# -----------------------------------------------------------------------------
# Finish
# -----------------------------------------------------------------------------

elapsed <- difftime(
  Sys.time(),
  start_time,
  units = "secs"
)

cat("\n")
cat("============================================================\n")
cat(" Validation maps completed\n")
cat("============================================================\n\n")

cat(sprintf(
  "%-30s %s\n",
  "Richness:",
  richness_map
))

cat(sprintf(
  "%-30s %s\n",
  "Turnover:",
  turnover_map
))

cat(sprintf(
  "%-30s %s\n",
  "PD:",
  pd_map
))

cat(sprintf(
  "%-30s %s\n",
  "PE:",
  pe_map
))

cat(sprintf(
  "%-30s %s\n",
  "RPE:",
  rpe_map
))

cat(sprintf(
  "%-30s %.1f seconds\n",
  "Elapsed time:",
  as.numeric(elapsed)
))

cat("\nValidation workflow completed successfully.\n")