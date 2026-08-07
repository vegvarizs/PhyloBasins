# =============================================================================
# BalkanFishes validation
#
# 03_compare_balkan_results.R
#
# Compare PhyloBasins results with the original BalkanFishes reference data.
# =============================================================================

# -----------------------------------------------------------------------------
# Configuration
# -----------------------------------------------------------------------------

source("config.R")

# -----------------------------------------------------------------------------
# Compare one metric against the reference dataset
# -----------------------------------------------------------------------------

compare_metric <- function(
    pb,
    reference,
    metric,
    reference_column,
    id_column = "HYBAS_ID",
    tolerance = 1e-10
) {
  
  if (!reference_column %in% names(reference)) {
    
    stop(
      sprintf(
        "Reference column '%s' not found.",
        reference_column
      ),
      call. = FALSE
    )
    
  }
  
  current <- metric_table(
    pb,
    metric = metric
  )
  
  names(current) <- c(
    id_column,
    "current"
  )
  
  reference[[id_column]] <-
    as.character(reference[[id_column]])
  
  current[[id_column]] <-
    as.character(current[[id_column]])
  
  comparison <- merge(
    reference,
    current,
    by = id_column,
    all.x = TRUE,
    sort = FALSE
  )
  
  comparison$difference <-
    comparison$current -
    comparison[[reference_column]]
  
  comparison$abs_difference <-
    abs(comparison$difference)
  
  max_diff <- suppressWarnings(
    max(
      comparison$abs_difference,
      na.rm = TRUE
    )
  )
  
  if (is.infinite(max_diff)) {
    
    max_diff <- NA_real_
    
  }
  
  summary <- data.frame(
    
    Metric = metric,
    
    Rows = nrow(comparison),
    
    Mean_abs_difference =
      mean(
        comparison$abs_difference,
        na.rm = TRUE
      ),
    
    Max_abs_difference =
      max_diff,
    
    RMSE =
      sqrt(
        mean(
          comparison$difference^2,
          na.rm = TRUE
        )
      ),
    
    Pearson =
      suppressWarnings(
        cor(
          comparison[[reference_column]],
          comparison$current,
          use = "complete.obs",
          method = "pearson"
        )
      ),
    
    Spearman =
      suppressWarnings(
        cor(
          comparison[[reference_column]],
          comparison$current,
          use = "complete.obs",
          method = "spearman"
        )
      ),
    
    Missing_reference =
      sum(
        is.na(
          comparison[[reference_column]]
        )
      ),
    
    Missing_current =
      sum(
        is.na(
          comparison$current
        )
      ),
    
    Exact_matches =
      sum(
        comparison$abs_difference <= tolerance,
        na.rm = TRUE
      ),
    
    Exact_match_percent =
      100 *
      mean(
        comparison$abs_difference <= tolerance,
        na.rm = TRUE
      ),
    
    PASS =
      isTRUE(
        max_diff <= tolerance
      ),
    
    stringsAsFactors = FALSE
    
  )
  
  list(
    
    summary = summary,
    
    details = comparison
    
  )
  
}
start_time <- Sys.time()

cat("\n")
cat("============================================================\n")
cat(" BalkanFishes validation\n")
cat(" Step 3 of 6 : Compare reference results\n")
cat("============================================================\n\n")

# -----------------------------------------------------------------------------
# Load data
# -----------------------------------------------------------------------------

cat("Loading project...\n")

pb <- readRDS(
  pb_project_file
)

cat("Loading reference data...\n")

reference <- read.csv(
  file.path(
    project_dir,
    "inst",
    "extdata",
    "balkanfishes_reference",
    "fish_richness_turnover.csv"
  ),
  stringsAsFactors = FALSE
)

# -----------------------------------------------------------------------------
# Compare richness
# -----------------------------------------------------------------------------

cat("Comparing richness...\n")

richness <- compare_metric(
  
  pb = pb,
  
  reference = reference,
  
  metric = "richness",
  
  reference_column = "richness"
  
)

# -----------------------------------------------------------------------------
# Compare turnover
# -----------------------------------------------------------------------------

cat("Comparing turnover...\n")

turnover <- compare_metric(
  
  pb = pb,
  
  reference = reference,
  
  metric = "turnover",
  
  reference_column = "turnover_mean"
  
)

# -----------------------------------------------------------------------------
# Summary table
# -----------------------------------------------------------------------------

summary_table <- rbind(
  
  richness$summary,
  
  turnover$summary
  
)

# -----------------------------------------------------------------------------
# Output directory
# -----------------------------------------------------------------------------

comparison_dir <- file.path(
  output_dir,
  "comparison"
)

if (!dir.exists(comparison_dir)) {
  
  dir.create(
    comparison_dir,
    recursive = TRUE
  )
  
}

# -----------------------------------------------------------------------------
# Export
# -----------------------------------------------------------------------------

write.csv(
  
  summary_table,
  
  file.path(
    comparison_dir,
    "comparison_summary.csv"
  ),
  
  row.names = FALSE
  
)

write.csv(
  
  richness$details,
  
  file.path(
    comparison_dir,
    "richness_comparison.csv"
  ),
  
  row.names = FALSE
  
)

write.csv(
  
  turnover$details,
  
  file.path(
    comparison_dir,
    "turnover_comparison.csv"
  ),
  
  row.names = FALSE
  
)

# -----------------------------------------------------------------------------
# Check phylogenetic metrics
# -----------------------------------------------------------------------------

cat("Checking phylogenetic metrics...\n")

pd <- metric_table(pb, "pd")
pe <- metric_table(pb, "pe")
rpe <- metric_table(pb, "rpe")

phylo_summary <- data.frame(
  
  Metric = c(
    "PD",
    "PE",
    "RPE"
  ),
  
  Rows = c(
    nrow(pd),
    nrow(pe),
    nrow(rpe)
  ),
  
  Missing = c(
    sum(is.na(pd[[2]])),
    sum(is.na(pe[[2]])),
    sum(is.na(rpe[[2]]))
  ),
  
  stringsAsFactors = FALSE
  
)

write.csv(
  
  phylo_summary,
  
  file.path(
    comparison_dir,
    "phylogenetic_metric_summary.csv"
  ),
  
  row.names = FALSE
  
)

# -----------------------------------------------------------------------------
# Finish
# -----------------------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat(" Comparison completed\n")
cat("============================================================\n\n")

print(summary_table)

if (all(summary_table$PASS)) {
  
  cat(
    "\nRichness and turnover reproduced successfully.\n"
  )
  
}

cat("\n")

print(phylo_summary)

cat("\nDone.\n")

if (!all(summary_table$PASS)) {
  
  warning(
    "One or more metrics differ from the reference dataset."
  )
  
} else {
  
  cat(
    "\nAll numerical comparisons passed.\n"
  )
  
}

elapsed <- difftime(
  Sys.time(),
  start_time,
  units = "secs"
)

cat(sprintf(
  "\nElapsed time: %.1f seconds\n",
  as.numeric(elapsed)
))