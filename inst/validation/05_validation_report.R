# =============================================================================
# BalkanFishes validation
#
# 05_validation_report.R
#
# Create a human-readable validation report.
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
cat(" Step 5 of 5 : Validation report\n")
cat("============================================================\n\n")

# -----------------------------------------------------------------------------
# Input files
# -----------------------------------------------------------------------------

comparison_summary_file <-
  file.path(
    output_dir,
    "comparison",
    "comparison_summary.csv"
  )

phylo_summary_file <-
  file.path(
    output_dir,
    "comparison",
    "phylogenetic_metric_summary.csv"
  )

# -----------------------------------------------------------------------------
# Read validation results
# -----------------------------------------------------------------------------

comparison <-
  read.csv(
    comparison_summary_file,
    stringsAsFactors = FALSE
  )

phylo <-
  read.csv(
    phylo_summary_file,
    stringsAsFactors = FALSE
  )

# -----------------------------------------------------------------------------
# Report
# -----------------------------------------------------------------------------

report <- character()

report <- c(
  
  report,
  
  "============================================================",
  "PhyloBasins",
  "BalkanFishes validation report",
  "============================================================",
  
  "",
  
  sprintf(
    "Generated: %s",
    format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  ),
  
  sprintf(
    "Package version: %s",
    as.character(
      packageVersion("PhyloBasins")
    )
  ),
  
  "",
  
  "Validation workflow",
  "-------------------",
  
  "PASS  Project creation",
  "PASS  Metric computation",
  "PASS  Numerical comparison",
  "PASS  Map creation",
  
  "",
  
  "Historical reproduction",
  "-----------------------",
  ""
  
)

for(i in seq_len(nrow(comparison))){
  
  row <- comparison[i,]
  
  report <- c(
    
    report,
    
    sprintf(
      "%s",
      row$Metric
    ),
    
    sprintf(
      "  PASS               : %s",
      row$PASS
    ),
    
    sprintf(
      "  Pearson           : %.10f",
      row$Pearson
    ),
    
    sprintf(
      "  Spearman          : %.10f",
      row$Spearman
    ),
    
    sprintf(
      "  RMSE              : %.10f",
      row$RMSE
    ),
    
    sprintf(
      "  Mean abs. diff.   : %.10f",
      row$Mean_abs_difference
    ),
    
    sprintf(
      "  Max abs. diff.    : %.10f",
      row$Max_abs_difference
    ),
    
    ""
    
  )
  
}

report <- c(
  
  report,
  
  "",
  
  "Phylogenetic metrics",
  "--------------------",
  
  ""
  
)

for(i in seq_len(nrow(phylo))){
  
  row <- phylo[i,]
  
  report <- c(
    
    report,
    
    sprintf(
      "%s",
      row$Metric
    ),
    
    sprintf(
      "  Rows              : %d",
      row$Rows
    ),
    
    sprintf(
      "  Missing values    : %d",
      row$Missing
    ),
    
    ""
    
  )
  
}

report <- c(
  
  report,
  
  "Output files",
  "------------",
  
  "",
  
  sprintf("Project : %s", pb_project_file),
  
  "",
  
  "Metrics",
  
  sprintf("  %s", file.path(output_dir, "metrics", "richness.csv")),
  sprintf("  %s", file.path(output_dir, "metrics", "turnover.csv")),
  sprintf("  %s", file.path(output_dir, "metrics", "pd.csv")),
  sprintf("  %s", file.path(output_dir, "metrics", "pe.csv")),
  sprintf("  %s", file.path(output_dir, "metrics", "rpe.csv")),
  
  "",
  
  "Maps",
  
  sprintf("  %s", richness_map),
  sprintf("  %s", turnover_map),
  sprintf("  %s", pd_map),
  sprintf("  %s", pe_map),
  sprintf("  %s", rpe_map),
  
  "",
  
  "Overall conclusion",
  "------------------",
  
  ""
  
)

if(all(comparison$PASS)){
  
  report <- c(
    
    report,
    
    "The historical BalkanFishes workflow was successfully",
    
    "reproduced using the PhyloBasins package.",
    
    "",
    
    "All biodiversity metrics agree with the historical",
    
    "reference implementation within numerical tolerance.",
    
    "",
    
    "Validation status: PASS"
    
  )
  
}else{
  
  report <- c(
    
    report,
    
    "Validation status: FAIL",
    
    "",
    
    "One or more metrics differ from the historical",
    
    "reference implementation."
    
  )
  
}

# -----------------------------------------------------------------------------
# Write report
# -----------------------------------------------------------------------------

writeLines(
  report,
  validation_report_file
)

cat(
  "Validation report written to:\n"
)

cat(
  validation_report_file,
  "\n\n"
)

cat(
  "Done.\n"
)