# =============================================================================
# 06_reproduce_balkanfishes.R
#
# Complete reproducible BalkanFishes validation workflow.
#
# This script executes the full validation pipeline:
#
#   01_species_metrics.R
#   02_phylogenetic_metrics.R
#   03_compare_balkan_results.R
#   04_create_validation_maps.R
#   05_validation_report.R
#
# =============================================================================

cat("\n")
cat("============================================================\n")
cat("PhyloBasins\n")
cat("Complete BalkanFishes validation workflow\n")
cat("============================================================\n\n")

start.time <- Sys.time()

scripts.dir <- "scripts"

scripts <-
  
  c(
    
    "01_species_metrics.R",
    
    "02_phylogenetic_metrics.R",
    
    "03_compare_balkan_results.R",
    
    "04_create_validation_maps.R",
    
    "05_validation_report.R"
    
  )

cat("Scripts to execute:\n\n")

for(i in seq_along(scripts)){
  
  cat(sprintf(
    "  %d. %s\n",
    i,
    scripts[i]
  ))
  
}

cat("\n")

# -------------------------------------------------------------------------
# Execute validation scripts
# -------------------------------------------------------------------------

for(script in scripts){
  
  script.path <-
    
    file.path(
      scripts.dir,
      script
    )
  
  cat(
    "------------------------------------------------------------\n"
  )
  
  cat(
    "Running:",
    script,
    "\n\n"
  )
  
  if(!file.exists(script.path)){
    
    stop(
      
      paste(
        
        "Validation script not found:",
        
        script.path
        
      ),
      
      call. = FALSE
      
    )
    
  }
  
  source(
    script.path,
    local = FALSE
  )
  
  cat("\n")
  
}

# -------------------------------------------------------------------------
# Final report
# -------------------------------------------------------------------------

report.file <-
  
  file.path(
    
    scripts.dir,
    
    "output",
    
    "validation_report.txt"
    
  )

cat(
  "============================================================\n"
)

if(file.exists(report.file)){
  
  cat(
    "Validation successfully completed.\n\n"
  )
  
  cat(
    "Summary report:\n\n"
  )
  
  cat(
    paste(
      readLines(report.file),
      collapse = "\n"
    )
  )
  
}else{
  
  warning(
    
    "Validation report not found."
    
  )
  
}

cat("\n\n")

elapsed <-
  
  difftime(
    
    Sys.time(),
    
    start.time,
    
    units = "secs"
    
  )

cat(
  "============================================================\n"
)

cat(
  sprintf(
    "Workflow finished successfully in %.1f seconds.\n",
    as.numeric(elapsed)
  )
)

cat(
  "============================================================\n\n"
)