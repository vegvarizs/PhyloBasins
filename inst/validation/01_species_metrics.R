# ============================================================
# PhyloBasins
#
# Validation workflow
#
# Step 1
# Species richness and mean Jaccard turnover
#
# Input
#   catchments shapefile
#
# Output
#   output/species_metrics.csv
#   output/catchments_species_metrics.*
#   figures/Fish_species_richness.png
#   figures/Fish_turnover.png
#
# ============================================================

library(sf)
library(dplyr)
library(vegan)
library(ggplot2)
library(viridis)

# ------------------------------------------------------------
# User settings
# ------------------------------------------------------------

args <- commandArgs(trailingOnly = TRUE)

if (length(args) == 0) {

  shape_file <- "catchments_lv8_danube_balkans_fish.shp"

} else {

  shape_file <- args[1]

}

output_dir <- "output"
figure_dir <- "figures"

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  figure_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

# ------------------------------------------------------------
# Read shapefile
# ------------------------------------------------------------

cat("\nReading shapefile...\n")

fish.shp <- st_read(
  shape_file,
  quiet = TRUE
)

cat("Catchments:", nrow(fish.shp), "\n")

# ------------------------------------------------------------
# Species columns
# ------------------------------------------------------------

first_species <- "ABR_BRA"
last_species  <- "COR_SP"

stopifnot(first_species %in% names(fish.shp))
stopifnot(last_species %in% names(fish.shp))

sp.cols <-
  which(names(fish.shp) == first_species):
  which(names(fish.shp) == last_species)

species_names <- names(fish.shp)[sp.cols]

cat("Species:", length(species_names), "\n")

# ------------------------------------------------------------
# Community matrix
# ------------------------------------------------------------

comm <-
  st_drop_geometry(fish.shp)[, sp.cols]

comm <- as.matrix(comm)

storage.mode(comm) <- "numeric"

# ------------------------------------------------------------
# Species richness
# ------------------------------------------------------------

cat("\nComputing species richness...\n")

fish.shp$richness <-
  rowSums(
    comm,
    na.rm = TRUE
  )

print(summary(fish.shp$richness))

# ------------------------------------------------------------
# Mean Jaccard turnover
# ------------------------------------------------------------

cat("\nComputing mean Jaccard turnover...\n")

jac.dist <-
  vegdist(
    comm,
    method = "jaccard",
    binary = TRUE
  )

jac.mat <- as.matrix(jac.dist)

fish.shp$turnover_mean <-
  sapply(
    seq_len(nrow(jac.mat)),
    function(i)
      mean(
        jac.mat[i, -i],
        na.rm = TRUE
      )
  )

print(summary(fish.shp$turnover_mean))

# ------------------------------------------------------------
# Export table
# ------------------------------------------------------------

cat("\nWriting table...\n")

out.tab <-
  fish.shp |>
  st_drop_geometry() |>
  dplyr::select(
    HYBAS_ID,
    richness,
    turnover_mean
  )

write.csv(
  out.tab,
  file.path(
    output_dir,
    "species_metrics.csv"
  ),
  row.names = FALSE
)

# ------------------------------------------------------------
# Export shapefile
# ------------------------------------------------------------

cat("Writing shapefile...\n")

st_write(
  fish.shp,
  file.path(
    output_dir,
    "catchments_species_metrics.shp"
  ),
  delete_layer = TRUE,
  quiet = TRUE
)

# ------------------------------------------------------------
# Richness map
# ------------------------------------------------------------

cat("Drawing richness map...\n")

p.rich <-
  ggplot(fish.shp) +
  geom_sf(
    aes(fill = richness),
    colour = NA
  ) +
  scale_fill_viridis_c(
    option = "viridis",
    name = "Species richness"
  ) +
  coord_sf() +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )

ggsave(
  filename = file.path(
    figure_dir,
    "Fish_species_richness.png"
  ),
  plot = p.rich,
  width = 10,
  height = 8,
  dpi = 600
)

# ------------------------------------------------------------
# Turnover map
# ------------------------------------------------------------

cat("Drawing turnover map...\n")

p.turn <-
  ggplot(fish.shp) +
  geom_sf(
    aes(fill = turnover_mean),
    colour = NA
  ) +
  scale_fill_viridis_c(
    option = "viridis",
    name = "Mean Jaccard\nturnover"
  ) +
  coord_sf() +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  )

ggsave(
  filename = file.path(
    figure_dir,
    "Fish_turnover.png"
  ),
  plot = p.turn,
  width = 10,
  height = 8,
  dpi = 600
)

# ------------------------------------------------------------
# Summary
# ------------------------------------------------------------

cat("\n=========================================\n")
cat("Validation Step 1 completed successfully.\n")
cat("=========================================\n\n")

cat("Output tables:\n")
cat(normalizePath(output_dir), "\n\n")

cat("Figures:\n")
cat(normalizePath(figure_dir), "\n\n")

cat("Generated files:\n")
cat("  species_metrics.csv\n")
cat("  catchments_species_metrics.shp\n")
cat("  Fish_species_richness.png\n")
cat("  Fish_turnover.png\n\n")
