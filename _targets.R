options(
  java.parameters = "-Xmx50G",
  N_CORES = 30L,
  SHOW_R5R_PROGRESS = TRUE
)

suppressPackageStartupMessages({
  library(targets)
  library(sf)
})

source("R/1_calculate_matrix.R", encoding = "UTF-8")

list(
  # data targets
  tar_target(rio_grid, "data-raw/rio_grid.rds", format = "file"),
  tar_target(rio_fare_structure, "data-raw/rio_fares.zip", format = "file"),
  
  # code-generated targets
  tar_target(od_points, generate_od_points(rio_grid))
)