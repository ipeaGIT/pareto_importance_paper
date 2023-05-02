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
  tar_target(rio_grid, "data-raw/rio_grid.rds", format = "file"),
  tar_target(od_points, generate_od_points(rio_grid))
)