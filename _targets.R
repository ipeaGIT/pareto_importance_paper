options(
  java.parameters = "-Xmx50G",
  N_CORES = 30L,
  SHOW_R5R_PROGRESS = TRUE,
  VERBOSE_R5R = FALSE,
  CALCULATE_FRONTIER = TRUE
)

suppressPackageStartupMessages({
  library(targets)
  library(sf)
  library(ggplot2)
})

source("R/1_calculate_matrix.R", encoding = "UTF-8")
source("R/2_calculate_accessibility.R", encoding = "UTF-8")
source("R/3_analysis.R", encoding = "UTF-8")

list(
  # data targets ----
  tar_target(rio_grid, "data-raw/rio_grid.rds", format = "file"),
  tar_target(rio_fare_structure, "data-raw/rio_fares.zip", format = "file"),
  tar_target(od_points, generate_od_points(rio_grid)),
  tar_target(
    gtfs_fetranspor_url,
    "https://github.com/ipeaGIT/pareto_importance_paper/releases/download/data/gtfs_fetranspor.zip",
    format = "url"
  ),
  tar_target(
    gtfs_supervia_url,
    "https://github.com/ipeaGIT/pareto_importance_paper/releases/download/data/gtfs_supervia.zip",
    format = "url"
  ),
  tar_target(
    rio_street_network_url,
    "https://github.com/ipeaGIT/pareto_importance_paper/releases/download/data/rio_street_network.osm.pbf",
    format = "url"
  ),
  tar_target(
    rio_topography_url,
    "https://github.com/ipeaGIT/pareto_importance_paper/releases/download/data/rio_topography.tif",
    format = "url"
  ),
  
  # code-generated targets ----
  tar_target(
    routing_dir,
    build_transport_network(
      gtfs_fetranspor_url,
      gtfs_supervia_url,
      rio_street_network_url,
      rio_topography_url
    ),
    format = "file"
  ),
  tar_target(
    frontier,
    calculate_frontier(routing_dir, od_points, rio_fare_structure)
  ),
  tar_target(adjusted_income, adjust_grid_income(rio_grid)),
  tar_target(
    frontier_with_afford,
    calculate_affordability_frontier(frontier, adjusted_income)
  ),
  tar_target(
    absolute_accessibility,
    calculate_accessibility(
      frontier_with_afford,
      rio_grid,
      od_points,
      "absolute"
    )
  ),
  tar_target(
    affordability_accessibility,
    calculate_accessibility(
      frontier_with_afford,
      rio_grid,
      od_points,
      "affordability"
    )
  ),
  
  # analysis targets ----
  tar_target(
    absolute_map,
    create_absolute_map(absolute_accessibility, rio_grid)
  ),
  tar_target(heatmap_theme, create_heatmap_theme()),
  tar_target(
    absolute_heatmap_no_cost,
    create_absolute_heatmap(
      absolute_accessibility,
      rio_grid,
      heatmap_theme,
      "free_fastest"
    )
  ),
  tar_target(
    absolute_heatmap_fastest_cost,
    create_absolute_heatmap(
      absolute_accessibility,
      rio_grid,
      heatmap_theme,
      "fastest_cost"
    )
  ),
  tar_target(
    affordability_heatmap_no_cost,
    create_affordability_heatmap(
      affordability_accessibility,
      rio_grid,
      heatmap_theme,
      "free_fastest"
    )
  ),
  tar_target(
    affordability_heatmap_fastest_cost,
    create_affordability_heatmap(
      affordability_accessibility,
      rio_grid,
      heatmap_theme,
      "fastest_cost"
    )
  ),
  tar_target(
    affordability_per_group_heatmap_no_cost,
    create_afford_per_group_heatmap(
      affordability_accessibility,
      rio_grid,
      heatmap_theme,
      "free_fastest"
    )
  ),
  tar_target(
    affordability_per_group_heatmap_fastest_cost,
    create_afford_per_group_heatmap(
      affordability_accessibility,
      rio_grid,
      heatmap_theme,
      "fastest_cost"
    )
  ),
  tar_target(
    pareto_frontier_plot,
    create_frontier_plot(frontier_with_afford, routing_dir, od_points)
  ),
  
  # chart-saving targets ----
  tar_target(
    figures_dir,
    {
      dir <- "figures"
      dir.create(dir)
      dir
    }
  ),
  tar_target(
    paper_absolute_map,
    ggsave(
      file.path(figures_dir, "paper_absolute_map.png"),
      plot = absolute_map,
      units = "cm",
      width = 24,
      height = 16
    ),
    format = "file"
  ),
  tar_target(
    paper_absolute_heatmap_no_cost,
    ggsave(
      file.path(figures_dir, "paper_absolute_heatmap_no_cost.png"),
      plot = absolute_heatmap_no_cost,
      units = "cm",
      width = 24,
      height = 9
    ),
    format = "file"
  ),
  tar_target(
    paper_absolute_heatmap_fastest_cost,
    ggsave(
      file.path(figures_dir, "paper_absolute_heatmap_fastest_cost.png"),
      plot = absolute_heatmap_fastest_cost,
      units = "cm",
      width = 24,
      height = 9
    ),
    format = "file"
  ),
  tar_target(
    paper_affordability_heatmap_no_cost,
    ggsave(
      file.path(figures_dir, "paper_affordability_heatmap_no_cost.png"),
      plot = affordability_heatmap_no_cost,
      units = "cm",
      width = 24,
      height = 9
    ),
    format = "file"
  ),
  tar_target(
    paper_affordability_heatmap_fastest_cost,
    ggsave(
      file.path(figures_dir, "paper_affordability_heatmap_fastest_cost.png"),
      plot = affordability_heatmap_fastest_cost,
      units = "cm",
      width = 24,
      height = 9
    ),
    format = "file"
  ),
  tar_target(
    paper_affordability_per_group_heatmap_no_cost,
    ggsave(
      file.path(figures_dir, "paper_affordability_per_group_heatmap_no_cost.png"),
      plot = affordability_per_group_heatmap_no_cost,
      units = "cm",
      width = 24,
      height = 13
    ),
    format = "file"
  ),
  tar_target(
    paper_affordability_per_group_heatmap_fastest_cost,
    ggsave(
      file.path(figures_dir, "paper_affordability_per_group_heatmap_fastest_cost.png"),
      plot = affordability_per_group_heatmap_fastest_cost,
      units = "cm",
      width = 24,
      height = 13
    ),
    format = "file"
  ),
  tar_target(
    paper_pareto_frontier_plot,
    ggsave(
      file.path(figures_dir, "paper_pareto_frontier_plot.png"),
      plot = pareto_frontier_plot,
      units = "cm",
      width = 16,
      height = 8
    ),
    format = "file"
  )
)