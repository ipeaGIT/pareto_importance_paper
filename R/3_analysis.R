# access <- tar_read(absolute_accessibility)
# grid_path <- tar_read(rio_grid)
create_absolute_heatmap <- function(access, grid_path) {
  grid <- readRDS(grid_path)
  
  access[grid, on = "id", population := i.population]
  access <- access[
    ,
    .(avg_access = weighted.mean(access, w = population)),
    keyby = .(method, travel_time, cost_cutoff = absolute)
  ]
  
  total_jobs <- sum(grid$jobs)
  
  # remove grid object to reduce plot object size
  rm(grid)
  
  distribution <- ggplot(access) + 
    geom_tile(aes(travel_time, cost_cutoff, fill = avg_access)) +
    facet_wrap(~ method) +
    scale_fill_viridis_c(
      name = "Average\naccessibility\n(% of total jobs)",
      labels = scales::label_percent(accuracy = 1, scale = 100 / total_jobs)
    ) +
    scale_x_continuous(
      name = "Travel time threshold",
      breaks = c(0, 30, 60, 90)
    ) +
    scale_y_continuous(
      name = "Absolute monetary cost threshold (BRL)",
      breaks = c(0, 5, 10, 15)
    ) +
    theme_minimal() +
    theme(panel.grid = element_blank(), legend.position = "right")
  
  access_diff <- access[method != "free_fastest"]
  access_diff[
    ,
    diff := avg_access - data.table::shift(avg_access),
    by = .(travel_time, cost_cutoff)
  ]
  access_diff <- access_diff[!is.na(diff)]
  access_diff[, `:=`(avg_access = NULL, method = "difference")]
  
  # remove access object to reduce plot object size
  rm(access)
  
  difference <- ggplot(access_diff) + 
    geom_tile(aes(travel_time, cost_cutoff, fill = diff)) +
    facet_wrap(~ method) +
    scale_fill_viridis_c(
      name = "Accessibility\ndifference\n(% of total jobs)",
      labels = scales::label_percent(accuracy = 1, scale = 100 / total_jobs),
      option = "inferno"
    ) +
    scale_x_continuous(
      name = "Travel time threshold",
      breaks = c(0, 30, 60, 90)
    ) +
    scale_y_continuous(
      name = "Absolute monetary cost threshold (BRL)",
      breaks = c(0, 5, 10, 15)
    ) +
    theme_minimal() +
    theme(panel.grid = element_blank(), legend.position = "right")
  
  # remove access_diff object to reduce plot object size
  rm(access_diff)
  
  p <- cowplot::plot_grid(
    distribution,
    difference,
    nrow = 1,
    rel_widths = c(3, 1)
  )
  
  p
}