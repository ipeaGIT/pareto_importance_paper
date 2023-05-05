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
  
  # remove grid object to reduce plot object size
  rm(grid)
  
  make_row <- function(to_compare = c("free_fastest", "fastest_cost")) {
    access_dist <- access[method %in% c(to_compare, "pareto_frontier")]
    
    distribution <- ggplot(access_dist) + 
      geom_tile(aes(travel_time, cost_cutoff, fill = avg_access)) +
      facet_wrap(~ method) +
      scale_fill_viridis_c(
        name = "Average\naccessibility\n",
        labels = scales::label_number(scale = 1 / 1000, suffix = "k")
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
    
    access_diff <- data.table::dcast(
      access_dist,
      travel_time + cost_cutoff ~ method,
      value.var = "avg_access"
    )
    access_diff[, diff := (pareto_frontier - get(to_compare)) / get(to_compare)]
    access_diff[is.nan(diff), diff := 0]
    access_diff[, c(to_compare, "pareto_frontier") := NULL]
    access_diff[, method := "difference"]
    
    # remove access_dist object to reduce plot object size
    rm(access_dist)
    
    difference <- ggplot(access_diff) + 
      geom_tile(aes(travel_time, cost_cutoff, fill = diff)) +
      facet_wrap(~ method) +
      scale_fill_viridis_c(
        name = "Accessibility\ndifference\n(% of original)",
        labels = scales::label_percent(),
        option = ifelse(to_compare == "free_fastest", "cividis", "inferno"),
        direction = ifelse(to_compare == "free_fastest", -1, 1)
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
      theme(
        panel.grid = element_blank(),
        legend.position = "right",
        axis.title.y = element_blank()
      )
    
    # remove access_diff object to reduce plot object size
    rm(access_diff)
    
    row <- cowplot::plot_grid(
      distribution,
      difference,
      nrow = 1,
      rel_widths = c(2, 1)
    )
    
    row
  }
  
  first_row <- make_row("free_fastest")
  second_row <- make_row("fastest_cost")
  
  p <- cowplot::plot_grid(first_row, second_row, nrow = 2)
  
  p
}

# access <- tar_read(affordability_accessibility)
# grid_path <- tar_read(rio_grid)
create_affordability_heatmap <- function(access, grid_path) {
  grid <- readRDS(grid_path)
  
  access[grid, on = "id", population := i.population]
  access <- access[
    ,
    .(avg_access = weighted.mean(access, w = population)),
    keyby = .(method, travel_time, cost_cutoff = affordability)
  ]
  
  # remove grid object to reduce plot object size
  rm(grid)
  
  make_row <- function(to_compare = c("free_fastest", "fastest_cost")) {
    access_dist <- access[method %in% c(to_compare, "pareto_frontier")]
    
    distribution <- ggplot(access_dist) + 
      geom_tile(aes(travel_time, cost_cutoff, fill = avg_access)) +
      facet_wrap(~ method) +
      scale_fill_viridis_c(
        name = "Average\naccessibility\n",
        labels = scales::label_number(scale = 1 / 1000, suffix = "k")
      ) +
      scale_x_continuous(
        name = "Travel time threshold",
        breaks = c(0, 30, 60, 90)
      ) +
      scale_y_continuous(
        name = "Relative monetary cost threshold\n(% of monthly budget)",
        breaks = c(0, 0.1, 0.2, 0.3, 0.4),
        labels = scales::label_percent()
      ) +
      theme_minimal() +
      theme(panel.grid = element_blank(), legend.position = "right")
    
    access_diff <- data.table::dcast(
      access_dist,
      travel_time + cost_cutoff ~ method,
      value.var = "avg_access"
    )
    access_diff[, diff := (pareto_frontier - get(to_compare)) / get(to_compare)]
    access_diff[is.nan(diff), diff := 0]
    access_diff[, c(to_compare, "pareto_frontier") := NULL]
    access_diff[, method := "difference"]
    
    # remove access_dist object to reduce plot object size
    rm(access_dist)
    
    difference <- ggplot(access_diff) + 
      geom_tile(aes(travel_time, cost_cutoff, fill = diff)) +
      facet_wrap(~ method) +
      scale_fill_viridis_c(
        name = "Accessibility\ndifference\n(% of original)",
        labels = scales::label_percent(),
        option = ifelse(to_compare == "free_fastest", "cividis", "inferno"),
        direction = ifelse(to_compare == "free_fastest", -1, 1)
      ) +
      scale_x_continuous(
        name = "Travel time threshold",
        breaks = c(0, 30, 60, 90)
      ) +
      scale_y_continuous(
        name = "Relative monetary cost threshold\n(% of monthly budget)",
        breaks = c(0, 0.1, 0.2, 0.3, 0.4),
        labels = scales::label_percent()
      ) +
      theme_minimal() +
      theme(
        panel.grid = element_blank(),
        legend.position = "right",
        axis.title.y = element_blank()
      )
    
    # remove access_diff object to reduce plot object size
    rm(access_diff)
    
    row <- cowplot::plot_grid(
      distribution,
      difference,
      nrow = 1,
      rel_widths = c(2, 1)
    )
    
    row
  }
  
  first_row <- make_row("free_fastest")
  second_row <- make_row("fastest_cost")
  
  p <- cowplot::plot_grid(first_row, second_row, nrow = 2)
  
  p
}

# access <- tar_read(affordability_accessibility)
# grid_path <- tar_read(rio_grid)
create_afford_per_group_heatmap <- function(access, grid_path) {
  grid <- readRDS(grid_path)
  
  access[grid, on = "id", `:=`(population = i.population, decile = i.decile)]
  access[decile == 10, group := "richest_10"]
  access[decile %in% 1:4, group := "poorest_40"]
  access <- access[!is.na(group)]
  
  access <- access[
    ,
    .(avg_access = weighted.mean(access, w = population)),
    keyby = .(method, travel_time, cost_cutoff = affordability, group)
  ]
  
  # remove grid object to reduce plot object size
  rm(grid)
  
  make_row <- function(to_compare = c("free_fastest", "fastest_cost")) {
    access_dist <- access[method %in% c(to_compare, "pareto_frontier")]
    
    distribution <- ggplot(access_dist) + 
      geom_tile(aes(travel_time, cost_cutoff, fill = avg_access)) +
      facet_grid(group ~ method) +
      scale_fill_viridis_c(
        name = "Average\naccessibility\n",
        labels = scales::label_number(scale = 1 / 1000, suffix = "k")
      ) +
      scale_x_continuous(
        name = "Travel time threshold",
        breaks = c(0, 30, 60, 90)
      ) +
      scale_y_continuous(
        name = "Relative monetary cost threshold\n(% of monthly budget)",
        breaks = c(0, 0.1, 0.2, 0.3, 0.4),
        labels = scales::label_percent()
      ) +
      theme_minimal() +
      theme(panel.grid = element_blank(), legend.position = "right")
    
    access_diff <- data.table::dcast(
      access_dist,
      travel_time + cost_cutoff + group ~ method,
      value.var = "avg_access"
    )
    access_diff[, diff := (pareto_frontier - get(to_compare)) / get(to_compare)]
    access_diff[is.nan(diff), diff := 0]
    access_diff[, c(to_compare, "pareto_frontier") := NULL]
    access_diff[, method := "difference"]
    
    # remove access_dist object to reduce plot object size
    rm(access_dist)
    
    difference <- ggplot(access_diff) + 
      geom_tile(aes(travel_time, cost_cutoff, fill = diff)) +
      facet_grid(group ~ method) +
      scale_fill_viridis_c(
        name = "Accessibility\ndifference\n(% of original)",
        labels = scales::label_percent(),
        option = ifelse(to_compare == "free_fastest", "cividis", "inferno"),
        direction = ifelse(to_compare == "free_fastest", -1, 1)
      ) +
      scale_x_continuous(
        name = "Travel time threshold",
        breaks = c(0, 30, 60, 90)
      ) +
      scale_y_continuous(
        name = "Relative monetary cost threshold\n(% of monthly budget)",
        breaks = c(0, 0.1, 0.2, 0.3, 0.4),
        labels = scales::label_percent()
      ) +
      theme_minimal() +
      theme(
        panel.grid = element_blank(),
        legend.position = "right",
        axis.title.y = element_blank()
      )
    
    # remove access_diff object to reduce plot object size
    rm(access_diff)
    
    row <- cowplot::plot_grid(
      distribution,
      difference,
      nrow = 1,
      rel_widths = c(2, 1)
    )
    
    row
  }
  
  first_row <- make_row("free_fastest")
  second_row <- make_row("fastest_cost")
  
  p <- cowplot::plot_grid(first_row, second_row, nrow = 2)
  
  p
}