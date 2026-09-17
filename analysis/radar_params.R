script <- "radar"
source("support-functions/local_full_setup.R") # switch to remote_setup.R for remote runs

library(ggradar)
library(scales)

turtle_cluster_data$identity <- factor(turtle_cluster_data$identity, labels = c("Conservative", "Liberal"))

turtle_cluster_data <- turtle_cluster_data |> rename("ch_prev" = `ch-prev`)

turtle_data <- turtle_cluster_data[,happy_num := fifelse(unhappy == "false", 1, fifelse(unhappy == "true", 0, NA))
][, .(prop_happy = mean(happy_num, na.rm = T)), by = .(sample, tick, identity,
                                            world_size, density, conflict_range, ch_prev, ch_prop,
                                            ch_inv, homophily, distol, dt_offset)]

turtle_data <- turtle_data |>
  mutate(world_size = rescale(world_size, to = c(0,1)), 
         density = rescale(density, to = c(0,1), from = c(0,100)), 
         conflict_range = as.numeric(as.character(turtle_data$conflict_range)) |> rescale(from = c(0,0.03), to = c(0,1)), 
         ch_prev = rescale(ch_prev, to = c(0,1), from = c(0,100)),
         ch_prop = rescale(ch_prop, to = c(0,1), from = c(0,100)),
         ch_inv = rescale(ch_inv, to = c(0,1), from = c(0,100)),
         homophily = rescale(as.numeric(as.character(homophily)), to = c(0,1), from = c(0,8)),
         distol = rescale(distol, to = c(0,1), from = c(0,100)),
         dt_offset = rescale(dt_offset, to = c(0,1), from = c(0,10))
  ) |>
  group_by(sample, identity, world_size, density, conflict_range, 
           ch_prev, ch_prop, ch_inv, homophily, distol, dt_offset) |>
  summarise(prop_happy = mean(prop_happy, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(names_from = identity, 
              values_from = prop_happy,
              names_prefix = "prop_happy_") |>
  mutate(
    lib_bin = cut(prop_happy_Liberal, 
                  breaks = seq(0, 1, 0.25), 
                  labels = c("0-25%", "25-50%", "50-75%", "75-100%"),
                  include.lowest = TRUE),
    con_bin = cut(prop_happy_Conservative, 
                  breaks = seq(0, 1, 0.25), 
                  labels = c("0-25%", "25-50%", "50-75%", "75-100%"),
                  include.lowest = TRUE),
    happy_label = paste0("Liberal: ", lib_bin, "\n", "Conservative: ", con_bin),
    happy_label = factor(happy_label, levels = c(
      paste0("Liberal: ", rep(c("0-25%", "25-50%", "50-75%", "75-100%"), each = 4), 
             "\n", "Conservative: ", rep(c("0-25%", "25-50%", "50-75%", "75-100%"), times = 4))
    ))
  ) |>
  select(sample, world_size, density, conflict_range, ch_prev, ch_prop, 
         ch_inv, homophily, distol, dt_offset, happy_label)

facet_radar <- function(df, group, vars = NULL, facet, var_names = NULL) {
  
  # Rename group column
  df <- df |> 
    select(all_of(c(group, facet, vars))) |> 
    rename("Group" = all_of(group)) 
  
  # Create all possible facet levels (4x4 = 16 combinations)
  all_levels <- levels(df[[facet]])
  if (is.null(all_levels)) {
    all_levels <- sort(unique(df[[facet]]))
  }
  
  # Initialize plot list with 16 slots
  plot_list <- vector("list", length = length(all_levels))
  names(plot_list) <- all_levels
  
  # Get existing facets in data
  existing_facets <- unique(df[[facet]])
  
  # Create plots for existing facets
  for (f in all_levels) {
    if (f %in% existing_facets) {
      f_df <- df |> 
        filter(.data[[facet]] == f) |>
        select(-all_of(facet))
      
      # Rename variables if var_names provided
      if (!is.null(var_names)) {
        if (length(var_names) != length(vars)) {
          stop("var_names must have the same length as vars")
        }
        # Create named vector for renaming (excluding Group)
        rename_vec <- setNames(vars, var_names)
        f_df <- f_df |> 
          rename(!!!rename_vec)
      }
      
      # Get samples for caption with line breaks
      samples <- unique(f_df$Group)
      
      if (length(samples) <= 10) {
        sample_text <- paste(samples, collapse = ", ")
      } else {
        lines <- list()
        # First line: first 10 samples
        lines[[1]] <- paste(samples[1:10], collapse = ", ")
        
        # Remaining samples in groups of 12
        remaining_idx <- 11:length(samples)
        if (length(remaining_idx) > 0) {
          groups <- split(remaining_idx, ceiling(seq_along(remaining_idx) / 12))
          for (i in seq_along(groups)) {
            lines[[i + 1]] <- paste(samples[groups[[i]]], collapse = ", ")
          }
        }
        
        sample_text <- paste(lines, collapse = "\n")
      }
      
      p <- ggradar(f_df,
                   group.point.size = 1,        # Reduce point size
                   group.line.width = 0.5,      # Reduce line width
                   axis.label.size = 2.5,       # Smaller axis labels
                   grid.label.size = 2.5,       # Smaller grid labels
                   plot.legend = FALSE,         # Remove legend
                   gridline.mid.colour = "grey",
                   background.circle.colour = NA,
                   base.size = 5
                   ) +
        labs(caption = paste0("Samples: ", sample_text), title = f) +
        #scale_x_continuous(expand = expansion(mult = 0.25)) +  # Expand x-axis by 25%
        # scale_y_continuous(expand = expansion(mult = 0.25)) +  # Expand y-axis by 25%
        scale_colour_manual(values = setNames(rep("orange", length(unique(f_df$Group))), 
                                              unique(f_df$Group))) +
        theme(
          plot.title = element_text(size = 7),
          plot.caption = element_text(size = 6, hjust = 0),
          plot.margin = margin(t = 2, r = 1, b = 1, l = 1, unit = "pt")
        )
      
      # Add alpha transparency to the correct layers
      p$layers[[7]]$aes_params$alpha <- 0.5   # Polygon fill (layer 7)
      p$layers[[9]]$aes_params$alpha <- 0.5   # Lines (layer 9)
      p$layers[[10]]$aes_params$alpha <- 0.5  # Points (layer 10)
      
      plot_list[[f]] <- p
    } else {
      # Add empty spacer for missing combinations
      plot_list[[f]] <- plot_spacer()
    }
  }
  
  # Combine plots with patchwork in 4x4 grid
  combined_plot <- wrap_plots(plot_list, ncol = 4, nrow = 4) &
    theme(plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"))
  
  return(combined_plot)
}

# --- Study 1 (sims 1-150) ----
facet_radar(turtle_data |> filter(sample %in% 1:150), 
            group = "sample", 
            facet = "happy_label",
            vars = c("ch_prop", "ch_inv", "dt_offset", "homophily", "distol", "conflict_range",  "world_size", "density",
                     "ch_prev"),
            var_names = c("Event proportion", "Event\ninvaraince", "DT\noffset",
                          "Homophily", "Dissimilarity\ntolerance",  "Conflict\nthreshold", "World\nsize", "Agent\ndensity", 
                          "Event\nprevelance"))

ggsave(filename = "s1_param_radar_happy_test.png",
       path = str_c(main_dir, "/figures"),
       width = 25, height = 25, units = "cm", dpi = 1000)

# --- Study 2 (sims 151-300) ----
facet_radar(turtle_data |> filter(sample %in% 151:300), 
            group = "sample", 
            facet = "happy_label",
            vars = c("ch_prop", "ch_inv", "dt_offset", "homophily", "distol", "conflict_range",  "world_size", "density",
                     "ch_prev"),
            var_names = c("Event proportion", "Event\ninvaraince", "DT\noffset",
                          "Homophily", "Dissimilarity\ntolerance",  "Conflict\nthreshold", "World\nsize", "Agent\ndensity", 
                          "Event\nprevelance"))

ggsave(plot = s2_radar, filename = "s2_param_radar_happy.png",
       path = str_c(main_dir, "/figures"),
       width = 25, height = 25, units = "cm", dpi = 100)
