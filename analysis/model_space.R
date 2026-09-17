script <- "space"
source("support-functions/local_full_setup.R") # switch to remote_setup.R for remote runs

# settings ----
space_theme <- 
  base_theme +
  theme(panel.grid = element_blank(), 
        panel.border =  element_rect(fill = NA, color = "black"), 
        strip.background = element_rect(fill = "White", color = NA), 
        strip.text.y = element_text(angle = 270, size = upper_base, face = "plain"), 
        axis.ticks = element_blank(), 
        axis.text = element_blank(), 
        axis.title = element_blank(),
        axis.line = element_blank(),
        legend.key = element_rect(colour = NA),
        legend.position = "top") 

alphas <- c(1,1, 0.25,0.25)
shapes <- c(15,15,16,16)
label <- "Agent key"
n_cols <- 2
l_size <- 2
space_p_size <- 0.3

# data samples ----
turtle_cluster_data$identity <- factor(turtle_cluster_data$identity, labels = c("Conservative", "Liberal"))
turtle_cluster_data$unhappy <- factor(turtle_cluster_data$unhappy, labels = c("0", "Satisfied", "Dissatisfied"))

set.seed(1917169)
s1_samples <- sample(1:150, 3)

set.seed(1917169)
s2_samples <- sample(151:300, 3)

tick_seq_all <- c(1, 25 ,50,100,150,200)
tick_seq_start <- c(1, 5, 10, 15,20, 25)

# Low invariance ====
s1_spaces <- turtle_cluster_data[sample %in% s1_samples & 
                      tick %in% c(tick_seq_all, tick_seq_start),
][, sample := paste("Sample ", sample)
][, iter := paste("Iteration ", tick)
][, id_hap := interaction(identity, unhappy, sep=": ")
][, sample := factor(sample, levels = paste("Sample ", sort(s1_samples)))
][, iter := factor(iter, levels = paste("Iteration ", c(tick_seq_start[-6], tick_seq_all[-1])))]

s1_space <- ggplot(s1_spaces[tick %in% tick_seq_all,], 
                   aes(x = x_cor, y = y_cor)) +
  geom_point(aes(shape = id_hap,
                 alpha = id_hap,
                 color = id_hap),
             size = space_p_size)+
  facet_grid(sample~iter)+
  scale_color_manual(values=c(s1_cols, s1_cols)) +
  scale_alpha_manual(values = alphas)+
  scale_shape_manual(values = shapes)+
  labs(colour = label, 
       alpha = label,
       shape = label)+
  guides(colour = guide_legend(override.aes = list(size = l_size))) +
  space_theme

ggsave(filename = "s1_space_samples_all_iters.png",path = str_c(main_dir, "/figures"),
       width = 16, height = 9, units = "cm", dpi = 1200)
save(s1_space, file = str_c(main_dir, "/figures/ggplot/s1_space_ggplot.RData"))

s1_space_start <- ggplot(s1_spaces[tick %in% tick_seq_start,],
       aes(x = x_cor, y = y_cor)) +
  geom_point(aes(shape = id_hap,
                 alpha = id_hap,
                 color = id_hap),
             size = space_p_size)+
  facet_grid(sample~iter)+
  scale_color_manual(values=c(s1_cols, s1_cols)) +
  scale_alpha_manual(values = alphas)+
  scale_shape_manual(values = shapes)+
  labs(colour = label,
       alpha = label,
       shape = label)+
  guides(colour = guide_legend(override.aes = list(size = l_size))) +
  space_theme

ggsave(plot = s1_space_start, filename = "s1_space_samples_start_iters.png",path = str_c(main_dir, "/figures"),
       width = 16, height = 9, units = "cm", dpi = 1200)
save(s1_space_start, file = str_c(main_dir, "/figures/ggplot/s1_space_start_ggplot.RData"))

param_tab <- turtle_cluster_data[sample %in% s1_samples & who == 1 & tick == 0][,c(1,14:22)]
write_csv(param_tab, file = str_c(main_dir, "/figures/","s1_space_samples_params.csv"))

outcomes_tab <- s1_spaces[,unhappy_num := fifelse(unhappy == "Dissatisfied", 0,1)
][, cluster_l := fifelse(cluster != "no_cluster", 1, 0)
][,"cluster_size" := fifelse( cluster != "no_cluster", .N, NA), 
                          by = .(sample, iter, cluster)
][, cluster_homog :=  .N/cluster_size,
    by = .(sample, iter, cluster, identity)
][, .(satisfaction = mean(unhappy_num),
      perc_ingroup = mean(perceived_ingroup_n/total_neighbours, na.rm = T),
      act_ingroup = mean(actual_ingroup_n/total_neighbours, na.rm = T),
      accuracy = mean(hit_rate - false_alarm),
      homogeniety = mean(cluster_homog, na.rm = T),
      prop_clustered = mean(cluster_l)
), by = .(sample, iter, identity)] 
write_csv(outcomes_tab, file = str_c(main_dir, "/figures/","s1_space_samples_outcomes.csv"))


# High invariance ====
s2_spaces <- turtle_cluster_data[sample %in% s2_samples & 
                                   tick %in% c(tick_seq_all, tick_seq_start),
][, sample := paste("Sample ", sample)
][, iter := paste("Iteration ", tick)
][, id_hap := interaction(identity, unhappy, sep=": ")
][, sample := factor(sample, levels = paste("Sample ", sort(s2_samples)))
][, iter := factor(iter, levels = paste("Iteration ", c(tick_seq_start[-6], tick_seq_all[-1])))]

s2_space <- ggplot(s2_spaces[tick %in% tick_seq_all,], aes(x = x_cor, y = y_cor)) +
  geom_point(aes(shape = id_hap,
                 alpha = id_hap,
                 color = id_hap),
             size = space_p_size)+
  facet_grid(sample~iter)+
  scale_color_manual(values=c(s2_cols[3:4], s2_cols[3:4])) +
  scale_alpha_manual(values = alphas)+
  scale_shape_manual(values = shapes)+
  labs(colour = label,
       alpha = label,
       shape = label)+
  guides(colour = guide_legend(override.aes = list(size = l_size))) +
  space_theme

ggsave(plot = s2_space, filename = "s2_space_samples_all_iters.png",path = str_c(main_dir, "/figures"),
       width = 16, height = 9, units = "cm", dpi = 1200)
save(s2_space, file = str_c(main_dir, "/figures/ggplot/s2_space_ggplot.RData"))

s2_space_start <- ggplot(s2_spaces[tick %in% tick_seq_start,],
       aes(x = x_cor, y = y_cor)) +
  geom_point(aes(shape = id_hap,
                 alpha = id_hap,
                 color = id_hap),
             size = space_p_size)+
  facet_grid(sample~iter)+
  scale_color_manual(values=c(s2_cols[3:4], s2_cols[3:4])) +
  scale_alpha_manual(values = alphas)+
  scale_shape_manual(values = shapes)+
  labs(colour = label,
       alpha = label,
       shape = label)+
  guides(colour = guide_legend(override.aes = list(size = l_size))) +
  space_theme

ggsave(plot = s2_space_start, filename = "s2_space_samples_start_iters.png",path = str_c(main_dir, "/figures"),
       width = 16, height = 9, units = "cm", dpi = 1200)
save(s2_space_start, file = str_c(main_dir, "/figures/ggplot/s2_space_start_ggplot.RData"))

param_tab <- turtle_cluster_data[sample %in% s2_samples & who == 1 & tick == 0][,c(1,14:22)]
write_csv(param_tab, file = str_c(main_dir, "/figures/","s2_space_samples_params.csv"))

outcomes_tab <- s2_spaces[,unhappy_num := fifelse(unhappy == "Dissatisfied", 0,1)
][, cluster_l := fifelse(cluster != "no_cluster", 1, 0)
][,"cluster_size" := fifelse( cluster != "no_cluster", .N, NA), 
  by = .(sample, iter, cluster)
][, cluster_homog :=  .N/cluster_size,
  by = .(sample, iter, cluster, identity)
][, .(satisfaction = mean(unhappy_num),
      perc_ingroup = mean(perceived_ingroup_n/total_neighbours, na.rm = T),
      act_ingroup = mean(actual_ingroup_n/total_neighbours, na.rm = T),
      accuracy = mean(hit_rate - false_alarm),
      homogeniety = mean(cluster_homog, na.rm = T),
      prop_clustered = mean(cluster_l)
), by = .(sample, iter, identity)] 
write_csv(outcomes_tab, file = str_c(main_dir, "/figures/","s2_space_samples_outcomes.csv"))

# full dataset ----
tick_seq_all <- c(1,50,100,150,200)

space_p_size <- 0.0005
space_theme <- space_theme + 
  theme(
    panel.spacing = unit(0, "lines"),
    strip.text = element_text(size = 8),
    strip.text.y = element_text(size = 8),
    legend.text = element_text(size = 9),
    legend.title = element_text(size = 10)
  )
## s1 ----
s1_spaces <- turtle_cluster_data[sample %in% 1:150 &
                                   tick %in% c(tick_seq_all, tick_seq_start),
][, sample := paste0("S", sample)
][, iter := paste0("I", tick)
][, id_hap := interaction(identity, unhappy, sep=": ")
][, sample := factor(sample, levels = paste0("S", 1:150))
][, iter := factor(iter, levels = paste0("I", c(tick_seq_start[-6], tick_seq_all[-1])))]

p1_samples <- paste0("S", 1:60)
p2_samples <- paste0("S", 61:120)
p3_samples <- paste0("S", 121:150)

### p1 ----
s1_space_p1_1 <- ggplot(s1_spaces[sample %in% p1_samples[1:20] & tick %in% tick_seq_all], 
                   aes(x = x_cor, y = y_cor)) +
  geom_point(aes(shape = id_hap,
                 alpha = id_hap,
                 color = id_hap),
             size = space_p_size)+
  facet_grid(sample~iter)+
  scale_color_manual(values=c(s1_cols, s1_cols)) +
  scale_alpha_manual(values = alphas)+
  scale_shape_manual(values = shapes)+
  labs(colour = label, 
       alpha = label,
       shape = label)+
  guides(colour = guide_legend(override.aes = list(size = l_size), ncol = 2)) +
  space_theme 

s1_space_p1_2 <- ggplot(s1_spaces[sample %in% p1_samples[21:40] & tick %in% tick_seq_all], 
                        aes(x = x_cor, y = y_cor)) +
  geom_point(aes(shape = id_hap,
                 alpha = id_hap,
                 color = id_hap),
             size = space_p_size)+
  facet_grid(sample~iter)+
  scale_color_manual(values=c(s1_cols, s1_cols)) +
  scale_alpha_manual(values = alphas)+
  scale_shape_manual(values = shapes)+
  labs(colour = label, 
       alpha = label,
       shape = label)+
  guides(colour = guide_legend(override.aes = list(size = l_size), ncol = 2)) +
  space_theme 

s1_space_p1_3 <- ggplot(s1_spaces[sample %in% p1_samples[41:60] & tick %in% tick_seq_all], 
                        aes(x = x_cor, y = y_cor)) +
  geom_point(aes(shape = id_hap,
                 alpha = id_hap,
                 color = id_hap),
             size = space_p_size)+
  facet_grid(sample~iter)+
  scale_color_manual(values=c(s1_cols, s1_cols)) +
  scale_alpha_manual(values = alphas)+
  scale_shape_manual(values = shapes)+
  labs(colour = label, 
       alpha = label,
       shape = label)+
  guides(colour = guide_legend(override.aes = list(size = l_size), ncol = 2)) +
  space_theme 

s1_space_p1 <- (s1_space_p1_1 + s1_space_p1_2 + s1_space_p1_3) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

ggsave(filename = "s1_space_samples_all_p1.png",path = str_c(main_dir, "/figures"),
       width = 19, height = 25, units = "cm", dpi = 1200)
save(s1_space_p1, file = str_c(main_dir, "/figures/ggplot/s1_space_p1_ggplot.RData"))

### p2 ----
s1_space_p2_1 <- ggplot(s1_spaces[sample %in% p2_samples[1:20] & tick %in% tick_seq_all], 
                        aes(x = x_cor, y = y_cor)) +
  geom_point(aes(shape = id_hap,
                 alpha = id_hap,
                 color = id_hap),
             size = space_p_size)+
  facet_grid(sample~iter)+
  scale_color_manual(values=c(s1_cols, s1_cols)) +
  scale_alpha_manual(values = alphas)+
  scale_shape_manual(values = shapes)+
  labs(colour = label, 
       alpha = label,
       shape = label)+
  guides(colour = guide_legend(override.aes = list(size = l_size), ncol = 2)) +
  space_theme 

s1_space_p2_2 <- ggplot(s1_spaces[sample %in% p2_samples[21:40] & tick %in% tick_seq_all], 
                        aes(x = x_cor, y = y_cor)) +
  geom_point(aes(shape = id_hap,
                 alpha = id_hap,
                 color = id_hap),
             size = space_p_size)+
  facet_grid(sample~iter)+
  scale_color_manual(values=c(s1_cols, s1_cols)) +
  scale_alpha_manual(values = alphas)+
  scale_shape_manual(values = shapes)+
  labs(colour = label, 
       alpha = label,
       shape = label)+
  guides(colour = guide_legend(override.aes = list(size = l_size), ncol = 2)) +
  space_theme 

s1_space_p2_3 <- ggplot(s1_spaces[sample %in% p2_samples[41:60] & tick %in% tick_seq_all], 
                        aes(x = x_cor, y = y_cor)) +
  geom_point(aes(shape = id_hap,
                 alpha = id_hap,
                 color = id_hap),
             size = space_p_size)+
  facet_grid(sample~iter)+
  scale_color_manual(values=c(s1_cols, s1_cols)) +
  scale_alpha_manual(values = alphas)+
  scale_shape_manual(values = shapes)+
  labs(colour = label, 
       alpha = label,
       shape = label)+
  guides(colour = guide_legend(override.aes = list(size = l_size), ncol = 2)) +
  space_theme 

s1_space_p2 <- (s1_space_p2_1 + s1_space_p2_2 + s1_space_p2_3) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

ggsave(filename = "s1_space_samples_all_p2.png",path = str_c(main_dir, "/figures"),
       width = 19, height = 25, units = "cm", dpi = 1200)
save(s1_space_p2, file = str_c(main_dir, "/figures/ggplot/s1_space_ggplot_p2.RData"))

### p3 ----
s1_space_p3_1 <- ggplot(s1_spaces[sample %in% p3_samples[1:10] & tick %in% tick_seq_all], 
                        aes(x = x_cor, y = y_cor)) +
  geom_point(aes(shape = id_hap,
                 alpha = id_hap,
                 color = id_hap),
             size = space_p_size)+
  facet_grid(sample~iter)+
  scale_color_manual(values=c(s1_cols, s1_cols)) +
  scale_alpha_manual(values = alphas)+
  scale_shape_manual(values = shapes)+
  labs(colour = label, 
       alpha = label,
       shape = label)+
  guides(colour = guide_legend(override.aes = list(size = l_size), ncol = 2)) +
  space_theme 

s1_space_p3_2 <- ggplot(s1_spaces[sample %in% p3_samples[11:20] & tick %in% tick_seq_all], 
                        aes(x = x_cor, y = y_cor)) +
  geom_point(aes(shape = id_hap,
                 alpha = id_hap,
                 color = id_hap),
             size = space_p_size)+
  facet_grid(sample~iter)+
  scale_color_manual(values=c(s1_cols, s1_cols)) +
  scale_alpha_manual(values = alphas)+
  scale_shape_manual(values = shapes)+
  labs(colour = label, 
       alpha = label,
       shape = label)+
  guides(colour = guide_legend(override.aes = list(size = l_size), ncol = 2)) +
  space_theme 

s1_space_p3_3 <- ggplot(s1_spaces[sample %in% p3_samples[21:30] & tick %in% tick_seq_all], 
                        aes(x = x_cor, y = y_cor)) +
  geom_point(aes(shape = id_hap,
                 alpha = id_hap,
                 color = id_hap),
             size = space_p_size)+
  facet_grid(sample~iter)+
  scale_color_manual(values=c(s1_cols, s1_cols)) +
  scale_alpha_manual(values = alphas)+
  scale_shape_manual(values = shapes)+
  labs(colour = label, 
       alpha = label,
       shape = label)+
  guides(colour = guide_legend(override.aes = list(size = l_size), ncol = 2)) +
  space_theme 

s1_space_p3 <- (s1_space_p3_1 + s1_space_p3_2 + s1_space_p3_3) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

ggsave(filename = "s1_space_samples_all_p3.png",path = str_c(main_dir, "/figures"),
       width = 19, height = 13.5, units = "cm", dpi = 1200)
save(s1_space_p3, file = str_c(main_dir, "/figures/ggplot/s1_space_ggplot_p3.RData"))


## s2 ----
s2_spaces <- turtle_cluster_data[sample %in% 151:300 &
                                   tick %in% c(tick_seq_all, tick_seq_start),
][, sample := paste0("S", sample)
][, iter := paste0("I", tick)
][, id_hap := interaction(identity, unhappy, sep=": ")
][, sample := factor(sample, levels = paste0("S", 151:300))
][, iter := factor(iter, levels = paste0("I", c(tick_seq_start[-6], tick_seq_all[-1])))]

p1_samples <- paste0("S", 151:210)
p2_samples <- paste0("S", 211:270)
p3_samples <- paste0("S", 271:300)

### p1 ----
s2_space_p1_1 <- ggplot(s2_spaces[sample %in% p1_samples[1:20] & tick %in% tick_seq_all], 
                        aes(x = x_cor, y = y_cor)) +
  geom_point(aes(shape = id_hap,
                 alpha = id_hap,
                 color = id_hap),
             size = space_p_size)+
  facet_grid(sample~iter)+
  scale_color_manual(values=c(s2_cols[3:4], s2_cols[3:4])) +
  scale_alpha_manual(values = alphas)+
  scale_shape_manual(values = shapes)+
  labs(colour = label, 
       alpha = label,
       shape = label)+
  guides(colour = guide_legend(override.aes = list(size = l_size), ncol = 2)) +
  space_theme 

s2_space_p1_2 <- ggplot(s2_spaces[sample %in% p1_samples[21:40] & tick %in% tick_seq_all], 
                        aes(x = x_cor, y = y_cor)) +
  geom_point(aes(shape = id_hap,
                 alpha = id_hap,
                 color = id_hap),
             size = space_p_size)+
  facet_grid(sample~iter)+
  scale_color_manual(values=c(s2_cols[3:4], s2_cols[3:4])) +
  scale_alpha_manual(values = alphas)+
  scale_shape_manual(values = shapes)+
  labs(colour = label, 
       alpha = label,
       shape = label)+
  guides(colour = guide_legend(override.aes = list(size = l_size), ncol = 2)) +
  space_theme 

s2_space_p1_3 <- ggplot(s2_spaces[sample %in% p1_samples[41:60] & tick %in% tick_seq_all], 
                        aes(x = x_cor, y = y_cor)) +
  geom_point(aes(shape = id_hap,
                 alpha = id_hap,
                 color = id_hap),
             size = space_p_size)+
  facet_grid(sample~iter)+
  scale_color_manual(values=c(s2_cols[3:4], s2_cols[3:4])) +
  scale_alpha_manual(values = alphas)+
  scale_shape_manual(values = shapes)+
  labs(colour = label, 
       alpha = label,
       shape = label)+
  guides(colour = guide_legend(override.aes = list(size = l_size), ncol = 2)) +
  space_theme 

s2_space_p1 <- (s2_space_p1_1 + s2_space_p1_2 + s2_space_p1_3) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

ggsave(filename = "s2_space_samples_all_p1.png",path = str_c(main_dir, "/figures"),
       width = 19, height = 25, units = "cm", dpi = 1200)
save(s2_space_p1, file = str_c(main_dir, "/figures/ggplot/s2_space_p1_ggplot.RData"))

### p2 ----
s2_space_p2_1 <- ggplot(s2_spaces[sample %in% p2_samples[1:20] & tick %in% tick_seq_all], 
                        aes(x = x_cor, y = y_cor)) +
  geom_point(aes(shape = id_hap,
                 alpha = id_hap,
                 color = id_hap),
             size = space_p_size)+
  facet_grid(sample~iter)+
  scale_color_manual(values=c(s2_cols[3:4], s2_cols[3:4])) +
  scale_alpha_manual(values = alphas)+
  scale_shape_manual(values = shapes)+
  labs(colour = label, 
       alpha = label,
       shape = label)+
  guides(colour = guide_legend(override.aes = list(size = l_size), ncol = 2)) +
  space_theme 

s2_space_p2_2 <- ggplot(s2_spaces[sample %in% p2_samples[21:40] & tick %in% tick_seq_all], 
                        aes(x = x_cor, y = y_cor)) +
  geom_point(aes(shape = id_hap,
                 alpha = id_hap,
                 color = id_hap),
             size = space_p_size)+
  facet_grid(sample~iter)+
  scale_color_manual(values=c(s2_cols[3:4], s2_cols[3:4])) +
  scale_alpha_manual(values = alphas)+
  scale_shape_manual(values = shapes)+
  labs(colour = label, 
       alpha = label,
       shape = label)+
  guides(colour = guide_legend(override.aes = list(size = l_size), ncol = 2)) +
  space_theme 

s2_space_p2_3 <- ggplot(s2_spaces[sample %in% p2_samples[41:60] & tick %in% tick_seq_all], 
                        aes(x = x_cor, y = y_cor)) +
  geom_point(aes(shape = id_hap,
                 alpha = id_hap,
                 color = id_hap),
             size = space_p_size)+
  facet_grid(sample~iter)+
  scale_color_manual(values=c(s2_cols[3:4], s2_cols[3:4])) +
  scale_alpha_manual(values = alphas)+
  scale_shape_manual(values = shapes)+
  labs(colour = label, 
       alpha = label,
       shape = label)+
  guides(colour = guide_legend(override.aes = list(size = l_size), ncol = 2)) +
  space_theme 

s2_space_p2 <- (s2_space_p2_1 + s2_space_p2_2 + s2_space_p2_3) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

ggsave(filename = "s2_space_samples_all_p2.png",path = str_c(main_dir, "/figures"),
       width = 19, height = 25, units = "cm", dpi = 1200)
save(s2_space_p2, file = str_c(main_dir, "/figures/ggplot/s2_space_ggplot_p2.RData"))

### p3 ----
s2_space_p3_1 <- ggplot(s2_spaces[sample %in% p3_samples[1:10] & tick %in% tick_seq_all], 
                        aes(x = x_cor, y = y_cor)) +
  geom_point(aes(shape = id_hap,
                 alpha = id_hap,
                 color = id_hap),
             size = space_p_size)+
  facet_grid(sample~iter)+
  scale_color_manual(values=c(s2_cols[3:4], s2_cols[3:4])) +
  scale_alpha_manual(values = alphas)+
  scale_shape_manual(values = shapes)+
  labs(colour = label, 
       alpha = label,
       shape = label)+
  guides(colour = guide_legend(override.aes = list(size = l_size), ncol = 2)) +
  space_theme 

s2_space_p3_2 <- ggplot(s2_spaces[sample %in% p3_samples[11:20] & tick %in% tick_seq_all], 
                        aes(x = x_cor, y = y_cor)) +
  geom_point(aes(shape = id_hap,
                 alpha = id_hap,
                 color = id_hap),
             size = space_p_size)+
  facet_grid(sample~iter)+
  scale_color_manual(values=c(s2_cols[3:4], s2_cols[3:4])) +
  scale_alpha_manual(values = alphas)+
  scale_shape_manual(values = shapes)+
  labs(colour = label, 
       alpha = label,
       shape = label)+
  guides(colour = guide_legend(override.aes = list(size = l_size), ncol = 2)) +
  space_theme 

s2_space_p3_3 <- ggplot(s2_spaces[sample %in% p3_samples[21:30] & tick %in% tick_seq_all], 
                        aes(x = x_cor, y = y_cor)) +
  geom_point(aes(shape = id_hap,
                 alpha = id_hap,
                 color = id_hap),
             size = space_p_size)+
  facet_grid(sample~iter)+
  scale_color_manual(values=c(s2_cols[3:4], s2_cols[3:4])) +
  scale_alpha_manual(values = alphas)+
  scale_shape_manual(values = shapes)+
  labs(colour = label, 
       alpha = label,
       shape = label)+
  guides(colour = guide_legend(override.aes = list(size = l_size), ncol = 2)) +
  space_theme 

s2_space_p3 <- (s2_space_p3_1 + s2_space_p3_2 + s2_space_p3_3) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")

ggsave(filename = "s2_space_samples_all_p3.png",path = str_c(main_dir, "/figures"),
       width = 19, height = 13.5, units = "cm", dpi = 1200)
save(s2_space_p3, file = str_c(main_dir, "/figures/ggplot/s2_space_ggplot_p3.RData"))