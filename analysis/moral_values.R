script <- "moral_values"
source("support-functions/local_full_setup.R") # switch to remote_setup.R for remote runs

# data ----
turtle_data <- melt(turtle_cluster_data[tick <= 201],
                    measure.vars = c("care_weight", "fairness_weight", "ingroup_loyalty_weight",
                                     "authority_weight", "purity_weight"),
                    variable.name = "mf", value.name = "weight")

turtle_data[, `:=`(study    = fifelse(sample <= 150, "Study 1", "Study 2"),
                   identity = factor(identity, labels = c("Conservative", "Liberal")),
                   mf       = factor(mf, labels = c("Care", "Fairness", "Ingroup loyalty", "Authority", "Purity")))]

rm(turtle_cluster_data)

# === Starting moral values (tick == 0) ====

## S1 ----
start_plot_s1 <-
  turtle_data[tick == 0 & sample <= 150,
              .(sample_mean = mean(weight)),
              by = .(mf, identity, sample)] |>
  ggplot(aes(x = identity, y = sample_mean, color = mf)) +
  geom_line(alpha = s_alpha, linewidth = s_line, aes(group = interaction(mf, sample))) +
  stat_summary(geom = "line", fun = "mean", aes(group = mf), linewidth = m_line) +
  stat_summary(geom = "point", aes(group = mf), size = p_size) +
  scale_color_manual(values = mf_cols) +
  labs(y = "Moral value weight", x = "Political identity", color = "Moral foundation") +
  ylim(0, 5) +
  base_theme + theme(legend.position = "top")

ggsave(plot = start_plot_s1, filename = "s1_values_start.png",
       path = str_c(main_dir, "/figures"), width = 16, height = 10, units = "cm", dpi = 1200)
save(start_plot_s1, file = str_c(main_dir, "/figures/ggplot/s1_values_start_ggplot.RData"))

rm(start_plot_s1)
gc()

##  S2 ----
start_plot_s2 <-
  turtle_data[tick == 0 & sample > 150,
              .(sample_mean = mean(weight)),
              by = .(mf, identity, sample)] |>
  ggplot(aes(x = identity, y = sample_mean, color = mf)) +
  geom_line(alpha = s_alpha, linewidth = s_line, aes(group = interaction(mf, sample))) +
  stat_summary(geom = "line", fun = "mean", aes(group = mf), linewidth = m_line) +
  stat_summary(geom = "point", aes(group = mf), size = p_size) +
  scale_color_manual(values = mf_cols) +
  labs(y = "Moral value weight", x = "Political identity", color = "Moral foundation") +
  ylim(0, 5) +
  base_theme + theme(legend.position = "top")

ggsave(plot = start_plot_s2, filename = "s2_values_start.png",
       path = str_c(main_dir, "/figures"), width = 16, height = 10, units = "cm", dpi = 1200)
save(start_plot_s2, file = str_c(main_dir, "/figures/ggplot/s2_values_start_ggplot.RData"))

rm(start_plot_s2)
gc()

## S1-2 ----
### No facets ----
start_plot_s2_nofacet <-
  turtle_data[tick == 0,
              .(sample_mean = mean(weight)),
              by = .(mf, identity, sample, study)] |>
  ggplot(aes(x = identity, y = sample_mean, color = mf)) +
  geom_line(alpha = s_alpha, linewidth = s_line,
            aes(group = interaction(mf, sample, study), linetype = study)) +
  stat_summary(geom = "line", fun = "mean",
               aes(group = interaction(mf, study), linetype = study), linewidth = m_line) +
  stat_summary(geom = "point", aes(group = interaction(mf, study)), size = p_size) +
  scale_color_manual(values = mf_cols) +
  scale_linetype_manual(values = s2_lines) +
  labs(y = "Moral value weight", x = "Political identity", color = "Moral foundation") +
  ylim(0, 5) +
  guides(linetype = "none") +
  base_theme + theme(legend.position = "top")

### Study facets ----
start_plot_s2_faceted <- start_plot_s2_nofacet + facet_wrap(~study)

ggsave(plot = start_plot_s2_faceted, filename = "s1_s2_values_start_faceted.png",
       path = str_c(main_dir, "/figures"), width = 16, height = 5, units = "cm", dpi = 1200)
ggsave(plot = start_plot_s2_nofacet, filename = "s1_s2_values_start_nofacet.png",
       path = str_c(main_dir, "/figures"), width = 16, height = 5, units = "cm", dpi = 1200)
save(start_plot_s2_faceted,
     file = str_c(main_dir, "/figures/ggplot/s1_s2_values_start_faceted_ggplot.RData"))
save(start_plot_s2_nofacet,
     file = str_c(main_dir, "/figures/ggplot/s1_s2_values_start_nofacet_ggplot.RData"))

rm(start_plot_s2_faceted, start_plot_s2_nofacet)
gc()

# === Study 1 ====

## ---- initialise ----
turtle_data_s1 <- turtle_data[tick > 0 & sample <= 150,
][, Mean := mean(weight),
  by = .(identity, tick, mf)][, .(sample_mean = mean(weight)), by = .(identity, tick, mf, sample, Mean)
  ][, SE := sd(sample_mean)/sqrt(length(unique(sample))),
    by = .(identity, tick, mf)]

## ---- Moral values by foundation ----
values_time_plot_s1 <-
  ggplot(turtle_data_s1, aes(x = tick, y = Mean)) +
  geom_ribbon(alpha = rib_alpha, aes(ymin = Mean-SE, ymax = Mean+SE, fill = mf)) +
  geom_line(alpha = s_alpha, aes(y = sample_mean, colour = mf,
                                 group = interaction(sample, identity, mf)))+
  geom_line(aes(colour = mf), linewidth = m_line) +
  scale_color_manual(values = mf_cols) +
  scale_fill_manual(values = mf_cols) +
  labs(y = "Moral value weight", x = "Number of iterations",
       color = "Moral foundation", fill = "Moral foundation") +
  facet_wrap(~identity) +
  ylim(0, 5) +
  x_time +
  base_theme + theme(legend.position = "top",
                     plot.margin = unit(c(0,0.02,0,0), units = "cm"))

ggsave(plot = values_time_plot_s1, filename = "s1_values_time.png",
       path = str_c(main_dir, "/figures"), width = 16, height = 5, units = "cm", dpi = 1200)
save(values_time_plot_s1,
     file = str_c(main_dir, "/figures/ggplot/s1_values_time_ggplot.RData"))

rm(values_time_plot_s1)
gc()

## ---- Moral values by identity ----
### 2 x 3 ----
values_time_id_plot_s1_c3 <-
  ggplot(turtle_data_s1, aes(x = tick, y = Mean)) +
  geom_ribbon(alpha = rib_alpha, aes(ymin = Mean - SE, ymax = Mean + SE, fill = identity)) +
  geom_line(alpha = s_alpha, linewidth = s_line, aes(y = sample_mean, colour = identity,
                                 group = interaction(sample, identity, mf))) +
  geom_line(aes(group = interaction(identity, mf), colour = identity), linewidth = m_line) +
  scale_color_manual(values = s1_cols) +
  scale_fill_manual(values = s1_cols) +
  labs(y = "Moral value weight", x = "Number of iterations",
       color = "Political identity", fill = "Political identity") +
  ylim(0, 5) +
  x_time +
  facet_wrap(~mf, nrow = 2, ncol = 3) +
  base_theme + theme(legend.position = "top",
                     plot.margin = unit(c(0,0.08,0,0), units = "cm"))

### 1 x 5 ----
values_time_id_plot_s1_c5 <- values_time_id_plot_s1_c3 +
  facet_wrap(~mf, nrow = 1, ncol = 5) + x_time_2 + theme(plot.margin = unit(c(0,0.125,0,0), units = "cm"))

ggsave(plot = values_time_id_plot_s1_c3, filename = "s1_values_time_id_2x3.png",
       path = str_c(main_dir, "/figures"), width = 16, height = 12, units = "cm", dpi = 1200)
ggsave(plot = values_time_id_plot_s1_c5, filename = "s1_values_time_id_1x5.png",
       path = str_c(main_dir, "/figures"), width = 16, height = 5, units = "cm", dpi = 1200)

save(values_time_id_plot_s1_c3,
     file = str_c(main_dir, "/figures/ggplot/s1_values_time_id_2x3_ggplot.RData"))
save(values_time_id_plot_s1_c5,
     file = str_c(main_dir, "/figures/ggplot/s1_values_time_id_1x5_ggplot.RData"))

rm(values_time_id_plot_s1_c3, values_time_id_plot_s1_c5)
rm(turtle_data_s1)
gc()

# === Study 2 only ====
## ---- initialise ----
turtle_data_s2_only <- turtle_data[tick > 0 & sample > 150,
][, Mean := mean(weight),
  by = .(identity, tick, mf)][, .(sample_mean = mean(weight)), by = .(identity, tick, mf, sample, Mean)
  ][, SE := sd(sample_mean)/sqrt(length(unique(sample))),
    by = .(identity, tick, mf)]

## ---- Moral values by foundation ----
values_time_plot_s2 <-
  ggplot(turtle_data_s2_only, aes(x = tick, y = Mean)) +
  geom_ribbon(alpha = rib_alpha, aes(ymin = Mean - SE, ymax = Mean + SE, fill = mf)) +
  geom_line(alpha = s_alpha, linewidth = s_line, aes(y = sample_mean, colour = mf,
                                                     group = interaction(sample, identity, mf))) +
  geom_line(aes(group = interaction(identity, mf), colour = mf), linewidth = m_line) +
  scale_color_manual(values = mf_cols) +
  scale_fill_manual(values = mf_cols) +
  labs(y = "Moral value weight", x = "Number of iterations",
       color = "Moral foundation", fill = "Moral foundation") +
  facet_wrap(~identity) +
  ylim(0, 5) +
  x_time +
  base_theme + theme(legend.position = "top",
                     plot.margin = unit(c(0,0.02,0,0), units = "cm"))

ggsave(plot = values_time_plot_s2, filename = "s2_values_time.png",
       path = str_c(main_dir, "/figures"), width = 16, height = 7, units = "cm", dpi = 1200)
save(values_time_plot_s2,
     file = str_c(main_dir, "/figures/ggplot/s2_values_time_ggplot.RData"))

rm(values_time_plot_s2)
gc()

## ---- Moral values by identity ----
### 2 x 3 ----
values_time_id_plot_s2_c3 <-
  ggplot(turtle_data_s2_only, aes(x = tick, y = Mean)) +
  geom_ribbon(alpha = rib_alpha, aes(ymin = Mean - SE, ymax = Mean + SE, fill = identity)) +
  geom_line(alpha = s_alpha, linewidth = s_line, aes(y = sample_mean, colour = identity,
                                                     group = interaction(sample, identity, mf))) +
  geom_line(aes(group = interaction(identity, mf), colour = identity), linewidth = m_line) +
  scale_color_manual(values = s2_cols[3:4]) +
  scale_fill_manual(values = s2_cols[3:4]) +
  labs(y = "Moral value weight", x = "Number of iterations",
       color = "Political identity", fill = "Political identity") +
  ylim(0, 5) +
  x_time +
  facet_wrap(~mf, nrow = 2, ncol = 3) +
  base_theme + theme(legend.position = "top",
                     plot.margin = unit(c(0,0.08,0,0), units = "cm"))

### 1 x 5 ----
values_time_id_plot_s2_c5 <- values_time_id_plot_s2_c3 +
  facet_wrap(~mf, nrow = 1, ncol = 5) + x_time_2 + theme(plot.margin = unit(c(0,0.125,0,0), units = "cm"))

ggsave(plot = values_time_id_plot_s2_c3, filename = "s2_values_time_id_2x3.png",
       path = str_c(main_dir, "/figures"), width = 16, height = 11, units = "cm", dpi = 1200)
ggsave(plot = values_time_id_plot_s2_c5, filename = "s2_values_time_id_1x5.png",
       path = str_c(main_dir, "/figures"), width = 16, height = 5, units = "cm", dpi = 1200)

save(values_time_id_plot_s2_c3,
     file = str_c(main_dir, "/figures/ggplot/s2_values_time_id_2x3_ggplot.RData"))
save(values_time_id_plot_s2_c5,
     file = str_c(main_dir, "/figures/ggplot/s2_values_time_id_1x5_ggplot.RData"))

rm(values_time_id_plot_s2_c3, values_time_id_plot_s2_c5)
rm(turtle_data_s2_only)
gc()

# === Studies 1-2 ====

turtle_data_s2 <- turtle_data[tick > 0,
][, Mean := mean(weight),
  by = .(identity, tick, mf)][, .(sample_mean = mean(weight)), by = .(identity, tick, mf, sample, Mean, study)
  ][, SE := sd(sample_mean)/sqrt(length(unique(sample))),
    by = .(identity, tick, mf)]

## ---- Moral values by foundation ----
### identity facets ----
values_time_plot_s2_faceted <- ggplot(turtle_data_s2, aes(x = tick, y = Mean)) +
  geom_ribbon(alpha = rib_alpha, aes(ymin = Mean - SE, ymax = Mean + SE, fill = interaction(mf, study, sep = "\n"))) +
  geom_line(alpha = s_alpha, linewidth = s_line, aes(y = sample_mean, colour = interaction(mf, study, sep = "\n"),
                                                     group = interaction(sample, identity, mf, study),
                                                     linetype = study)) +
  geom_line(aes(colour = interaction(mf, study, sep = "\n"), linetype = study,
                group = interaction(mf, identity, study)), linewidth = m_line) +
  scale_color_manual(values = mf2_cols) +
  scale_fill_manual(values = mf2_cols) +
  scale_linetype_manual(values = s2_lines) +
  labs(y = "Moral value weight", x = "Number of iterations",
       color = "Moral foundation by \nstudy", fill = "Moral foundation by \nstudy") +
  ylim(0, 5) +
  guides(linetype = "none") +
  x_time +
  base_theme + theme(legend.position = "top",
                     plot.margin = unit(c(0,0.02,0,0), units = "cm")) +
  facet_wrap(~identity)

### Identity × study grid ----
values_time_plot_s2_grid <- values_time_plot_s2_faceted + facet_grid(identity ~ study) + x_time_2 +
  theme(plot.margin = unit(c(0,0,0,0), units = "cm"))

ggsave(plot = values_time_plot_s2_faceted, filename = "s1_s2_values_time_faceted.png",
       path = str_c(main_dir, "/figures"), width = 16, height = 8, units = "cm", dpi = 1200)
ggsave(plot = values_time_plot_s2_grid, filename = "s1_s2_values_time_grid.png",
       path = str_c(main_dir, "/figures"), width = 16, height = 12, units = "cm", dpi = 1200)
save(values_time_plot_s2_faceted,
     file = str_c(main_dir, "/figures/ggplot/s1_s2_values_time_faceted_ggplot.RData"))
save(values_time_plot_s2_grid,
     file = str_c(main_dir, "/figures/ggplot/s1_s2_values_time_grid_ggplot.RData"))

rm(values_time_plot_s2_faceted, values_time_plot_s2_grid)
gc()

## ---- Moral values by identity ----
### 2 x 3 ----
values_time_id_plot_s2_c3 <-
  ggplot(turtle_data_s2, aes(x = tick, y = Mean)) +
  geom_ribbon(alpha = rib_alpha, aes(ymin = Mean - SE, ymax = Mean + SE,
                                     fill = interaction(identity, study, sep = "\n"))) +
  geom_line(alpha = s_alpha, linewidth = s_line,
            aes(y = sample_mean,
                colour   = interaction(identity, study, sep = "\n"),
                group    = interaction(sample, identity, mf, study),
                linetype = study)) +
  geom_line(aes(colour   = interaction(identity, study, sep = "\n"),
                linetype = study), linewidth = m_line) +
  scale_color_manual(values = s2_cols) +
  scale_fill_manual(values = s2_cols) +
  scale_linetype_manual(values = s2_lines) +
  labs(y = "Moral value weight", x = "Number of iterations",
       color = "Political identity by \nstudy", fill = "Political identity by \nstudy") +
  ylim(0, 5) +
  guides(linetype = "none") +
  x_time +
  base_theme + theme(legend.position = "top",
                     plot.margin = unit(c(0,0.08,0,0), units = "cm")) +
  facet_wrap(~mf, nrow = 2, ncol = 3)

### 1 x 5 ----
values_time_id_plot_s2_c5 <- values_time_id_plot_s2_c3 + theme(legend.position = "top",
                                                               plot.margin = unit(c(0,0.125,0,0), units = "cm")) +
  facet_wrap(~mf, nrow = 1, ncol = 5) + x_time_2

### Foundation × study grid ----
values_time_id_plot_s2_grid <- values_time_id_plot_s2_c5 + facet_grid(study ~ mf) +
  theme(plot.margin = unit(c(0,0,0,0), units = "cm"))

ggsave(plot = values_time_id_plot_s2_c3, filename = "s1_s2_values_time_id_2x3.png",
       path = str_c(main_dir, "/figures"), width = 16, height = 12, units = "cm", dpi = 1200)
ggsave(plot = values_time_id_plot_s2_c5, filename = "s1_s2_values_time_id_1x5.png",
       path = str_c(main_dir, "/figures"), width = 16, height = 5, units = "cm", dpi = 1200)
ggsave(plot = values_time_id_plot_s2_grid, filename = "s1_s2_values_time_id_grid.png",
       path = str_c(main_dir, "/figures"), width = 16, height = 12, units = "cm", dpi = 1200)
save(values_time_id_plot_s2_c3,
     file = str_c(main_dir, "/figures/ggplot/s1_s2_values_time_id_2x3_ggplot.RData"))
save(values_time_id_plot_s2_c5,
     file = str_c(main_dir, "/figures/ggplot/s1_s2_values_time_id_1x5_ggplot.RData"))
save(values_time_id_plot_s2_grid,
     file = str_c(main_dir, "/figures/ggplot/s1_s2_values_time_id_grid_ggplot.RData"))

rm(values_time_id_plot_s2_c3, values_time_id_plot_s2_c5, values_time_id_plot_s2_grid)
rm(turtle_data_s2)
gc()


# === Individual trajectories ====

## S1 ----
indiv_time_plot_s1 <-
  turtle_data[tick > 0 & sample <= 150] |>
  ggplot(aes(x = tick, y = weight, color = mf)) +
  geom_line(alpha = s_alpha, linewidth = 0.005, aes(group = interaction(who, identity, sample))) +
  stat_summary(geom = "line", fun = "mean", linewidth = m_line) +
  facet_grid(mf ~ identity) +
  scale_color_manual(values = mf_cols) +
  labs(y = "Moral value weight", x = "Number of iterations", color = "Moral foundation") +
  ylim(0, 5) +
  x_time +
  base_theme + theme(legend.position = "top")

ggsave(plot = indiv_time_plot_s1, filename = "s1_values_individual_time.png",
       path = str_c(main_dir, "/figures"), width = 16, height = 16, units = "cm", dpi = 1200)
save(indiv_time_plot_s1,
     file = str_c(main_dir, "/figures/ggplot/s1_values_indiv_time_ggplot.RData"))

rm(indiv_time_plot_s1)
gc()

## S2 ----
indiv_time_plot_s2 <-
  turtle_data[tick > 0 & sample > 150] |>
  ggplot(aes(x = tick, y = weight, color = mf)) +
  geom_line(alpha = s_alpha, linewidth = s_line, aes(group = interaction(who, identity, sample))) +
  stat_summary(geom = "line", fun = "mean", linewidth = m_line) +
  facet_grid(mf ~ identity) +
  scale_color_manual(values = mf_cols) +
  labs(y = "Moral value weight", x = "Number of iterations", color = "Moral foundation") +
  ylim(0, 5) +
  x_time +
  base_theme + theme(legend.position = "top")

ggsave(plot = indiv_time_plot_s2, filename = "s2_values_individual_time.png",
       path = str_c(main_dir, "/figures"), width = 16, height = 16, units = "cm", dpi = 1200)
save(indiv_time_plot_s2,
     file = str_c(main_dir, "/figures/ggplot/s2_values_indiv_time_ggplot.RData"))

rm(indiv_time_plot_s2)
gc()
## S1-2 ----
indiv_time_plot_s2 <-
  turtle_data[tick > 0] |>
  ggplot(aes(x = tick, y = weight, color = mf)) +
  geom_line(alpha = s_alpha, linewidth = 0.005, aes(group = interaction(who, identity, sample, study),
                                linetype = study)) +
  stat_summary(geom = "line", fun = "mean", aes(linetype = study), linewidth = m_line) +
  facet_grid(mf ~ identity) +
  scale_color_manual(values = mf_cols) +
  scale_linetype_manual(values = s2_lines) +
  labs(y = "Moral value weight", x = "Number of iterations", color = "Moral foundation") +
  ylim(0, 5) +
  guides(linetype = "none") +
  x_time +
  base_theme + theme(legend.position = "top")

ggsave(plot = indiv_time_plot_s2, filename = "s1_s2_values_individual_time.png",
       path = str_c(main_dir, "/figures"), width = 16, height = 16, units = "cm", dpi = 1200)
save(indiv_time_plot_s2,
     file = str_c(main_dir, "/figures/ggplot/s1_s2_values_indiv_time_ggplot.RData"))

rm(indiv_time_plot_s2)
gc()


