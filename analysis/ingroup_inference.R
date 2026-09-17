script <- "ingroup_inference"
source("support-functions/local_full_setup.R") # switch to remote_setup.R for remote runs

## initialise data ----
turtle_data <- turtle_cluster_data[tick > 0 & tick <= 201,][,
                                                            "study" := fifelse(sample <= 150,
                                                                               "Study 1",
                                                                               "Study 2")][,
                                                            d_prime_ingroup := hit_rate - false_alarm]

turtle_data$identity <- factor(turtle_data$identity,
                               labels = c("Conservative", "Liberal"))
rm(turtle_cluster_data); gc()

# === Study 1 ====

## ---- initialise ----
turtle_data_s1 <- turtle_data[sample <= 150]

## ---- Perceived ingroup over time ----
### No facets ----
perceived_time_plot_s1_nofacet <- turtle_data_s1[,
                                                 perc_prop := fifelse(total_neighbours > 0, perceived_ingroup_n/total_neighbours, 0),
                                                 by = .(sample, tick, who, identity)
][, sample_mean := mean(perc_prop), by = .(sample, tick, identity)
][, SE := sd(perc_prop)/sqrt(length(unique(sample))), by = .(tick, identity)
][, Mean := mean(perc_prop), by = .(tick, identity)] |>
  ggplot(aes(y = Mean, x = tick)) +
  geom_ribbon(alpha = rib_alpha, aes(ymin = Mean-SE, ymax = Mean+SE, fill = identity)) +
  geom_line(aes(color = identity), linewidth = m_line) +
  geom_line(alpha = s_alpha, linewidth = s_line, aes(y = sample_mean, color = identity, group = interaction(sample, identity))) +
  scale_color_manual(values = s1_cols) +
  scale_fill_manual(values = s1_cols) +
  labs(y = "Proportion of neighbouring agents", x = "Number of iterations",
       color = "Political \nidentity", fill = "Political \nidentity",
       title = "S1: Proportion of neighbours perceived as ingroup over time \nby political identity",
       caption = "Solid lines indicates identity-level mean proportion across samples.
       \nFaint lines indicates identity-level sample mean proportions.
       \nRibbons indicate standard error between mean sample proportions.") +
  ylim(0, 1) +
  x_time +
  base_theme + theme(legend.position = "top")

### Identity facets ----
perceived_time_plot_s1_faceted <- perceived_time_plot_s1_nofacet + facet_wrap(~identity)

ggsave(plot = perceived_time_plot_s1_faceted, filename = "s1_perceived_ingroup_time_faceted.png",
       path = str_c(main_dir, "/figures"), width = 16, height = 10, units = "cm", dpi = 1200)
ggsave(plot = perceived_time_plot_s1_nofacet, filename = "s1_perceived_ingroup_time_nofacet.png",
       path = str_c(main_dir, "/figures"), width = 16, height = 10, units = "cm", dpi = 1200)
save(perceived_time_plot_s1_faceted, file = str_c(main_dir, "/figures/ggplot/s1_perceived_ingroup_time_faceted_ggplot.RData"))
save(perceived_time_plot_s1_nofacet, file = str_c(main_dir, "/figures/ggplot/s1_perceived_ingroup_time_nofacet_ggplot.RData"))

rm(perceived_time_plot_s1_faceted, perceived_time_plot_s1_nofacet)
gc()

## ---- Perceived ingroup aggregated ----
perceived_agg_plot_s1 <- turtle_data_s1[,
                                        perc_prop := fifelse(total_neighbours > 0, perceived_ingroup_n/total_neighbours, 0),
                                        by = .(sample, tick, who, identity)
][, .(sample_mean = mean(perc_prop)), by = .(sample, who, identity)] |>
  ggbetween_cus(x = identity, y = sample_mean,
                title   = "S1: Proportion of perceived ingroup by political identity",
                caption = "Individual points indicate agent-level proportions averaged over time.
                \nn indicates the number of agents across samples.",
                ggtheme = base_theme + theme(legend.position = "top"),
                ylab    = "Proportion of neighbouring agents",
                xlab    = "Political identity",
                ggplot.component = list(scale_color_manual(values = s1_cols), ylim(0, 1))) +
  geom_point(data = turtle_data_s1[, .(sample_mean = mean(fifelse(total_neighbours > 0,
                                                                  perceived_ingroup_n/total_neighbours, 0))),
                                   by = .(sample, identity)],
             size = s_m_p_size, alpha = s_m_p_alpha, shape = s_m_p_shape,
             aes(colour = identity), position = position_nudge(x = s_m_p_nudge))

ggsave(filename = "s1_perceived_ingroup_agg.png", path = str_c(main_dir, "/figures"),
       width = 16, height = 10, units = "cm", dpi = 1200)
save(perceived_agg_plot_s1, file = str_c(main_dir, "/figures/ggplot/s1_perceived_ingroup_agg_ggplot.RData"))

rm(perceived_agg_plot_s1)
gc()

## ---- Actual ingroup over time ----
### No facets ----
actual_time_plot_s1_nofacet <- turtle_data_s1[,
                                              actual_prop := fifelse(total_neighbours > 0, actual_ingroup_n/total_neighbours, 0),
                                              by = .(sample, tick, who, identity)
][, sample_mean := mean(actual_prop), by = .(sample, tick, identity)
][, SE := sd(actual_prop)/sqrt(length(unique(sample))), by = .(tick, identity)
][, Mean := mean(actual_prop), by = .(tick, identity)] |>
  ggplot(aes(y = Mean, x = tick)) +
  geom_ribbon(alpha = rib_alpha, aes(ymin = Mean-SE, ymax = Mean+SE, fill = identity)) +
  geom_line(aes(color = identity), linewidth = m_line) +
  geom_line(alpha = s_alpha, linewidth = s_line, aes(y = sample_mean, color = identity, group = interaction(sample, identity))) +
  scale_color_manual(values = s1_cols) +
  scale_fill_manual(values = s1_cols) +
  labs(y = "Proportion of neighbouring agents", x = "Number of iterations",
       color = "Political \nidentity", fill = "Political \nidentity",
       title = "S1: Proportion of neighbours that are ingroup over time \nby political identity",
       caption = "Solid lines indicates identity-level mean proportion across samples.
       \nFaint lines indicates identity-level sample mean proportions.
       \nRibbons indicate standard error between mean sample proportions.") +
  ylim(0, 1) +
  x_time +
  base_theme + theme(legend.position = "top")

### Identity facets ----
actual_time_plot_s1_faceted <- actual_time_plot_s1_nofacet + facet_wrap(~identity)

ggsave(plot = actual_time_plot_s1_faceted, filename = "s1_actual_ingroup_time_faceted.png",
       path = str_c(main_dir, "/figures"), width = 16, height = 10, units = "cm", dpi = 1200)
ggsave(plot = actual_time_plot_s1_nofacet, filename = "s1_actual_ingroup_time_nofacet.png",
       path = str_c(main_dir, "/figures"), width = 16, height = 10, units = "cm", dpi = 1200)
save(actual_time_plot_s1_faceted, file = str_c(main_dir, "/figures/ggplot/s1_actual_ingroup_time_faceted_ggplot.RData"))
save(actual_time_plot_s1_nofacet, file = str_c(main_dir, "/figures/ggplot/s1_actual_ingroup_time_nofacet_ggplot.RData"))

rm(actual_time_plot_s1_faceted, actual_time_plot_s1_nofacet)
gc()

## ---- Actual ingroup aggregated ----
actual_agg_plot_s1 <- turtle_data_s1[,
                                     actual_prop := fifelse(total_neighbours > 0, actual_ingroup_n/total_neighbours, 0),
                                     by = .(sample, tick, who, identity)
][, .(sample_mean = mean(actual_prop)), by = .(sample, who, identity)] |>
  ggbetween_cus(x = identity, y = sample_mean,
                title   = "S1: Proportion of actual ingroup by political identity",
                caption = "Individual points indicate agent-level proportions averaged over time.
                \nn indicates the number of agents across samples.",
                ggtheme = base_theme + theme(legend.position = "top"),
                ylab    = "Proportion of neighbouring agents",
                xlab    = "Political identity",
                ggplot.component = list(scale_color_manual(values = s1_cols), ylim(0, 1))) +
  geom_point(data = turtle_data_s1[, .(sample_mean = mean(fifelse(total_neighbours > 0,
                                                                  actual_ingroup_n/total_neighbours, 0))),
                                   by = .(sample, identity)],
             size = s_m_p_size, alpha = s_m_p_alpha, shape = s_m_p_shape,
             aes(colour = identity), position = position_nudge(x = s_m_p_nudge))

ggsave(filename = "s1_actual_ingroup_agg.png", path = str_c(main_dir, "/figures"),
       width = 16, height = 10, units = "cm", dpi = 1200)
save(actual_agg_plot_s1, file = str_c(main_dir, "/figures/ggplot/s1_actual_ingroup_agg_ggplot.RData"))

rm(actual_agg_plot_s1)
gc()

## ---- Accuracy over time ----
### No facets ----
accuracy_time_plot_s1_nofacet <- turtle_data_s1[,
                                                .(accuracy = mean(d_prime_ingroup)),
                                                by = .(sample, tick, identity, who)
][, sample_mean := mean(accuracy), by = .(sample, tick, identity)
][, SE := sd(accuracy)/sqrt(length(unique(sample))), by = .(tick, identity)
][, Mean := mean(accuracy), by = .(tick, identity)] |>
  ggplot(aes(y = Mean, x = tick)) +
  geom_ribbon(alpha = rib_alpha, aes(ymin = Mean-SE, ymax = Mean+SE, fill = identity)) +
  geom_line(aes(color = identity), linewidth = m_line) +
  geom_line(alpha = s_alpha, linewidth = s_line, aes(y = sample_mean, color = identity, group = interaction(sample, identity))) +
  scale_color_manual(values = s1_cols) +
  scale_fill_manual(values = s1_cols) +
  labs(y = "Mean accuracy", x = "Number of iterations",
       color = "Political \nidentity", fill = "Political \nidentity",
       title = "S1: Accuracy of ingroup inference over time by political identity",
       caption = "Accuracy is the difference between correctly inferred ingroup members and falsely identified ingroup members.
       \nSolid lines indicates identity-level mean proportion across samples.
       \nFaint lines indicates identity-level sample mean proportions.
       \nRibbons indicate standard error between mean sample proportions.") +
  ylim(-1, 1) +
  x_time +
  base_theme + theme(legend.position = "top")

### Identity facets ----
accuracy_time_plot_s1_faceted <- accuracy_time_plot_s1_nofacet + facet_wrap(~identity)

ggsave(plot = accuracy_time_plot_s1_faceted, filename = "s1_accuracy_time_faceted.png",
       path = str_c(main_dir, "/figures"), width = 16, height = 10, units = "cm", dpi = 1200)
ggsave(plot = accuracy_time_plot_s1_nofacet, filename = "s1_accuracy_time_nofacet.png",
       path = str_c(main_dir, "/figures"), width = 16, height = 10, units = "cm", dpi = 1200)
save(accuracy_time_plot_s1_faceted, file = str_c(main_dir, "/figures/ggplot/s1_accuracy_time_faceted_ggplot.RData"))
save(accuracy_time_plot_s1_nofacet, file = str_c(main_dir, "/figures/ggplot/s1_accuracy_time_nofacet_ggplot.RData"))

rm(accuracy_time_plot_s1_faceted, accuracy_time_plot_s1_nofacet)
gc()

## ---- Accuracy aggregated ----
accuracy_agg_plot_s1 <-
  turtle_data_s1[, .(accuracy = mean(d_prime_ingroup)), by = .(sample, identity, who)] |>
  ggbetween_cus(x = identity, y = accuracy,
                title   = "S1: Accuracy of ingroup inference by political identity",
                caption = "Accuracy is the difference between correctly inferred ingroup members and falsely identified ingroup members.
                \nIndividual points indicates agent-level proportions averaged across iterations.
                \nn indicates the number of agents.",
                ggtheme = base_theme + theme(legend.position = "top"),
                ylab    = "Mean accuracy of ingroup inferences",
                xlab    = "Political identity",
                ggplot.component = list(scale_color_manual(values = s1_cols), ylim(-1, 1))) +
  geom_point(data = turtle_data_s1[, .(accuracy = mean(d_prime_ingroup)), by = .(sample, identity)],
             size = s_m_p_size, alpha = s_m_p_alpha, shape = s_m_p_shape,
             aes(colour = identity), position = position_nudge(x = s_m_p_nudge))

ggsave(filename = "s1_accuracy_agg.png", path = str_c(main_dir, "/figures"),
       width = 16, height = 10, units = "cm", dpi = 1200)
save(accuracy_agg_plot_s1, file = str_c(main_dir, "/figures/ggplot/s1_accuracy_agg_ggplot.RData"))

rm(accuracy_agg_plot_s1)
gc()

## ---- Cluster homogeneity over time ----
cluster_participation_s1 <- turtle_data[, .(
  proportion_in_cluster = paste0(round(sum(cluster != "no_cluster") / .N * 100, 2), "%")
), by = .(identity)]

homog_turtle_data_s1 <- turtle_data[cluster != "no_cluster",
][, "cluster_size" := .N, by = .(sample, tick, cluster)
][, .(cluster_homog = .N/cluster_size), by = .(sample, tick, cluster, identity)]

### No facets ----
homog_time_plot_s1_nofacet <- homog_turtle_data_s1[, mean_homog := mean(cluster_homog), by = .(tick, identity)
][, mean_homog_sample := mean(cluster_homog), by = .(sample, tick, identity, mean_homog)
][, SE := sd(cluster_homog)/sqrt(length(unique(sample))), by = .(tick, identity)] |>
  ggplot(aes(y = mean_homog, x = tick)) +
  geom_ribbon(alpha = rib_alpha, aes(ymin = mean_homog-SE, ymax = mean_homog+SE, fill = identity)) +
  geom_line(aes(color = identity), linewidth = m_line) +
  geom_line(alpha = s_alpha, linewidth = s_line, aes(y = mean_homog_sample, color = identity, group = interaction(sample, identity))) +
  scale_color_manual(values = s1_cols) +
  scale_fill_manual(values = s1_cols) +
  labs(y = "Proportion of identity in cluster", x = "Number of iterations",
       color = "Political identity", fill = "Political identity",
       title = "S1: Proportion of each identity in clusters over time by political identity",
       caption = paste("Across time points:", cluster_participation_s1[1,2], "of conservative agents and",
                       cluster_participation_s1[2,2], "of liberal agents were in a cluster.",
                       "\nSolid lines indicates identity-level mean proportion across samples.
       \nFaint lines indicates identity-level sample mean proportions.
       \nRibbons indicate standard error between mean sample proportions.")) +
  ylim(0, 1) +
  guides(linetype = "none") +
  x_time +
  base_theme + theme(legend.position = "top")

ggsave(plot = homog_time_plot_s1_nofacet, filename = "s1_homog_time_nofacet.png",
       path = str_c(main_dir, "/figures"), width = 16, height = 10, units = "cm", dpi = 1200)
save(homog_time_plot_s1_nofacet, file = str_c(main_dir, "/figures/ggplot/s1_homog_time_nofacet_ggplot.RData"))

rm(homog_time_plot_s1_nofacet)
gc()

## ---- Cluster homogeneity aggregated ----
homog_agg_plot_s1 <- homog_turtle_data_s1[, .(cluster_homog_mean = mean(cluster_homog)),
                                          by = .(sample, identity, cluster)] |>
  ggbetween_homog_cus(x = identity, y = cluster_homog_mean,
                title   = "S1: Cluster homogeneity by political identity",
                caption = paste("Cluster homogeneity is the proportion of agents in a cluster that share the same identity.
                \nIndividual points indicates cluster-level proportions averaged across iterations.
                \nn indicates the number of clusters.
                \nAcross time points:", cluster_participation_s1[1,2], "of conservative agents and",
                                cluster_participation_s1[2,2], "of liberal agents were in a cluster."),
                ggtheme = base_theme + theme(legend.position = "top"),
                ylab    = "Mean cluster homogeneity",
                xlab    = "Political identity",
                ggplot.component = list(scale_color_manual(values = s1_cols), ylim(0, 1))) +
  geom_point(data = homog_turtle_data_s1[, .(cluster_homog_mean = mean(cluster_homog)),
                                         by = .(sample, identity)],
             size = s_m_p_size, alpha = s_m_p_alpha, shape = s_m_p_shape,
             aes(colour = identity), position = position_nudge(x = s_m_p_nudge))

ggsave(filename = "s1_homog_agg.png", path = str_c(main_dir, "/figures"),
       width = 16, height = 10, units = "cm", dpi = 1200)
save(homog_agg_plot_s1, file = str_c(main_dir, "/figures/ggplot/s1_homog_agg_ggplot.RData"))

rm(homog_agg_plot_s1)
rm(homog_turtle_data_s1)
gc()

## ---- Proportion of iterations in cluster over time and aggregated (S1) ----

# Add binary cluster membership indicator to turtle_data_s1
turtle_data_s1[, bi_clustered := as.integer(cluster != "no_cluster")]

### Over time - no facets ----
prop_clustered_time_plot_s1_nofacet <-
  turtle_data_s1[, clustered_sample_prop := sum(bi_clustered)/.N, by = .(sample, identity, tick)
  ][, c("clustered_prop", "clustered_SE") := .(
    mean(clustered_sample_prop),
    sd(clustered_sample_prop)/sqrt(length(unique(sample)))
  ), by = .(identity, tick)] |>
  ggplot(aes(y = clustered_prop, x = tick)) +
  geom_ribbon(alpha = rib_alpha, aes(ymin = clustered_prop - clustered_SE,
                                     ymax = clustered_prop + clustered_SE, fill = identity)) +
  geom_line(aes(color = identity), linewidth = m_line) +
  geom_line(alpha = s_alpha, linewidth = s_line, aes(y = clustered_sample_prop, color = identity,
                                                     group = interaction(sample, identity))) +
  scale_color_manual(values = s1_cols) +
  scale_fill_manual(values = s1_cols) +
  labs(y = "Proportion of agents in cluster", x = "Number of iterations",
       color = "Political \nidentity", fill = "Political \nidentity",
       title = "S1: Mean proportion of agents in a cluster over time by political identity",
       caption = "Solid line indicates mean across samples and faint lines indicate sample means.
       \nRibbon indicates standard error between samples.") +
  ylim(0, 1) +
  x_time +
  base_theme + theme(legend.position = "top")

### Over time - identity facets ----
prop_clustered_time_plot_s1_faceted <- prop_clustered_time_plot_s1_nofacet + facet_wrap(~identity)

ggsave(plot = prop_clustered_time_plot_s1_faceted, filename = "s1_prop_clustered_time_faceted.png",
       path = str_c(main_dir, "/figures"), width = 16, height = 10, units = "cm", dpi = 1200)
ggsave(plot = prop_clustered_time_plot_s1_nofacet, filename = "s1_prop_clustered_time_nofacet.png",
       path = str_c(main_dir, "/figures"), width = 16, height = 10, units = "cm", dpi = 1200)
save(prop_clustered_time_plot_s1_faceted,
     file = str_c(main_dir, "/figures/ggplot/s1_prop_clustered_time_faceted_ggplot.RData"))
save(prop_clustered_time_plot_s1_nofacet,
     file = str_c(main_dir, "/figures/ggplot/s1_prop_clustered_time_nofacet_ggplot.RData"))

rm(prop_clustered_time_plot_s1_faceted, prop_clustered_time_plot_s1_nofacet)
gc()

### Aggregated ----
prop_clustered_agg_plot_s1 <-
  turtle_data_s1[, .(prop_clustered = sum(bi_clustered)/.N), by = .(who, sample, identity)] |>
  ggbetween_cus(x = identity, y = prop_clustered,
                title   = "S1: Proportion of iterations an agent is in a cluster by political identity",
                caption = "Individual points indicate agent-level proportion of iterations spent in a cluster.
                \nn indicates the number of agents across samples.
                \nDiamonds indicate sample means.",
                ggtheme = base_theme + theme(legend.position = "none"),
                ylab    = "Proportion of iterations in cluster",
                xlab    = "Political identity",
                ggplot.component = list(scale_color_manual(values = s1_cols), ylim(0, 1))) +
  geom_point(data = turtle_data_s1[, .(prop_clustered = sum(bi_clustered)/.N), by = .(sample, identity)],
             size = s_m_p_size, alpha = s_m_p_alpha, shape = s_m_p_shape,
             aes(colour = identity), position = position_nudge(x = s_m_p_nudge)) +
  scale_fill_manual(values = s1_cols) +
  scale_color_manual(values = s1_cols) +
  labs(x = "Political identity", y = "Proportion of iterations in cluster")

ggsave(filename = "s1_prop_clustered_agg.png", path = str_c(main_dir, "/figures"),
       width = 16, height = 10, units = "cm", dpi = 1200)
save(prop_clustered_agg_plot_s1,
     file = str_c(main_dir, "/figures/ggplot/s1_prop_clustered_agg_ggplot.RData"))

rm(prop_clustered_agg_plot_s1)
rm(turtle_data_s1)
gc()


# === Study 2 only ====

## ---- initialise ----
turtle_data_s2_only <- turtle_data[sample > 150]

## ---- Perceived ingroup over time ----
### No facets ----
perceived_time_plot_s2_only_nofacet <- turtle_data_s2_only[,
                                                 perc_prop := fifelse(total_neighbours > 0, perceived_ingroup_n/total_neighbours, 0),
                                                 by = .(sample, tick, who, identity)
][, sample_mean := mean(perc_prop), by = .(sample, tick, identity)
][, SE := sd(perc_prop)/sqrt(length(unique(sample))), by = .(tick, identity)
][, Mean := mean(perc_prop), by = .(tick, identity)] |>
  ggplot(aes(y = Mean, x = tick)) +
  geom_ribbon(alpha = rib_alpha, aes(ymin = Mean-SE, ymax = Mean+SE, fill = identity)) +
  geom_line(aes(color = identity), linewidth = m_line) +
  geom_line(alpha = s_alpha, linewidth = s_line, aes(y = sample_mean, color = identity, group = interaction(sample, identity))) +
  scale_color_manual(values = s2_cols[3:4]) +
  scale_fill_manual(values = s2_cols[3:4]) +
  labs(y = "Proportion of neighbouring agents", x = "Number of iterations",
       color = "Political \nidentity", fill = "Political \nidentity",
       title = "S2: Proportion of neighbours perceived as ingroup over time \nby political identity",
       caption = "Solid lines indicates identity-level mean proportion across samples.
       \nFaint lines indicates identity-level sample mean proportions.
       \nRibbons indicate standard error between mean sample proportions.") +
  ylim(0, 1) +
  x_time +
  base_theme + theme(legend.position = "top")

### Identity facets ----
perceived_time_plot_s2_only_faceted <- perceived_time_plot_s2_only_nofacet + facet_wrap(~identity)

ggsave(plot = perceived_time_plot_s2_only_faceted, filename = "s2_perceived_ingroup_time_faceted.png",
       path = str_c(main_dir, "/figures"), width = 16, height = 10, units = "cm", dpi = 1200)
ggsave(plot = perceived_time_plot_s2_only_nofacet, filename = "s2_perceived_ingroup_time_nofacet.png",
       path = str_c(main_dir, "/figures"), width = 16, height = 10, units = "cm", dpi = 1200)
save(perceived_time_plot_s2_only_faceted, file = str_c(main_dir, "/figures/ggplot/s2_perceived_ingroup_time_faceted_ggplot.RData"))
save(perceived_time_plot_s2_only_nofacet, file = str_c(main_dir, "/figures/ggplot/s2_perceived_ingroup_time_nofacet_ggplot.RData"))

rm(perceived_time_plot_s2_only_faceted, perceived_time_plot_s2_only_nofacet)
gc()

## ---- Perceived ingroup aggregated ----
perceived_agg_plot_s2_only <- turtle_data_s2_only[,
                                        perc_prop := fifelse(total_neighbours > 0, perceived_ingroup_n/total_neighbours, 0),
                                        by = .(sample, tick, who, identity)
][, .(sample_mean = mean(perc_prop)), by = .(sample, who, identity)] |>
  ggbetween_cus(x = identity, y = sample_mean,
                title   = "S2: Proportion of perceived ingroup by political identity",
                caption = "Individual points indicate agent-level proportions averaged over time.
                \nn indicates the number of agents across samples.",
                ggtheme = base_theme + theme(legend.position = "top"),
                ylab    = "Proportion of neighbouring agents",
                xlab    = "Political identity",
                ggplot.component = list(scale_color_manual(values = s2_cols[3:4]), ylim(0, 1))) +
  geom_point(data = turtle_data_s2_only[, .(sample_mean = mean(fifelse(total_neighbours > 0,
                                                                  perceived_ingroup_n/total_neighbours, 0))),
                                   by = .(sample, identity)],
             size = s_m_p_size, alpha = s_m_p_alpha, shape = s_m_p_shape,
             aes(colour = identity), position = position_nudge(x = s_m_p_nudge))

ggsave(filename = "s2_perceived_ingroup_agg.png", path = str_c(main_dir, "/figures"),
       width = 16, height = 10, units = "cm", dpi = 1200)
save(perceived_agg_plot_s2_only, file = str_c(main_dir, "/figures/ggplot/s2_perceived_ingroup_agg_ggplot.RData"))

rm(perceived_agg_plot_s2_only)
gc()

## ---- Actual ingroup over time ----
### No facets ----
actual_time_plot_s2_only_nofacet <- turtle_data_s2_only[,
                                              actual_prop := fifelse(total_neighbours > 0, actual_ingroup_n/total_neighbours, 0),
                                              by = .(sample, tick, who, identity)
][, sample_mean := mean(actual_prop), by = .(sample, tick, identity)
][, SE := sd(actual_prop)/sqrt(length(unique(sample))), by = .(tick, identity)
][, Mean := mean(actual_prop), by = .(tick, identity)] |>
  ggplot(aes(y = Mean, x = tick)) +
  geom_ribbon(alpha = rib_alpha, aes(ymin = Mean-SE, ymax = Mean+SE, fill = identity)) +
  geom_line(aes(color = identity), linewidth = m_line) +
  geom_line(alpha = s_alpha, linewidth = s_line, aes(y = sample_mean, color = identity, group = interaction(sample, identity))) +
  scale_color_manual(values = s2_cols[3:4]) +
  scale_fill_manual(values = s2_cols[3:4]) +
  labs(y = "Proportion of neighbouring agents", x = "Number of iterations",
       color = "Political \nidentity", fill = "Political \nidentity",
       title = "S2: Proportion of neighbours that are ingroup over time \nby political identity",
       caption = "Solid lines indicates identity-level mean proportion across samples.
       \nFaint lines indicates identity-level sample mean proportions.
       \nRibbons indicate standard error between mean sample proportions.") +
  ylim(0, 1) +
  x_time +
  base_theme + theme(legend.position = "top")

### Identity facets ----
actual_time_plot_s2_only_faceted <- actual_time_plot_s2_only_nofacet + facet_wrap(~identity)

ggsave(plot = actual_time_plot_s2_only_faceted, filename = "s2_actual_ingroup_time_faceted.png",
       path = str_c(main_dir, "/figures"), width = 16, height = 10, units = "cm", dpi = 1200)
ggsave(plot = actual_time_plot_s2_only_nofacet, filename = "s2_actual_ingroup_time_nofacet.png",
       path = str_c(main_dir, "/figures"), width = 16, height = 10, units = "cm", dpi = 1200)
save(actual_time_plot_s2_only_faceted, file = str_c(main_dir, "/figures/ggplot/s2_actual_ingroup_time_faceted_ggplot.RData"))
save(actual_time_plot_s2_only_nofacet, file = str_c(main_dir, "/figures/ggplot/s2_actual_ingroup_time_nofacet_ggplot.RData"))

rm(actual_time_plot_s2_only_faceted, actual_time_plot_s2_only_nofacet)
gc()

## ---- Actual ingroup aggregated ----
actual_agg_plot_s2_only <- turtle_data_s2_only[,
                                     actual_prop := fifelse(total_neighbours > 0, actual_ingroup_n/total_neighbours, 0),
                                     by = .(sample, tick, who, identity)
][, .(sample_mean = mean(actual_prop)), by = .(sample, who, identity)] |>
  ggbetween_cus(x = identity, y = sample_mean,
                title   = "S2: Proportion of actual ingroup by political identity",
                caption = "Individual points indicate agent-level proportions averaged over time.
                \nn indicates the number of agents across samples.",
                ggtheme = base_theme + theme(legend.position = "top"),
                ylab    = "Proportion of neighbouring agents",
                xlab    = "Political identity",
                ggplot.component = list(scale_color_manual(values = s2_cols[3:4]), ylim(0, 1))) +
  geom_point(data = turtle_data_s2_only[, .(sample_mean = mean(fifelse(total_neighbours > 0,
                                                                  actual_ingroup_n/total_neighbours, 0))),
                                   by = .(sample, identity)],
             size = s_m_p_size, alpha = s_m_p_alpha, shape = s_m_p_shape,
             aes(colour = identity), position = position_nudge(x = s_m_p_nudge))

ggsave(filename = "s2_actual_ingroup_agg.png", path = str_c(main_dir, "/figures"),
       width = 16, height = 10, units = "cm", dpi = 1200)
save(actual_agg_plot_s2_only, file = str_c(main_dir, "/figures/ggplot/s2_actual_ingroup_agg_ggplot.RData"))

rm(actual_agg_plot_s2_only)
gc()

## ---- Accuracy over time ----
### No facets ----
accuracy_time_plot_s2_only_nofacet <- turtle_data_s2_only[,
                                                .(accuracy = mean(d_prime_ingroup)),
                                                by = .(sample, tick, identity, who)
][, sample_mean := mean(accuracy), by = .(sample, tick, identity)
][, SE := sd(accuracy)/sqrt(length(unique(sample))), by = .(tick, identity)
][, Mean := mean(accuracy), by = .(tick, identity)] |>
  ggplot(aes(y = Mean, x = tick)) +
  geom_ribbon(alpha = rib_alpha, aes(ymin = Mean-SE, ymax = Mean+SE, fill = identity)) +
  geom_line(aes(color = identity), linewidth = m_line) +
  geom_line(alpha = s_alpha, linewidth = s_line, aes(y = sample_mean, color = identity, group = interaction(sample, identity))) +
  scale_color_manual(values = s2_cols[3:4]) +
  scale_fill_manual(values = s2_cols[3:4]) +
  labs(y = "Mean accuracy", x = "Number of iterations",
       color = "Political \nidentity", fill = "Political \nidentity",
       title = "S2: Accuracy of ingroup inference over time by political identity",
       caption = "Accuracy is the difference between correctly inferred ingroup members and falsely identified ingroup members.
       \nSolid lines indicates identity-level mean proportion across samples.
       \nFaint lines indicates identity-level sample mean proportions.
       \nRibbons indicate standard error between mean sample proportions.") +
  ylim(-1, 1) +
  x_time +
  base_theme + theme(legend.position = "top")

### Identity facets ----
accuracy_time_plot_s2_only_faceted <- accuracy_time_plot_s2_only_nofacet + facet_wrap(~identity)

ggsave(plot = accuracy_time_plot_s2_only_faceted, filename = "s2_accuracy_time_faceted.png",
       path = str_c(main_dir, "/figures"), width = 16, height = 10, units = "cm", dpi = 1200)
ggsave(plot = accuracy_time_plot_s2_only_nofacet, filename = "s2_accuracy_time_nofacet.png",
       path = str_c(main_dir, "/figures"), width = 16, height = 10, units = "cm", dpi = 1200)
save(accuracy_time_plot_s2_only_faceted, file = str_c(main_dir, "/figures/ggplot/s2_accuracy_time_faceted_ggplot.RData"))
save(accuracy_time_plot_s2_only_nofacet, file = str_c(main_dir, "/figures/ggplot/s2_accuracy_time_nofacet_ggplot.RData"))

rm(accuracy_time_plot_s2_only_faceted, accuracy_time_plot_s2_only_nofacet)
gc()

## ---- Accuracy aggregated ----
accuracy_agg_plot_s2_only <-
  turtle_data_s2_only[, .(accuracy = mean(d_prime_ingroup)), by = .(sample, identity, who)] |>
  ggbetween_cus(x = identity, y = accuracy,
                title   = "S2: Accuracy of ingroup inference by political identity",
                caption = "Accuracy is the difference between correctly inferred ingroup members and falsely identified ingroup members.
                \nIndividual points indicates agent-level proportions averaged across iterations.
                \nn indicates the number of agents.",
                ggtheme = base_theme + theme(legend.position = "top"),
                ylab    = "Mean accuracy of ingroup inferences",
                xlab    = "Political identity",
                ggplot.component = list(scale_color_manual(values = s2_cols[3:4]), ylim(-1, 1))) +
  geom_point(data = turtle_data_s2_only[, .(accuracy = mean(d_prime_ingroup)), by = .(sample, identity)],
             size = s_m_p_size, alpha = s_m_p_alpha, shape = s_m_p_shape,
             aes(colour = identity), position = position_nudge(x = s_m_p_nudge))

ggsave(filename = "s2_accuracy_agg.png", path = str_c(main_dir, "/figures"),
       width = 16, height = 10, units = "cm", dpi = 1200)
save(accuracy_agg_plot_s2_only, file = str_c(main_dir, "/figures/ggplot/s2_accuracy_agg_ggplot.RData"))

rm(accuracy_agg_plot_s2_only)
gc()

## ---- Cluster homogeneity over time ----
cluster_participation_s2_only <- turtle_data_s2_only[, .(
  proportion_in_cluster = paste0(round(sum(cluster != "no_cluster") / .N * 100, 2), "%")
), by = .(identity)]

homog_turtle_data_s2_only <- turtle_data_s2_only[cluster != "no_cluster",
][, "cluster_size" := .N, by = .(sample, tick, cluster)
][, .(cluster_homog = .N/cluster_size), by = .(sample, tick, cluster, identity)]

### No facets ----
homog_time_plot_s2_only_nofacet <- homog_turtle_data_s2_only[, mean_homog := mean(cluster_homog), by = .(tick, identity)
][, mean_homog_sample := mean(cluster_homog), by = .(sample, tick, identity, mean_homog)
][, SE := sd(cluster_homog)/sqrt(length(unique(sample))), by = .(tick, identity)] |>
  ggplot(aes(y = mean_homog, x = tick)) +
  geom_ribbon(alpha = rib_alpha, aes(ymin = mean_homog-SE, ymax = mean_homog+SE, fill = identity)) +
  geom_line(aes(color = identity), linewidth = m_line) +
  geom_line(alpha = s_alpha, linewidth = s_line, aes(y = mean_homog_sample, color = identity, group = interaction(sample, identity))) +
  scale_color_manual(values = s2_cols[3:4]) +
  scale_fill_manual(values = s2_cols[3:4]) +
  labs(y = "Proportion of identity in cluster", x = "Number of iterations",
       color = "Political identity", fill = "Political identity",
       title = "S2: Proportion of each identity in clusters over time by political identity",
       caption = paste("Across time points:", cluster_participation_s2_only[1,2], "of conservative agents and",
                       cluster_participation_s2_only[2,2], "of liberal agents were in a cluster.",
                       "\nSolid lines indicates identity-level mean proportion across samples.
       \nFaint lines indicates identity-level sample mean proportions.
       \nRibbons indicate standard error between mean sample proportions.")) +
  ylim(0, 1) +
  guides(linetype = "none") +
  x_time +
  base_theme + theme(legend.position = "top")

ggsave(plot = homog_time_plot_s2_only_nofacet, filename = "s2_homog_time_nofacet.png",
       path = str_c(main_dir, "/figures"), width = 16, height = 10, units = "cm", dpi = 1200)
save(homog_time_plot_s2_only_nofacet, file = str_c(main_dir, "/figures/ggplot/s2_homog_time_nofacet_ggplot.RData"))

rm(homog_time_plot_s2_only_nofacet)
gc()

## ---- Cluster homogeneity aggregated ----
homog_agg_plot_s2_only <- homog_turtle_data_s2_only[, .(cluster_homog_mean = mean(cluster_homog)),
                                          by = .(sample, identity, cluster)] |>
  ggbetween_homog_cus(x = identity, y = cluster_homog_mean,
                title   = "S2: Cluster homogeneity by political identity",
                caption = paste("Cluster homogeneity is the proportion of agents in a cluster that share the same identity.
                \nIndividual points indicates cluster-level proportions averaged across iterations.
                \nn indicates the number of clusters.
                \nAcross time points:", cluster_participation_s2_only[1,2], "of conservative agents and",
                                cluster_participation_s2_only[2,2], "of liberal agents were in a cluster."),
                ggtheme = base_theme + theme(legend.position = "top"),
                ylab    = "Mean cluster homogeneity",
                xlab    = "Political identity",
                ggplot.component = list(scale_color_manual(values = s2_cols[3:4]), ylim(0, 1))) +
  geom_point(data = homog_turtle_data_s2_only[, .(cluster_homog_mean = mean(cluster_homog)),
                                         by = .(sample, identity)],
             size = s_m_p_size, alpha = s_m_p_alpha, shape = s_m_p_shape,
             aes(colour = identity), position = position_nudge(x = s_m_p_nudge))

ggsave(filename = "s2_homog_agg.png", path = str_c(main_dir, "/figures"),
       width = 16, height = 10, units = "cm", dpi = 1200)
save(homog_agg_plot_s2_only, file = str_c(main_dir, "/figures/ggplot/s2_homog_agg_ggplot.RData"))

rm(homog_agg_plot_s2_only)
gc()

## ---- Proportion of iterations in cluster over time and aggregated (S2 only) ----

# Add binary cluster membership indicator to turtle_data_s2_only
turtle_data_s2_only[, bi_clustered := as.integer(cluster != "no_cluster")]

### Over time - no facets ----
prop_clustered_time_plot_s2_only_nofacet <-
  turtle_data_s2_only[, clustered_sample_prop := sum(bi_clustered)/.N, by = .(sample, identity, tick)
  ][, c("clustered_prop", "clustered_SE") := .(
    mean(clustered_sample_prop),
    sd(clustered_sample_prop)/sqrt(length(unique(sample)))
  ), by = .(identity, tick)] |>
  ggplot(aes(y = clustered_prop, x = tick)) +
  geom_ribbon(alpha = rib_alpha, aes(ymin = clustered_prop - clustered_SE,
                                     ymax = clustered_prop + clustered_SE, fill = identity)) +
  geom_line(aes(color = identity), linewidth = m_line) +
  geom_line(alpha = s_alpha, linewidth = s_line, aes(y = clustered_sample_prop, color = identity,
                                                     group = interaction(sample, identity))) +
  scale_color_manual(values = s2_cols[3:4]) +
  scale_fill_manual(values = s2_cols[3:4]) +
  labs(y = "Proportion of agents in cluster", x = "Number of iterations",
       color = "Political \nidentity", fill = "Political \nidentity",
       title = "S2: Mean proportion of agents in a cluster over time by political identity",
       caption = "Solid line indicates mean across samples and faint lines indicate sample means.
       \nRibbon indicates standard error between samples.") +
  ylim(0, 1) +
  x_time +
  base_theme + theme(legend.position = "top")

### Over time - identity facets ----
prop_clustered_time_plot_s2_only_faceted <- prop_clustered_time_plot_s2_only_nofacet + facet_wrap(~identity)

ggsave(plot = prop_clustered_time_plot_s2_only_faceted, filename = "s2_prop_clustered_time_faceted.png",
       path = str_c(main_dir, "/figures"), width = 16, height = 10, units = "cm", dpi = 1200)
ggsave(plot = prop_clustered_time_plot_s2_only_nofacet, filename = "s2_prop_clustered_time_nofacet.png",
       path = str_c(main_dir, "/figures"), width = 16, height = 10, units = "cm", dpi = 1200)
save(prop_clustered_time_plot_s2_only_faceted,
     file = str_c(main_dir, "/figures/ggplot/s2_prop_clustered_time_faceted_ggplot.RData"))
save(prop_clustered_time_plot_s2_only_nofacet,
     file = str_c(main_dir, "/figures/ggplot/s2_prop_clustered_time_nofacet_ggplot.RData"))

rm(prop_clustered_time_plot_s2_only_faceted, prop_clustered_time_plot_s2_only_nofacet)
gc()

### Aggregated ----
prop_clustered_agg_plot_s2_only <-
  turtle_data_s2_only[, .(prop_clustered = sum(bi_clustered)/.N), by = .(who, sample, identity)] |>
  ggbetween_cus(x = identity, y = prop_clustered,
                title   = "S2: Proportion of iterations an agent is in a cluster by political identity",
                caption = "Individual points indicate agent-level proportion of iterations spent in a cluster.
                \nn indicates the number of agents across samples.
                \nDiamonds indicate sample means.",
                ggtheme = base_theme + theme(legend.position = "none"),
                ylab    = "Proportion of iterations in cluster",
                xlab    = "Political identity",
                ggplot.component = list(scale_color_manual(values = s2_cols[3:4]), ylim(0, 1))) +
  geom_point(data = turtle_data_s2_only[, .(prop_clustered = sum(bi_clustered)/.N), by = .(sample, identity)],
             size = s_m_p_size, alpha = s_m_p_alpha, shape = s_m_p_shape,
             aes(colour = identity), position = position_nudge(x = s_m_p_nudge)) +
  scale_fill_manual(values = s2_cols[3:4]) +
  scale_color_manual(values = s2_cols[3:4]) +
  labs(x = "Political identity", y = "Proportion of iterations in cluster")

ggsave(filename = "s2_prop_clustered_agg.png", path = str_c(main_dir, "/figures"),
       width = 16, height = 10, units = "cm", dpi = 1200)
save(prop_clustered_agg_plot_s2_only,
     file = str_c(main_dir, "/figures/ggplot/s2_prop_clustered_agg_ggplot.RData"))

rm(prop_clustered_agg_plot_s2_only)
rm(turtle_data_s2_only, homog_turtle_data_s2_only)
gc()