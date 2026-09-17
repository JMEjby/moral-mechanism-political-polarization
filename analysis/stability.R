script <- "stability"
source("support-functions/local_full_setup.R") # switch to remote_setup.R for remote runs

turtle_data <- turtle_cluster_data[tick > 0 & 
                                     tick <= 201,] [, 
                                                    c("bi_happy", "bi_conflict", "study"):= .(fifelse(unhappy == "true", 0, 1),
                                                                                                  fifelse(conflicted == "true", 1, 0),
                                                                                                  fifelse(sample <= 150, 
                                                                                                          "Study 1", 
                                                                                                          "Study 2")
                                                    )]
turtle_data$identity <- factor(turtle_data$identity, labels = c("Conservative", "Liberal"))
rm(turtle_cluster_data); gc()

# === Study 1 ====

## ---- initialise ----
turtle_data_s1 <- turtle_data[sample <= 150] 

## --- proportion and quantiles of happy agents over time  ----
turtle_data_happy <- turtle_data_s1[
  , "happy_sample_prop" := sum(bi_happy)/.N, 
  by = .(sample, identity, tick)
][
  , "type" := fifelse(mean(happy_sample_prop) > 0.5, 
                      "High satisfaction samples", 
                      fifelse(mean(happy_sample_prop) < 0.25, 
                              "Low satisfaction samples",
                              "Moderate satisfaction samples")), 
  by = .(sample)
][
  , c("happy_prop","SE", "median_happy_prop", "q10", "q20", "q30", "q40", "q60" ,"q70", "q80", "q90") := 
    .(mean(happy_sample_prop),
      sd(happy_sample_prop)/sqrt(length(unique(sample))),
      median(happy_sample_prop),
      quantile(happy_sample_prop, 0.10),
      quantile(happy_sample_prop, 0.20),
      quantile(happy_sample_prop, 0.30),
      quantile(happy_sample_prop, 0.40),
      quantile(happy_sample_prop, 0.60),
      quantile(happy_sample_prop, 0.70),
      quantile(happy_sample_prop, 0.80),
      quantile(happy_sample_prop, 0.90)), 
  by = .(identity, tick)
][
  , "type" := factor(type,
                     levels = c("Low satisfaction samples",
                                "Moderate satisfaction samples",
                                "High satisfaction samples"))
]

### percentile plot ----
#### No facets ----
time_happy_perc_plot_nofacet <-
  ggplot(turtle_data_happy,
         aes(y = median_happy_prop, 
             x= tick))+
  geom_ribbon(alpha = 0.15, aes(ymin = q10, ymax = q90, fill = identity)) +
  geom_ribbon(alpha = 0.25, aes(ymin = q20, ymax = q80, fill = identity)) +
  geom_ribbon(alpha = 0.35, aes(ymin = q30, ymax = q70, fill = identity)) +
  geom_ribbon(alpha = 0.45, aes(ymin = q40, ymax = q60, fill = identity)) +
  geom_line(aes(color = identity), linewidth = m_line)+
  scale_color_manual(values=s1_cols) +
  scale_fill_manual(values=s1_cols) +
  labs(y = "Satisfied agent proportion", x = "Number of iterations", 
       color = "Political \nidentity", fill = "Political \nidentity",
       title = "S1: 10-percentiles of proportion of agents satisfied \nover time by political identity",
       caption = "Solid line indicates median across samples. 
       \nRibbons 10-90%, 20-80%, 30-70%, and 40-60% percentiles of sample proportions.") +
  ylim(0,1)+
  x_time +
  base_theme +
  theme(legend.position = "top")

#### Identity facets ----
time_happy_perc_plot_faceted <- time_happy_perc_plot_nofacet +
  facet_wrap(~identity)

ggsave(plot = time_happy_perc_plot_faceted, filename ="s1_happy_time_perc_faceted.png", path = str_c(main_dir, "/figures"), 
       width =16, height = 10, units = "cm", dpi = 1200)
ggsave(plot = time_happy_perc_plot_nofacet, filename ="s1_happy_time_perc_nofacet.png", path = str_c(main_dir, "/figures"), 
       width =16, height = 10, units = "cm", dpi = 1200)
save(time_happy_perc_plot_faceted, file = str_c(main_dir, "/figures/ggplot/s1_time_happy_perc_faceted_ggplot.RData"))
save(time_happy_perc_plot_nofacet, file = str_c(main_dir, "/figures/ggplot/s1_time_happy_perc_nofacet_ggplot.RData"))
save(turtle_data_happy, file = str_c(main_dir, "/figures/ggplot/s1_time_happy_perc_data.RData"))

rm(time_happy_perc_plot_faceted, time_happy_perc_plot_nofacet)
gc()

### Mean and SE plot ----
time_happy_plot <-
  ggplot(turtle_data_happy,
         aes(y = happy_prop, 
             x= tick))+
  geom_ribbon(alpha = rib_alpha, aes(ymin = happy_prop-SE, ymax = happy_prop+SE, fill = identity)) +
  geom_line(aes(color = identity), linewidth = m_line)+
  geom_line(alpha = s_alpha, linewidth = s_line, aes(y = happy_sample_prop, color = identity, group = interaction(sample, identity)))+
  scale_color_manual(values=s1_cols) +
  scale_fill_manual(values=s1_cols) +
  labs(y = "Satisfied agent proportion", x = "Number of iterations", 
       color = "Political \nidentity", fill = "Political \nidentity",
       title = "S1: Mean proportion of agents satisfied over time by political identity",
       caption = "Solid line indicates mean across samples and faint lines indicate sample means. 
       \nRibbon indicates standard error between samples.")+
  ylim(0,1)+
  x_time +
  base_theme +
  theme(legend.position = "top")

ggsave(filename ="s1_happy_time.png", path = str_c(main_dir, "/figures"), 
       width =16, height = 10, units = "cm", dpi = 1200)
save(time_happy_plot, file = str_c(main_dir, "/figures/ggplot/s1_time_happy_ggplot.RData"))
save(turtle_data_happy, file = str_c(main_dir, "/figures/ggplot/s1_time_happy_data.RData"))

rm(time_happy_plot)
gc()

### plot separating samples with high vs. low happiness ----
time_happy_plot_split <-
  turtle_data_happy[, .(happy_sample_prop=sum(bi_happy)/.N), 
                     by = .(sample, type, tick, identity)][, type_n := .N, 
                                                          by = .(type, tick, identity)] [, type := paste(type, "\n(", type_n, " samples)", sep = "")
                                                          ][, type := factor(type, levels = unique(type[order(match(gsub(" \\(.*", "", type),
                                                                                                                    c("Low satisfaction samples",
                                                                                                                      "Moderate satisfaction samples",
                                                                                                                      "High satisfaction samples")))]))]|>
  ggplot(aes(y = happy_sample_prop, 
             x= tick,
             color = identity))+
  stat_summary(geom = "line", linewidth = m_line, fun = "mean")+
  geom_line(alpha = s_alpha, linewidth = s_line, aes(group = interaction(sample, identity)))+
  facet_wrap(~type)+
  scale_color_manual(values=s1_cols) +
  labs(y = "Satisfied agent proportion", x = "Number of iterations", 
       color = "Political \nidentity", fill = "Political \nidentity",
       title = "S1: Proportion of agents satisfied over time by political identity and level of overall \nsatisfaction",
       caption = "Solid line indicates mean across samples and faint lines indicate sample means.
       \nHigh satisfaction samples have an average satisfaction proportion > 0.5.
       \nModerate satisfaction samples have an average satisfaction proportion betwen 0.5 and 0.25.
       \nLow satisfaction samples have an average satisfaction proportion < 0.25.")+
  ylim(0,1)+
  x_time +
  base_theme +
  theme(legend.position = "top")

ggsave(filename ="s1_happy_time_split.png", path = str_c(main_dir, "/figures"), 
       width =20, height = 12.5, units = "cm", dpi = 1200)
save(time_happy_plot_split, file = str_c(main_dir, "/figures/ggplot/s1_time_happy_split_ggplot.RData"))

rm(time_happy_plot_split)
rm(turtle_data_happy)
gc()

## Stability across time ----
happy_plot <-  
  turtle_data_s1[, .("happy_prop" = sum(bi_happy)/.N),
                 by = .(who, sample, identity)] |>
  ggbetween_cus(x = identity, y = happy_prop,
                 title = "S1: Proportion of satisfied iterations across iterations by political \nidentity",
                 caption = "Individual points indicates agent-level satisfaction proportion.
                 \n n indicates the number of agents in each identity group across samples.
                 \n Diamonds indicate sample means.") +
  geom_point(data = turtle_data[, .("happy_prop" = sum(bi_happy)/.N),
                                by = .(sample, identity)], size = s_m_p_size, alpha = s_m_p_alpha, shape = s_m_p_shape,
             aes(colour = identity), position = position_nudge(x = s_m_p_nudge)
  )+
  ylim(0,1)+
  scale_fill_manual(values=s1_cols) +
  scale_color_manual(values=s1_cols) +
  labs(x = "Political identity", y ="Satisfied agent proportion")+
  base_theme +
  theme(legend.position = "none")

ggsave(filename ="s1_happy_agg.png", path = str_c(main_dir, "/figures"), width =16, height = 10, units = "cm", dpi = 1200)
save(happy_plot, file = str_c(main_dir, "/figures/ggplot/s1_happy_ggplot.RData"))
rm(happy_plot, turtle_data_s1); gc()

# ## Intermed plots (proportion happy by perceived ingroup members and conflict) ----
# conf_labels <- c("Never/rarely (<25 %)", "Sometimes (25-50 %)", "Often (50-75 %)","Mostly (75-100 %)")
# 
# perc_lab <-c("None/very few (<25 %)", "Few (25-50 %)", "Some (50-75 %)","A lot (75-100 %)")
# 
# breaks <- 0.25*(0:4)
# breaks[1] <- breaks[1]-0.01
# 
# happy_intermed_plot <- 
#   turtle_data_s1[, .("conflict_prop" = sum(bi_conflict)/sum(action_taken != "NC"), 
#                      "perceived_ingroup_prop" = sum(perceived_ingroup_n/8)/100,
#                      happy_prop = sum(bi_happy)/100), 
#                  by = .(sample, 
#                         who, 
#                         identity
#                  )] [, c("conflict_bin", 
#                          "ingroup_bin") := .(
#                            factor(cut(conflict_prop, breaks = breaks), 
#                                   labels = conf_labels), 
#                            factor(cut(perceived_ingroup_prop, breaks =breaks), 
#                                   labels =perc_lab)
#                          )] [, n_group := length(happy_prop), 
#                              by =.(identity, 
#                                    conflict_bin, 
#                                    ingroup_bin) ] |> 
#   ggplot(aes(x = identity, y = happy_prop, fill = identity, color = identity)) + 
#   geom_jitter(alpha = .2, height = 0)+
#   geom_violin(alpha = 0.5, color = "black") +
#   geom_boxplot(width=0.1, color = "black") +
#   facet_grid(rows = vars(conflict_bin), cols = vars(ingroup_bin), margins = T)+
#   ylim(0,1)+
#   scale_fill_manual(values=s1_cols) +
#   scale_color_manual(values=s1_cols) +
#   labs(x = "Political identity", y ="Proportion of satisfied iterations")+
#   theme_ggstatsplot() +
#   theme(panel.background = element_rect(fill = "white", color = "black", linetype ="solid"),
#         strip.background = element_rect(fill = "white", color= "black"),
#         axis.line = element_line(color = "black"),
#         axis.ticks = element_line(color = "black"),
#         legend.position = "top",
#         legend.background = element_rect(fill = "white", color = "black"))
# 
# ggsave(filename ="s1_happy_intermed.png", path = str_c(main_dir, "/figures"), width =18, height = 19, units = "cm", dpi = 1200)
# save(happy_intermed_plot, file = str_c(main_dir, "/figures/ggplot/s1_intermed_happy_ggplot.RData"))
# rm(happy_intermed_plot)
# gc()

# === Study 2 only ====

turtle_data_s2_only <- turtle_data[sample > 150]

turtle_data_happy_s2 <- turtle_data_s2_only[
  , "happy_sample_prop" := sum(bi_happy)/.N,
  by = .(sample, identity, tick)
][
  , "type" := fifelse(mean(happy_sample_prop) > 0.5,
                      "High satisfaction samples",
                      fifelse(mean(happy_sample_prop) < 0.25,
                              "Low satisfaction samples",
                              "Moderate satisfaction samples")),
  by = .(sample)
][
  , c("happy_prop","SE", "median_happy_prop", "q10", "q20", "q30", "q40", "q60" ,"q70", "q80", "q90") :=
    .(mean(happy_sample_prop),
      sd(happy_sample_prop)/sqrt(length(unique(sample))),
      median(happy_sample_prop),
      quantile(happy_sample_prop, 0.10),
      quantile(happy_sample_prop, 0.20),
      quantile(happy_sample_prop, 0.30),
      quantile(happy_sample_prop, 0.40),
      quantile(happy_sample_prop, 0.60),
      quantile(happy_sample_prop, 0.70),
      quantile(happy_sample_prop, 0.80),
      quantile(happy_sample_prop, 0.90)),
  by = .(identity, tick)
][
  , "type" := factor(type,
                     levels = c("Low satisfaction samples",
                                "Moderate satisfaction samples",
                                "High satisfaction samples"))
]

### percentile plot ----
#### No facets ----
time_happy_perc_plot_s2_nofacet <-
  ggplot(turtle_data_happy_s2,
         aes(y = median_happy_prop,
             x= tick))+
  geom_ribbon(alpha = 0.15, aes(ymin = q10, ymax = q90, fill = identity)) +
  geom_ribbon(alpha = 0.25, aes(ymin = q20, ymax = q80, fill = identity)) +
  geom_ribbon(alpha = 0.35, aes(ymin = q30, ymax = q70, fill = identity)) +
  geom_ribbon(alpha = 0.45, aes(ymin = q40, ymax = q60, fill = identity)) +
  geom_line(aes(color = identity), linewidth = m_line)+
  scale_color_manual(values=s2_cols[3:4]) +
  scale_fill_manual(values=s2_cols[3:4]) +
  labs(y = "Satisfied agent proportion", x = "Number of iterations",
       color = "Political \nidentity", fill = "Political \nidentity",
       title = "S2: 10-percentiles of proportion of agents satisfied \nover time by political identity",
       caption = "Solid line indicates median across samples.
       \nRibbons 10-90%, 20-80%, 30-70%, and 40-60% percentiles of sample proportions.") +
  ylim(0,1)+
  x_time +
  base_theme +
  theme(legend.position = "top")

#### Identity facets ----
time_happy_perc_plot_s2_faceted <- time_happy_perc_plot_s2_nofacet +
  facet_wrap(~identity)

ggsave(plot = time_happy_perc_plot_s2_faceted, filename ="s2_happy_time_perc_faceted.png", path = str_c(main_dir, "/figures"),
       width =16, height = 10, units = "cm", dpi = 1200)
ggsave(plot = time_happy_perc_plot_s2_nofacet, filename ="s2_happy_time_perc_nofacet.png", path = str_c(main_dir, "/figures"),
       width =16, height = 10, units = "cm", dpi = 1200)
save(time_happy_perc_plot_s2_faceted, file = str_c(main_dir, "/figures/ggplot/s2_time_happy_perc_faceted_ggplot.RData"))
save(time_happy_perc_plot_s2_nofacet, file = str_c(main_dir, "/figures/ggplot/s2_time_happy_perc_nofacet_ggplot.RData"))

rm(time_happy_perc_plot_s2_faceted, time_happy_perc_plot_s2_nofacet)
gc()

### Mean and SE plot ----
time_happy_plot_s2 <-
  ggplot(turtle_data_happy_s2,
         aes(y = happy_prop,
             x= tick))+
  geom_ribbon(alpha = rib_alpha, aes(ymin = happy_prop-SE, ymax = happy_prop+SE, fill = identity)) +
  geom_line(aes(color = identity), linewidth = m_line)+
  geom_line(alpha = s_alpha, linewidth = s_line, aes(y = happy_sample_prop, color = identity, group = interaction(sample, identity)))+
  scale_color_manual(values=s2_cols[3:4]) +
  scale_fill_manual(values=s2_cols[3:4]) +
  labs(y = "Satisfied agent proportion", x = "Number of iterations",
       color = "Political \nidentity", fill = "Political \nidentity",
       title = "S2: Mean proportion of agents satisfied over time by political identity",
       caption = "Solid line indicates mean across samples and faint lines indicate sample means.
       \nRibbon indicates standard error between samples.")+
  ylim(0,1)+
  x_time +
  base_theme +
  theme(legend.position = "top")

ggsave(filename ="s2_happy_time.png", path = str_c(main_dir, "/figures"),
       width =16, height = 10, units = "cm", dpi = 1200)
save(time_happy_plot_s2, file = str_c(main_dir, "/figures/ggplot/s2_time_happy_ggplot.RData"))

rm(time_happy_plot_s2)
gc()

### plot separating samples with high vs. low happiness ----
time_happy_plot_split_s2 <-
  turtle_data_happy_s2[, .(happy_sample_prop=sum(bi_happy)/.N),
                       by = .(sample, type, tick, identity)][, type_n := .N,
                                                             by = .(type, tick, identity)] [, type := paste(type, "\n(", type_n, " samples)", sep = "")
                                                             ][, type := factor(type, levels = unique(type[order(match(gsub(" \\(.*", "", type),
                                                                                                                       c("Low satisfaction samples",
                                                                                                                         "Moderate satisfaction samples",
                                                                                                                         "High satisfaction samples")))]))]|>
  ggplot(aes(y = happy_sample_prop,
             x= tick,
             color = identity))+
  stat_summary(geom = "line", linewidth = m_line, fun = "mean")+
  geom_line(alpha = s_alpha, linewidth = s_line, aes(group = interaction(sample, identity)))+
  facet_wrap(~type)+
  scale_color_manual(values=s2_cols[3:4]) +
  labs(y = "Satisfied agent proportion", x = "Number of iterations",
       color = "Political \nidentity", fill = "Political \nidentity",
       title = "S2: Proportion of agents satisfied over time by political identity and level of overall \nsatisfaction",
       caption = "Solid line indicates mean across samples and faint lines indicate sample means.
       \nHigh satisfaction samples have an average satisfaction proportion > 0.5.
       \nModerate satisfaction samples have an average satisfaction proportion betwen 0.5 and 0.25.
       \nLow satisfaction samples have an average satisfaction proportion < 0.25.")+
  ylim(0,1)+
  x_time +
  base_theme +
  theme(legend.position = "top")

ggsave(filename ="s2_happy_time_split.png", path = str_c(main_dir, "/figures"),
       width =20, height = 12.5, units = "cm", dpi = 1200)
save(time_happy_plot_split_s2, file = str_c(main_dir, "/figures/ggplot/s2_time_happy_split_ggplot.RData"))

rm(time_happy_plot_split_s2)
gc()

## Stability across time ----
happy_plot_s2 <- turtle_data_s2_only[, .("happy_prop" = sum(bi_happy)/.N),
                                     by = .(who, sample, identity)] |>
  ggbetween_cus(x = identity, y = happy_prop,
                title = "S2: Proportion of satisfied iterations across iterations by political \nidentity",
                caption = "Individual points indicates agent-level satisfaction proportion.
                 \n n indicates the number of agents in each identity group across samples.
                 \n Diamonds indicate sample means.") +
  geom_point(data = turtle_data_s2_only[, .("happy_prop" = sum(bi_happy)/.N),
                                        by = .(sample, identity)], size = s_m_p_size, alpha = s_m_p_alpha, shape = s_m_p_shape,
             aes(colour = identity), position = position_nudge(x = s_m_p_nudge)
  )+
  ylim(0,1)+
  scale_fill_manual(values=s2_cols[3:4]) +
  scale_color_manual(values=s2_cols[3:4]) +
  labs(x = "Political identity", y ="Satisfied agent proportion")+
  base_theme +
  theme(legend.position = "none")

ggsave(filename ="s2_happy_agg.png", path = str_c(main_dir, "/figures"), width =16, height = 10, units = "cm", dpi = 1200)
save(happy_plot_s2, file = str_c(main_dir, "/figures/ggplot/s2_happy_ggplot.RData"))

rm(turtle_data_s2_only, turtle_data_happy_s2)
gc()

# === Study 2 ====
## ---- Proportion of happy agents over time  ----
turtle_data_happy <- turtle_data[, "happy_sample_prop" := sum(bi_happy)/.N, 
                                 by = .(sample,
                                        study,
                                        identity, 
                                        tick
                                 )] [,"type" := fifelse(mean(happy_sample_prop) > 0.5, 
                                                        "High satisfaction samples", 
                                                        fifelse(mean(happy_sample_prop) < 0.25, 
                                                                "Low satisfaction samples",
                                                                "Moderate satisfaction samples")), 
                                     by = .(sample,
                                            study
                                     )] [
                                       , c("happy_prop","SE", "median_happy_prop", "q10", "q20", "q30", "q40", "q60" ,"q70", "q80", "q90") := 
                                         .(mean(happy_sample_prop),
                                           sd(happy_sample_prop)/sqrt(length(unique(sample))),
                                           median(happy_sample_prop),
                                           quantile(happy_sample_prop, 0.10),
                                           quantile(happy_sample_prop, 0.20),
                                           quantile(happy_sample_prop, 0.30),
                                           quantile(happy_sample_prop, 0.40),
                                           quantile(happy_sample_prop, 0.60),
                                           quantile(happy_sample_prop, 0.70),
                                           quantile(happy_sample_prop, 0.80),
                                           quantile(happy_sample_prop, 0.90)),
                                       by = .(identity,
                                              study,
                                              tick
                                       )] [, "type" := factor(type,
                                                              levels = c(
                                                                "Low satisfaction samples",
                                                                "Moderate satisfaction samples",
                                                                "High satisfaction samples"))]  


### Percentile plot ----
#### No facets ----
time_happy_perc_plot_nofacet <-
  ggplot(turtle_data_happy,
         aes(y = median_happy_prop, 
             x = tick))+
  geom_ribbon(alpha = 0.15, aes(ymin = q10, ymax = q90, fill = interaction(identity, study, sep = "\n"))) +
  geom_ribbon(alpha = 0.25, aes(ymin = q20, ymax = q80, fill = interaction(identity, study, sep = "\n"))) +
  geom_ribbon(alpha = 0.35, aes(ymin = q30, ymax = q70, fill = interaction(identity, study, sep = "\n"))) +
  geom_ribbon(alpha = 0.45, aes(ymin = q40, ymax = q60, fill = interaction(identity, study, sep = "\n"))) +
  # by inv level
  geom_line(aes(color = interaction(identity, study, sep = "\n"), linetype = study), linewidth = m_line)+
  scale_color_manual(values=s2_cols) +
  scale_fill_manual(values=s2_cols) +
  scale_linetype_manual(values = s2_lines) +
  labs(y = "Satisfied agent proportion", x = "Number of iterations", 
       color = "Political \nidentity by \nstudy", fill = "Political \nidentity by \nstudy",
       title = "S2: 10-percentiles of proportion of agents satisfied over time by political \nidentity and invariance condition",
       caption = "Solid line indicates median across samples. 
       \nRibbons 10-90%, 20-80%, 30-70%, and 40-60% percentiles of sample proportions.")+
  ylim(0,1)+
  guides(linetype = "none") +
  x_time +
  base_theme +
  theme(legend.position = "top")

#### full grid ----
time_happy_perc_plot_grid <- time_happy_perc_plot_nofacet +
  facet_grid(identity~study)

#### rows only ----
time_happy_perc_plot_rows <- time_happy_perc_plot_nofacet +
  facet_wrap(~identity) 

#### columns only ----
time_happy_perc_plot_cols <- time_happy_perc_plot_nofacet +
  facet_wrap(~study) 

ggsave(plot = time_happy_perc_plot_grid, filename ="s1_s2_happy_time_perc_grid.png", path = str_c(main_dir, "/figures"),
       width =16, height = 15, units = "cm", dpi = 1200)
ggsave(plot = time_happy_perc_plot_rows, filename ="s1_s2_happy_time_perc_rows.png",
       path = str_c(main_dir, "/figures"), width =16, height = 10, units = "cm", dpi = 1200)
ggsave(plot = time_happy_perc_plot_cols, filename ="s1_s2_happy_time_perc_cols.png", path = str_c(main_dir, "/figures"),
       width =16, height = 10, units = "cm", dpi = 1200)
ggsave(plot = time_happy_perc_plot_nofacet, filename ="s1_s2_happy_time_perc_nofacet.png", path = str_c(main_dir, "/figures"), width =16, height = 10, units = "cm", dpi = 1200)
save(time_happy_perc_plot_grid, file = str_c(main_dir, "/figures/ggplot/s1_s2_time_happy_perc_grid_ggplot.RData"))
save(time_happy_perc_plot_rows, file = str_c(main_dir, "/figures/ggplot/s1_s2_time_happy_perc_rows_ggplot.RData"))
save(time_happy_perc_plot_cols, file = str_c(main_dir, "/figures/ggplot/s1_s2_time_happy_perc_cols_ggplot.RData"))
save(time_happy_perc_plot_nofacet, file = str_c(main_dir, "/figures/ggplot/s1_s2_time_happy_perc_nofacet_ggplot.RData"))
save(turtle_data_happy, file = str_c(main_dir, "/figures/ggplot/s1_s2_time_happy_perc_data.RData"))

rm(time_happy_perc_plot_grid, time_happy_perc_plot_rows, time_happy_perc_plot_cols, time_happy_perc_plot_nofacet)
gc()

### mean and SE plot ----
#### no facets ----
time_happy_plot_nofacet <-  
  ggplot(turtle_data_happy,
         aes(y = happy_prop, 
             x = tick))+
  geom_ribbon(alpha = rib_alpha, aes(ymin = happy_prop-SE, ymax = happy_prop+SE, fill = interaction(identity, study, sep = "\n"))) +
  # by inv level
  geom_line(aes(color = interaction(identity, study, sep = "\n"), linetype = study), linewidth = m_line)+
  #
  geom_line(alpha = s_alpha, linewidth = s_line, aes(y = happy_sample_prop,  colour = interaction(identity, study, sep = "\n"), group = interaction(sample, identity), linetype = study))+
  scale_color_manual(values=s2_cols) +
  scale_fill_manual(values=s2_cols) +
  scale_linetype_manual(values = s2_lines) +
  labs(y = "Satisfied agent proportion", x = "Number of iterations", 
       color = "Political \nidentity by \nstudy", fill = "Political \nidentity by \nstudy",
       title = "S2: Mean proportion of agents satisfied over time by political identity and \ninvariance condition",
       caption = "Solid line indicates mean across samples and faint lines indicate sample means. 
       \nRibbon indicates standard error between samples.")+
  ylim(0,1)+
  guides(linetype = "none") +
  x_time +
  base_theme +
  theme(legend.position = "top")

#### identity facets ----
time_happy_plot_faceted <- time_happy_plot_nofacet +
  facet_wrap(~study)

ggsave(plot = time_happy_plot_faceted, filename ="s1_s2_happy_time_faceted.png", path = str_c(main_dir, "/figures"),
       width =16, height = 10, units = "cm", dpi = 1200)
ggsave(plot = time_happy_plot_nofacet, filename ="s1_s2_happy_time_nofacet.png", path = str_c(main_dir, "/figures"),
       width =16, height = 10, units = "cm", dpi = 1200)
save(time_happy_plot_faceted, file = str_c(main_dir, "/figures/ggplot/s1_s2_time_happy_faceted_ggplot.RData"))
save(time_happy_plot_nofacet, file = str_c(main_dir, "/figures/ggplot/s1_s2_time_happy_nofacet_ggplot.RData"))
save(turtle_data_happy, file = str_c(main_dir, "/figures/ggplot/s1_s2_time_happy_data.RData"))

rm(time_happy_plot_faceted, time_happy_plot_nofacet)
gc()

### plot separating samples with high vs. low happiness ----
#### no inv facets ----
time_happy_plot_split_cols <-
  turtle_data_happy[, .(happy_sample_prop=sum(bi_happy)/.N), 
                    by = .(sample, type, tick, study, identity)][, type_n := .N, 
                                                                     by = .(type, tick, identity)] [, type := paste(type, "\n(", type_n, " samples)", sep = "")
                                                                     ][, type := factor(type, levels = unique(type[order(match(gsub(" \\(.*", "", type), 
                                                                                                                               c("Low satisfaction samples", 
                                                                                                                                 "Moderate satisfaction samples", 
                                                                                                                                 "High satisfaction samples")))]))]|>
  ggplot(aes(y = happy_sample_prop, 
             x= tick,
             color = interaction(identity, study, sep = "\n")))+
  stat_summary(geom = "line", linewidth = m_line, aes(linetype = study), fun = "mean")+
  geom_line(alpha = s_alpha, linewidth = s_line, aes(group = interaction(sample, identity, linetype = study)))+
  facet_wrap(~type)+
  scale_color_manual(values=s2_cols) +
  scale_linetype_manual(values = s2_lines) +
  labs(y = "Satisfied agent proportion", x = "Number of iterations", 
       color = "Political \nidentity by \nstudy",
       title = "S2: Proportion of agents satisfied over time by political identity, invariance condition, \nand level of overall satisfaction",
       caption = "Solid line indicates mean across samples and faint lines indicate sample means.
       \nHigh satisfaction samples have an average satisfaction proportion > 0.5.
       \nModerate satisfaction samples have an average satisfaction proportion betwen 0.5 and 0.25.
       \nLow satisfaction samples have an average satisfaction proportion < 0.25.")+
  ylim(0,1)+
  guides(linetype = "none") +
  x_time +
  base_theme +
  theme(legend.position = "top")

#### full grid ----
time_happy_plot_split_grid <-
  turtle_data_happy[, .(happy_sample_prop=sum(bi_happy)/.N), 
                    by = .(sample, type, tick, study, identity)][, type_n := .N, 
                                                                     by = .(type, tick, identity)] [, type := paste(type, "\n(", type_n, " samples)", sep = "")
                                                                     ][, type := factor(type, levels = unique(type[order(match(gsub(" \\(.*", "", type), 
                                                                                                                               c("Low satisfaction samples", 
                                                                                                                                 "Moderate satisfaction samples", 
                                                                                                                                 "High satisfaction samples")))]))]|>
  ggplot(aes(y = happy_sample_prop, 
             x= tick,
             color = interaction(identity, study, sep = "\n")))+
  stat_summary(geom = "line", linewidth = m_line, aes(linetype = study), fun = "mean")+
  geom_line(alpha = s_alpha, linewidth = s_line, aes(group = interaction(sample, identity, linetype = study)))+
  facet_grid(study~type)+
  scale_color_manual(values=s2_cols) +
  scale_linetype_manual(values = s2_lines) +
  labs(y = "Satisfied agent proportion", x = "Number of iterations", 
       color = "Political \nidentity by \nstudy", fill = "Political \nidentity by \nstudy",
       title = "S2: Proportion of agents satisfied over time by political identity, invariance condition, \nand level of overall satisfaction",
       caption = "Solid line indicates mean across samples and faint lines indicate sample means.
       \nHigh satisfaction samples have an average satisfaction proportion > 0.5.
       \nModerate satisfaction samples have an average satisfaction proportion betwen 0.5 and 0.25.
       \nLow satisfaction samples have an average satisfaction proportion < 0.25.")+
  ylim(0,1)+
  guides(linetype = "none") +
  x_time +
  base_theme +
  theme(legend.position = "top")


ggsave(plot = time_happy_plot_split_grid, filename ="s1_s2_happy_time_split_grid.png", path = str_c(main_dir, "/figures"),
       width =20, height = 15, units = "cm", dpi = 1200)
ggsave(plot = time_happy_plot_split_cols, filename ="s1_s2_happy_time_split_cols.png", path = str_c(main_dir, "/figures"),
       width =20, height = 10, units = "cm", dpi = 1200)
save(time_happy_plot_split_grid, file = str_c(main_dir, "/figures/ggplot/s1_s2_time_happy_split_grid_ggplot.RData"))
save(time_happy_plot_split_cols, file = str_c(main_dir, "/figures/ggplot/s1_s2_time_happy_split_cols_ggplot.RData"))
save

rm(time_happy_plot_split_grid, time_happy_plot_split_cols)
rm(turtle_data_happy)
gc()

## stability across time ----
happy_plot <-  
  turtle_data[, .("happy_prop" = sum(bi_happy)/.N,
                  "iden_inv" = interaction(identity, study, sep = "\n")),
              by = .(who, sample, study, identity)]|>
  ggbetween_cus(x = iden_inv, y = happy_prop,
                 title = "S2: Proportion of satisfied iterations across iterations 
                 \nby political identity and invariance condition",
                 caption = "Individual points indicates agent-level satisfaction proportion.
                 \n n indicates the number of agents in each identity group across samples.
                 \n Diamonds indicate sample means."
  ) +
  geom_point(data = turtle_data[, .("happy_prop" = sum(bi_happy)/.N,
                                    "iden_inv" = interaction(identity, study, sep = "\n")),
                                by = .(study, sample, identity)], size = s_m_p_size, alpha = s_m_p_alpha, shape = s_m_p_shape,
             aes(colour = iden_inv), position = position_nudge(x = s_m_p_nudge)
  )+
  ylim(0,1)+
  scale_fill_manual(values=s2_cols) +
  scale_color_manual(values=s2_cols) +
  labs(x = "Political identity", y ="Satisfied agent proportion")+
  base_theme +
  theme(legend.position = "none")

ggsave(filename ="s1_s2_happy_agg.png", path = str_c(main_dir, "/figures"),
       width =16, height = 10, units = "cm", dpi = 1200)
save(happy_plot, file = str_c(main_dir, "/figures/ggplot/s1_s2_happy_ggplot.RData"))
