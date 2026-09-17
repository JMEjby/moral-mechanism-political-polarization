script <- "moral_signals"
source("support-functions/local_full_setup.R") # switch to remote_setup.R for remote runs

## initialise data ----
turtle_data <- turtle_cluster_data[tick <= 201 & tick > 0,][, 
                                                            "study":= fifelse(sample <= 150, 
                                                                                  "Study 1", 
                                                                                  "Study 2")]

turtle_data$identity <- factor(turtle_data$identity,
                               labels = c("Conservative", "Liberal"))
rm(turtle_cluster_data); gc()

# === Study 1 ====

## ---- initialise ----
moral_signals_s1 <- turtle_data[sample <= 150]

## ---- Moral signals over time ----
moral_signals_s1 <- melt(moral_signals_s1,
    measure.vars = c("past_choices_1", 
                     "past_choices_2",
                     "past_choices_3",
                     "past_choices_4",
                     "past_choices_5"),
    variable.name = "signal_time",
    value.name = "signal_mf")  [,.("Care" = sum(signal_mf == 1)/5,
                                   "Fairness" = sum(signal_mf == 2)/5,
                                   "Authority" = sum(signal_mf == 3)/5,
                                   "Ingroup loyalty" = sum(signal_mf == 4)/5,
                                   "Purity" = sum(signal_mf == 5)/5,
                                   "Disengage" = sum(signal_mf == "d")/5),
                                by = .(sample, tick, who, identity)]

moral_signals_s1 <- melt(moral_signals_s1,
                         measure.vars = c("Care", 
                                          "Fairness",
                                          "Authority",
                                          "Ingroup loyalty",
                                          "Purity",
                                          "Disengage"),
                         variable.name = "mf",
                         value.name = "prop_signal")

moral_signals_s1 <- moral_signals_s1[, c("Mean", "SE") := .(mean(prop_signal),
                                                            sd(prop_signal)/sqrt(length(unique(who)))),
                                     by = .(identity, 
                                            tick, 
                                            mf
                                     )] [, .("sample_mean" = mean(prop_signal)),
                                         by = .(mf, 
                                                identity, 
                                                sample, 
                                                tick, 
                                                SE, 
                                                Mean
                                         )][, "SE_sample" :=
                                              sd(sample_mean)/sqrt(length(unique(sample))),
                                            by = .(identity, 
                                                   mf, 
                                                   tick)] 

### All signals including disengagement ----
time_signal_id_plot_s1 <- 
  ggplot(data = moral_signals_s1, aes(x=tick, y= Mean))+
  geom_ribbon(alpha = rib_alpha, aes(ymin = Mean-SE_sample, ymax = Mean+SE_sample, fill = identity)) +
  geom_line(alpha = s_alpha, linewidth = s_line, aes(y = sample_mean, colour = identity, group = interaction(sample, identity, mf)))+
  geom_line(aes(colour = identity), linewidth = m_line)+
  scale_fill_manual(values=c("#AF2820", "#345DA9"))+  
  scale_color_manual(values=c("#AF2820", "#345DA9"))+  
  labs(y = "Proportion of signals", x= "Number of iterations", 
       color = "Political identity", fill = "Political identity",
       title = "S1: Proportion of moral signals over time by political identity",
       caption = "Solid lines indicates identity-level mean proportion across samples.
       \nFaint lines indicates identity-level sample mean proportions.
       \nRibbons indicate standard error between mean sample proportions.")+
  ylim(0,1)+
  x_time +
  facet_wrap(~mf, ncol = 3) +
  base_theme +
  theme(legend.position = "top")

  
ggsave(plot = time_signal_id_plot_s1, filename ="s1_moral_signals_time.png", path = str_c(main_dir, "/figures"), 
       width =16, height = 10, units = "cm", dpi = 1200)
save(time_signal_id_plot_s1, file = str_c(main_dir, "/figures/ggplot/s1_time_signal_id_ggplot.RData"))
save(moral_signals_s1, file = str_c(main_dir, "/figures/ggplot/s1_time_signal_id_data.RData"))

rm(time_signal_id_plot_s1)
gc()

### Moral foundation signals only (no disengagement) ----
moral_signals_mf_s1 <- moral_signals_s1[mf != "Disengage"]
time_mf_signal_id_plot_s1 <- 
  ggplot(data = moral_signals_mf_s1, aes(x=tick, y= Mean))+
  geom_ribbon(alpha = rib_alpha, aes(ymin = Mean-SE_sample, ymax = Mean+SE_sample, fill = identity)) +
  geom_line(alpha = s_alpha, linewidth = s_line, aes(y = sample_mean, colour = identity, group = interaction(sample, identity, mf)))+
  geom_line(aes(colour = identity), linewidth = m_line)+
  scale_fill_manual(values=c("#AF2820", "#345DA9"))+  
  scale_color_manual(values=c("#AF2820", "#345DA9"))+  
  labs(y = "Proportion of signals", x= "Number of iterations", 
       color = "Political identity", fill = "Political identity",
       title = "S1: Proportion of moral foundation signals over time by political identity",
       caption = "Solid lines indicates identity-level mean proportion across samples.
       \nFaint lines indicates identity-level sample mean proportions.
       \nRibbons indicate standard error between mean sample proportions.")+
  ylim(0,1)+
  x_time +
  facet_wrap(~mf) +
  base_theme +
  theme(legend.position = "top")


ggsave(plot = time_mf_signal_id_plot_s1, filename ="s1_moral_mf_signals_time.png", path = str_c(main_dir, "/figures"), 
       width =16, height = 10, units = "cm", dpi = 1200)
save(time_mf_signal_id_plot_s1, file = str_c(main_dir, "/figures/ggplot/s1_time_mf_signal_id_ggplot.RData"))

rm(time_mf_signal_id_plot_s1)
rm(moral_signals_mf_s1)
rm(moral_signals_s1)
gc()

# === Study 2 ====

## ---- Moral signals over time ----
moral_signals_s1_s2 <- melt(turtle_data, 
                         measure.vars = c("past_choices_1", 
                                          "past_choices_2",
                                          "past_choices_3",
                                          "past_choices_4",
                                          "past_choices_5"),
                         variable.name = "signal_time",
                         value.name = "signal_mf")  [,.("Care" = sum(signal_mf == 1)/5,
                                                        "Fairness" = sum(signal_mf == 2)/5,
                                                        "Authority" = sum(signal_mf == 3)/5,
                                                        "Ingroup loyalty" = sum(signal_mf == 4)/5,
                                                        "Purity" = sum(signal_mf == 5)/5,
                                                        "Disengage" = sum(signal_mf == "d")/5),
                                                     by = .(sample, tick, who, identity, study)]

moral_signals_s1_s2 <- melt(moral_signals_s1_s2,
                         measure.vars = c("Care", 
                                          "Fairness",
                                          "Authority",
                                          "Ingroup loyalty",
                                          "Purity",
                                          "Disengage"),
                         variable.name = "mf",
                         value.name = "prop_signal")

moral_signals_s1_s2 <- moral_signals_s1_s2[, c("Mean", "SE") := .(mean(prop_signal),
                                                                  sd(prop_signal)/sqrt(length(unique(who)))),
                                           by = .(identity, 
                                                  tick, 
                                                  mf,
                                                  study
                                           )] [, .("sample_mean" = mean(prop_signal)),
                                               by = .(mf, 
                                                      identity, 
                                                      sample, 
                                                      tick, 
                                                      SE, 
                                                      Mean,
                                                      study
                                               )][, "SE_sample" :=
                                                    sd(sample_mean)/sqrt(length(unique(sample))),
                                                  by = .(identity, 
                                                         mf, 
                                                         tick,
                                                         study)] 

### All signals including disengagement ----
time_signal_id_plot_s2_faceted <- 
  ggplot(data = moral_signals_s1_s2, aes(x=tick, y= Mean))+
  geom_ribbon(alpha = rib_alpha, aes(ymin = Mean-SE_sample, ymax = Mean+SE_sample, fill = interaction(identity, study, sep = "\n"))) +
  geom_line(alpha = s_alpha, linewidth = s_line, aes(y = sample_mean, colour = interaction(identity, study, sep = "\n"), group = interaction(sample, identity, mf), linetype = study))+
  geom_line(aes(colour = interaction(identity, study, sep = "\n"), linetype = study), linewidth = m_line)+
  scale_fill_manual(values= s2_cols)+  
  scale_color_manual(values= s2_cols)+  
  labs(y = "Proportion of signals", x= "Number of iterations", 
       color = "Political identity by \ninvariance", fill = "Political identity by \ninvariance",
       title = "S2: Proportion of moral signals over time by political identity \nand invariance condition",
       caption = "Solid lines indicates identity-level mean proportion across samples.
       \nFaint lines indicates identity-level sample mean proportions.
       \nRibbons indicate standard error between mean sample proportions.")+
  ylim(0,1)+
  guides(linetype = "none") +
  x_time +
  facet_wrap(~mf) +
  base_theme +
  theme(legend.position = "top")

#### Full grid (signal type x invariance) ----
time_signal_id_plot_s2_grid <- 
  ggplot(data = moral_signals_s1_s2, aes(x=tick, y= Mean))+
  geom_ribbon(alpha = rib_alpha, aes(ymin = Mean-SE_sample, ymax = Mean+SE_sample, fill = interaction(identity, study, sep = "\n"))) +
  geom_line(alpha = s_alpha, linewidth = s_line, aes(y = sample_mean, colour = interaction(identity, study, sep = "\n"), group = interaction(sample, identity, mf), linetype = study))+
  geom_line(aes(colour = interaction(identity, study, sep = "\n"), linetype = study), linewidth = m_line)+
  facet_grid(study~mf)+
  scale_fill_manual(values = s2_cols)+  
  scale_color_manual(values = s2_cols)+  
  labs(y = "Proportion of signals", x= "Number of iterations", 
       color = "Political identity by \ninvariance", fill = "Political identity by \ninvariance",
       title = "S2: Proportion of moral signals over time by political identity \nand invariance condition",
       caption = "Solid lines indicates identity-level mean proportion across samples.
       \nFaint lines indicates identity-level sample mean proportions.
       \nRibbons indicate standard error between mean sample proportions.")+
  ylim(0,1)+
  guides(linetype = "none") +
  x_time +
  base_theme +
  theme(legend.position = "top")

ggsave(plot = time_signal_id_plot_s2_grid, filename ="s1_s2_moral_signals_time_grid.png", path = str_c(main_dir, "/figures"),
       width =20, height = 15, units = "cm", dpi = 1200)
ggsave(plot = time_signal_id_plot_s2_faceted, filename ="s1_s2_moral_signals_time_faceted.png", path = str_c(main_dir, "/figures"),
       width =20, height = 12.5, units = "cm", dpi = 1200)
save(time_signal_id_plot_s2_grid, file = str_c(main_dir, "/figures/ggplot/s1_s2_time_signal_id_grid_ggplot.RData"))
save(time_signal_id_plot_s2_faceted, file = str_c(main_dir, "/figures/ggplot/s1_s2_time_signal_id_faceted_ggplot.RData"))
save(moral_signals_s1_s2, file = str_c(main_dir, "/figures/ggplot/s1_s2_time_signal_id_data.RData"))

rm(time_signal_id_plot_s2_grid, time_signal_id_plot_s2_faceted)
gc()

### Moral foundation signals only (no disengagement) ----
moral_signals_time_mf_s2 <- moral_signals_s1_s2[mf != "Disengage"]

#### No facets ----
time_mf_signal_id_plot_s2_faceted <- 
  ggplot(data = moral_signals_time_mf_s2, aes(x=tick, y= Mean))+
  geom_ribbon(alpha = rib_alpha, aes(ymin = Mean-SE_sample, ymax = Mean+SE_sample, fill = interaction(identity, study, sep = "\n"))) +
  geom_line(alpha = s_alpha, linewidth = s_line, aes(y = sample_mean, colour = interaction(identity, study, sep = "\n"), group = interaction(sample, identity, mf), linetype = study))+
  geom_line(aes(colour = interaction(identity, study, sep = "\n"), linetype = study), linewidth = m_line)+
  scale_fill_manual(values=s2_cols)+  
  scale_color_manual(values=s2_cols)+  
  labs(y = "Proportion of signals", x= "Number of iterations", 
       color = "Political identity by \ninvariance", fill = "Political identity by \ninvariance",
       title = "S2: Proportion of moral foundation signals over time by political identity \nand invariance condition",
       caption = "Solid lines indicates identity-level mean proportion across samples.
       \nFaint lines indicates identity-level sample mean proportions.
       \nRibbons indicate standard error between mean sample proportions.")+
  ylim(0,1)+
  guides(linetype = "none") +
  x_time +
  facet_wrap(~mf) +
  base_theme+
  theme(legend.position = "top")

#### Full grid (signal type x invariance) ----
time_mf_signal_id_plot_s2_grid <- 
  ggplot(data = moral_signals_time_mf_s2, aes(x=tick, y= Mean))+
  geom_ribbon(alpha = rib_alpha, aes(ymin = Mean-SE_sample, ymax = Mean+SE_sample, fill = interaction(identity, study, sep = "\n"))) +
  geom_line(alpha = s_alpha, aes(y = sample_mean, colour = interaction(identity, study, sep = "\n"), group = interaction(sample, identity, mf), linetype = study))+
  geom_line(aes(colour = interaction(identity, study, sep = "\n"), linetype = study), linewidth = m_line)+
  facet_grid(study~mf)+
  scale_fill_manual(values=s2_cols)+  
  scale_color_manual(values=s2_cols)+  
  labs(y = "Proportion of signals", x= "Number of iterations", 
       color = "Political identity by \ninvariance", fill = "Political identity by \ninvariance",
       title = "S2: Proportion of moral foundation signals over time by political identity \nand invariance condition",
       caption = "Solid lines indicates identity-level mean proportion across samples.
       \nFaint lines indicates identity-level sample mean proportions.
       \nRibbons indicate standard error between mean sample proportions.")+
  ylim(0,1)+
  guides(linetype = "none") +
    x_time +
    base_theme +
    theme(legend.position = "top")

ggsave(plot = time_mf_signal_id_plot_s2_grid, filename ="s1_s2_moral_mf_signals_time_grid.png", path = str_c(main_dir, "/figures"),
       width =16, height = 12.5, units = "cm", dpi = 1200)
ggsave(plot = time_mf_signal_id_plot_s2_faceted, filename ="s1_s2_moral_mf_signals_time_faceted.png", path = str_c(main_dir, "/figures"),
       width =16, height = 10, units = "cm", dpi = 1200)
save(time_mf_signal_id_plot_s2_grid, file = str_c(main_dir, "/figures/ggplot/s1_s2_time_mf_signal_id_grid_ggplot.RData"))
save(time_mf_signal_id_plot_s2_faceted, file = str_c(main_dir, "/figures/ggplot/s1_s2_time_mf_signal_id_faceted_ggplot.RData"))
save(moral_signals_time_mf_s2, file = str_c(main_dir, "/figures/ggplot/s1_s2_time_mf_signal_id_data.RData"))

rm(time_mf_signal_id_plot_s2_grid, time_mf_signal_id_plot_s2_faceted)
rm(moral_signals_time_mf_s2)
rm(moral_signals_s1_s2)
gc()

# === Study 2 only ====

## ---- initialise ----
moral_signals_s2 <- turtle_data[sample > 150]
## ---- Moral signals over time ----
moral_signals_s2 <- melt(moral_signals_s2,
                         measure.vars = c("past_choices_1", 
                                          "past_choices_2",
                                          "past_choices_3",
                                          "past_choices_4",
                                          "past_choices_5"),
                         variable.name = "signal_time",
                         value.name = "signal_mf")  [,.("Care" = sum(signal_mf == 1)/5,
                                                        "Fairness" = sum(signal_mf == 2)/5,
                                                        "Authority" = sum(signal_mf == 3)/5,
                                                        "Ingroup loyalty" = sum(signal_mf == 4)/5,
                                                        "Purity" = sum(signal_mf == 5)/5,
                                                        "Disengage" = sum(signal_mf == "d")/5),
                                                     by = .(sample, tick, who, identity)]

moral_signals_s2 <- melt(moral_signals_s2,
                         measure.vars = c("Care", 
                                          "Fairness",
                                          "Authority",
                                          "Ingroup loyalty",
                                          "Purity",
                                          "Disengage"),
                         variable.name = "mf",
                         value.name = "prop_signal")

moral_signals_s2 <- moral_signals_s2[, c("Mean", "SE") := .(mean(prop_signal),
                                                            sd(prop_signal)/sqrt(length(unique(who)))),
                                     by = .(identity, 
                                            tick, 
                                            mf
                                     )] [, .("sample_mean" = mean(prop_signal)),
                                         by = .(mf, 
                                                identity, 
                                                sample, 
                                                tick, 
                                                SE, 
                                                Mean
                                         )][, "SE_sample" :=
                                              sd(sample_mean)/sqrt(length(unique(sample))),
                                            by = .(identity, 
                                                   mf, 
                                                   tick)] 


### All signals including disengagement ----
time_signal_id_plot_s2 <-
  ggplot(data = moral_signals_s2, aes(x=tick, y= Mean))+
  geom_ribbon(alpha = rib_alpha, aes(ymin = Mean-SE_sample, ymax = Mean+SE_sample, fill = identity)) +
  geom_line(alpha = s_alpha, linewidth = s_line, aes(y = sample_mean, colour = identity, group = interaction(sample, identity, mf)))+
  geom_line(aes(colour = identity), linewidth = m_line)+
  scale_fill_manual(values = s2_cols[3:4])+
  scale_color_manual(values = s2_cols[3:4])+
  labs(y = "Proportion of signals", x= "Number of iterations",
       color = "Political identity", fill = "Political identity",
       title = "S2: Proportion of moral signals over time by political identity",
       caption = "Solid lines indicates identity-level mean proportion across samples.
       \nFaint lines indicates identity-level sample mean proportions.
       \nRibbons indicate standard error between mean sample proportions.")+
  ylim(0,1)+
  x_time +
  facet_wrap(~mf) +
  base_theme +
  theme(legend.position = "top")

ggsave(plot = time_signal_id_plot_s2, filename ="s2_moral_signals_time.png", path = str_c(main_dir, "/figures"),
       width =16, height = 10, units = "cm", dpi = 1200)
save(time_signal_id_plot_s2, file = str_c(main_dir, "/figures/ggplot/s2_time_signal_id_ggplot.RData"))
save(moral_signals_s2, file = str_c(main_dir, "/figures/ggplot/s2_time_signal_id_data.RData"))

rm(time_signal_id_plot_s2)
gc()

### Moral foundation signals only (no disengagement) ----
moral_signals_mf_s2 <- moral_signals_s2[mf != "Disengage"]

time_mf_signal_id_plot_s2 <-
  ggplot(data = moral_signals_mf_s2, aes(x=tick, y= Mean))+
  geom_ribbon(alpha = rib_alpha, aes(ymin = Mean-SE_sample, ymax = Mean+SE_sample, fill = identity)) +
  geom_line(alpha = s_alpha, linewidth = s_line, aes(y = sample_mean, colour = identity, group = interaction(sample, identity, mf)))+
  geom_line(aes(colour = identity), linewidth = m_line)+
  scale_fill_manual(values = s2_cols[3:4])+
  scale_color_manual(values = s2_cols[3:4])+
  labs(y = "Proportion of signals", x= "Number of iterations",
       color = "Political identity", fill = "Political identity",
       title = "S2: Proportion of moral foundation signals over time by political identity",
       caption = "Solid lines indicates identity-level mean proportion across samples.
       \nFaint lines indicates identity-level sample mean proportions.
       \nRibbons indicate standard error between mean sample proportions.")+
  ylim(0,1)+
  x_time +
  facet_wrap(~mf) +
  base_theme +
  theme(legend.position = "top")

ggsave(plot = time_mf_signal_id_plot_s2, filename ="s2_moral_mf_signals_time.png", path = str_c(main_dir, "/figures"),
       width =16, height = 10, units = "cm", dpi = 1200)
save(time_mf_signal_id_plot_s2, file = str_c(main_dir, "/figures/ggplot/s2_time_mf_signal_id_ggplot.RData"))
save(moral_signals_mf_s2, file = str_c(main_dir, "/figures/ggplot/s2_time_mf_signal_id_data.RData"))

rm(time_mf_signal_id_plot_s2)
rm(moral_signals_mf_s2)
rm(moral_signals_s2)
gc()
