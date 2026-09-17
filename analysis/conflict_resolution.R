script <- "conflict_resolution"
source("support-functions/local_full_setup.R") # switch to remote_setup.R for remote runs

# settings ----
make_conf_plot <- function(..., strategy, agent_df, sample_df, iv) {
  ggbetween_homog_cus(
    data    = agent_df[res == strategy],
    y       = conflict_prop,
    caption = conf_caption,
    ggtheme = base_theme + theme(legend.position = "none"),
    ylab    = "Conflict proportion",
    xlab    = "Political identity",
    ...
  ) +
    geom_point(data = sample_df[res == strategy],
               size = s_m_p_size, alpha = s_m_p_alpha, shape = s_m_p_shape,
               aes(colour = .data[[iv]]),
               position = position_nudge(x = s_m_p_nudge))
}

conf_caption <- "Individual points indicate agent-level proportions across iterations.
\nn indicates the number of agents across samples.
\nDiamonds indicate sample means."

## initialise data ----
turtle_data <- turtle_cluster_data[tick > 0 & tick <= 201,][,
                                                            "study" := fifelse(sample <= 150,
                                                                               "Study 1",
                                                                               "Study 2")
][, action_taken := factor(action_taken,
                           levels = c("co", "dis", "ran", "cho", "NC"))]

turtle_data$identity <- factor(turtle_data$identity, labels = c("Conservative", "Liberal"))

conf_data <- turtle_data[, c("choice_exposure", "conflicted", "bi_conflict") := .(
  fifelse(action_taken == "No choice", "No choice", "Choice"),
  fifelse(conflicted == "true", "Conflicted", "Not conflicted"),
  fifelse(conflicted == "true", 1, 0)
)]
rm(turtle_cluster_data); gc()

# === Study 1 ====

conf_data_s1 <- conf_data[sample <= 150]

## conflict iterations aggregated ----
conflict_plot_s1 <- 
  conf_data_s1[choice_exposure == "Choice",] [,.(prop_conf = mean(bi_conflict)), by = .(sample, who, identity)] |>
ggbetween_homog_cus(x = identity,
                    y = prop_conf,
                    digits = 4,
                      title   = "S1: Proportion of conflicts across iterations with choices.",
                      caption = "Points indicate agent-level proportions across iterations
      \nn indicates the number of agents across samples.",
                    ggtheme               = base_theme + theme(legend.position = "top"),
                    ylab                  = "Choice proportion",
                    xlab                  = "Political identity",
                    ggplot.component      = list(scale_color_manual(values = s1_cols), ylim(0,1)))

ggsave(plot = conflict_plot_s1,
       filename = "s1_conflicts.png",
       path = str_c(main_dir, "/figures"), width = 16, height = 10, units = "cm", dpi = 1200)
save(conflict_plot_s1,
     file = str_c(main_dir, "/figures/ggplot/s1_conflict_ggplot.RData"))

## conflict resolution aggregated ----
conf_agg_s1 <-
  conf_data_s1[choice_exposure == "Choice" &
                 conflicted == "Conflicted",
  ][, .("Random choice"       = mean(action_taken == "ran"),
        "Copy ingroup"         = mean(action_taken == "co"),
        "Moral disengagement"  = mean(action_taken == "dis")),
    by = .(sample, who, identity)] |>
  melt(measure.vars  = c("Random choice", "Copy ingroup", "Moral disengagement"),
       variable.name = "res",
       value.name    = "conflict_prop") 

## Sample means for diamonds ----
conf_sample_s1 <- conf_agg_s1[, .(conflict_prop = mean(conflict_prop)),
                              by = .(sample, identity, res)]

## Individual plots ----
conf_random_s1 <- make_conf_plot(
  x = identity, title = "S1: Random choice",
  ggplot.component = list(scale_color_manual(values = s1_cols),
                          scale_fill_manual(values = s1_cols), ylim(0, 1)),
  strategy   = "Random choice",
  agent_df   = conf_agg_s1,
  sample_df  = conf_sample_s1,
  iv = "identity"
)
conf_copy_s1   <- make_conf_plot(
  x = identity, title = "S1: Copy ingroup",
  ggplot.component = list(scale_color_manual(values = s1_cols),
                          scale_fill_manual(values = s1_cols), ylim(0, 1)),
  strategy = "Copy ingroup", 
  agent_df = conf_agg_s1,
  sample_df  = conf_sample_s1,
  iv = "identity")

conf_disengage_s1 <- make_conf_plot(x = identity, title = "S1: Moral disengagement",
                                    ggplot.component = list(scale_color_manual(values = s1_cols),
                                                            scale_fill_manual(values = s1_cols), ylim(0, 1)),
                                    strategy = "Moral disengagement", 
                                    agent_df = conf_agg_s1, 
                                    sample_df  = conf_sample_s1,
                                    iv = "identity")

ggsave(plot = conf_random_s1,    filename = "s1_conflict_random.png",
       path = str_c(main_dir, "/figures"), width = 10, height = 10, units = "cm", dpi = 1200)
ggsave(plot = conf_copy_s1,      filename = "s1_conflict_copy.png",
       path = str_c(main_dir, "/figures"), width = 10, height = 10, units = "cm", dpi = 1200)
ggsave(plot = conf_disengage_s1, filename = "s1_conflict_disengage.png",
       path = str_c(main_dir, "/figures"), width = 10, height = 10, units = "cm", dpi = 1200)

save(conf_random_s1,    file = str_c(main_dir, "/figures/ggplot/s1_conflict_random_ggplot.RData"))
save(conf_copy_s1,      file = str_c(main_dir, "/figures/ggplot/s1_conflict_copy_ggplot.RData"))
save(conf_disengage_s1, file = str_c(main_dir, "/figures/ggplot/s1_conflict_disengage_ggplot.RData"))

## Patchwork ----
conflict_plot_s1 <- conflict_plot_s1 +
  labs(title = "Moral conflict") 

conf_random_s1 <- conf_random_s1 +
  labs(title = "Random choice")
conf_copy_s1 <- conf_copy_s1 +
  labs(title = "Copy ingroup")
conf_disengage_s1 <- conf_disengage_s1 +
  labs(title = "Disengagement")

conflict_resolution_plot_s1 <- (conflict_plot_s1 | conf_random_s1 | conf_copy_s1 | conf_disengage_s1) +
  plot_annotation(
    tag_levels = "A"
  ) & 
  coord_flip() &
  labs(caption = NULL, x = NULL) &
  theme(plot.margin = unit(c(0,0.15,0,0), "cm"),
        plot.tag = element_text(size = 7),
        axis.text.y = element_text(hjust = 0.5, vjust = 0.5, angle = 90)) & x_id

ggsave(plot = conflict_resolution_plot_s1,
       filename = "s1_conflict_all.png",
       path = str_c(main_dir, "/figures"), width = 16, height =4.5, units = "cm", dpi = 1200)
save(conflict_resolution_plot_s1,
     file = str_c(main_dir, "/figures/ggplot/s1_conflict_all_ggplot.RData"))

## Patchwork ----
conflict_resolution_plot_s1 <- (conf_random_s1 | conf_copy_s1 | conf_disengage_s1) +
  plot_annotation(
    tag_levels = "A"
  ) & 
  coord_flip() &
  labs(caption = NULL, x = NULL) &
  theme(plot.margin = unit(c(0,0.15,0,0), "cm"),
        plot.tag = element_text(size = 7),
        axis.text.y = element_text(hjust = 0.5, vjust = 0.5, angle = 90)) & x_id

ggsave(plot = conflict_resolution_plot_s1,
       filename = "s1_conflict_res_all.png",
       path = str_c(main_dir, "/figures"), width = 16, height =4.5, units = "cm", dpi = 1200)
save(conflict_resolution_plot_s1,
     file = str_c(main_dir, "/figures/ggplot/s1_conflict_res_ggplot.RData"))

rm(conf_random_s1, conf_copy_s1, conf_disengage_s1, conflict_plot_s1,
   conflict_resolution_plot_s1, conf_agg_s1, conf_sample_s1, conf_data_s1)
gc()

# === Study 2 ====

# Add interaction variable for identity x study
conf_data[, iden_study := interaction(identity, study, sep = "\n")]

## conflict iterations aggregated ----
conflict_plot_s2 <- 
  conf_data[choice_exposure == "Choice",] [,.(prop_conf = mean(bi_conflict)), by = .(sample, who, iden_study)] |>
  ggbetween_homog_cus(x = iden_study,
                      y = prop_conf,
                      digits = 4,
                      annotation.args       = list(
                        title   = "S2: Proportion of conflicts across iterations with choices.",
                        caption = "Points indicate agent-level proportions across iterations.
      \nn indicates the number of agents across samples."),
                      ggtheme               = base_theme + theme(legend.position = "top"),
                      ylab                  = "Choice proportion",
                      xlab                  = "Political identity",
                      ggplot.component      = list(scale_color_manual(values = s2_cols), ylim(0,1)))

ggsave(plot = conflict_plot_s2,
       filename = "s1_s2_conflicts.png",
       path = str_c(main_dir, "/figures"), width = 16, height = 10, units = "cm", dpi = 1200)
save(conflict_plot_s2,
     file = str_c(main_dir, "/figures/ggplot/s1_s2_conflict_ggplot.RData"))

## conflict resolution aggregated ----
conf_agg_s2 <- conf_data[choice_exposure == "Choice" &
                             conflicted == "Conflicted",
][, .("Random choice"       = mean(action_taken == "ran"),
      "Copy ingroup"         = mean(action_taken == "co"),
      "Moral disengagement"  = mean(action_taken == "dis")),
  by = .(sample, who, identity, study, iden_study)] |>
  melt(measure.vars  = c("Random choice", "Copy ingroup", "Moral disengagement"),
       variable.name = "res",
       value.name    = "conflict_prop")

## Sample means for diamonds ----
conf_sample_s2 <- conf_agg_s2[, .(conflict_prop = mean(conflict_prop)),
                              by = .(sample, iden_study, res)]

## Individual plots ----
conf_random_s2 <- make_conf_plot(
  x = iden_study, title = "s2: Random choice",
  ggplot.component = list(scale_color_manual(values = s2_cols),
                          scale_fill_manual(values = s2_cols), ylim(0, 1)),
  strategy   = "Random choice",
  agent_df   = conf_agg_s2,
  sample_df  = conf_sample_s2,
  iv = "iden_study"
)
conf_copy_s2   <- make_conf_plot(
  x = iden_study, title = "s2: Copy ingroup",
  ggplot.component = list(scale_color_manual(values = s2_cols),
                          scale_fill_manual(values = s2_cols), ylim(0, 1)),
  strategy = "Copy ingroup", 
  agent_df = conf_agg_s2,
  sample_df  = conf_sample_s2,
  iv = "iden_study")

conf_disengage_s2 <- make_conf_plot(x = iden_study, title = "s2: Moral disengagement",
                                    ggplot.component = list(scale_color_manual(values = s2_cols),
                                                            scale_fill_manual(values = s2_cols), ylim(0, 1)),
                                    strategy = "Moral disengagement", 
                                    agent_df = conf_agg_s2, 
                                    sample_df  = conf_sample_s2,
                                    iv = "iden_study")

ggsave(plot = conf_random_s2,    filename = "s1_s2_conflict_random.png",
       path = str_c(main_dir, "/figures"), width = 10, height = 10, units = "cm", dpi = 1200)
ggsave(plot = conf_copy_s2,      filename = "s1_s2_conflict_copy.png",
       path = str_c(main_dir, "/figures"), width = 10, height = 10, units = "cm", dpi = 1200)
ggsave(plot = conf_disengage_s2, filename = "s1_s2_conflict_disengage.png",
       path = str_c(main_dir, "/figures"), width = 10, height = 10, units = "cm", dpi = 1200)

save(conf_random_s2,    file = str_c(main_dir, "/figures/ggplot/s1_s2_conflict_random_ggplot.RData"))
save(conf_copy_s2,      file = str_c(main_dir, "/figures/ggplot/s1_s2_conflict_copy_ggplot.RData"))
save(conf_disengage_s2, file = str_c(main_dir, "/figures/ggplot/s1_s2_conflict_disengage_ggplot.RData"))

## Patchwork ----
conflict_plot_s2 <- conflict_plot_s2 +
  labs(title = "Moral conflict") 

conf_random_s2 <- conf_random_s2 +
  labs(title = "Random choice")
conf_copy_s2 <- conf_copy_s2 +
  labs(title = "Copy ingroup")
conf_disengage_s2 <- conf_disengage_s2 +
  labs(title = "Disengagement")

conflict_resolution_plot_s2 <- (conflict_plot_s2 | conf_random_s2 | conf_copy_s2 | conf_disengage_s2) +
  plot_annotation(
    tag_levels = "A"
  ) &
  coord_flip() &
  labs(caption = NULL, x = NULL) &
  theme(plot.margin = unit(c(0,0.33,0,0), "cm"),
        plot.tag = element_text(size = 7),
        axis.text.y = element_text(hjust = 0.5, vjust = 0.5, angle = 90)) & x_id

ggsave(plot = conflict_resolution_plot_s2,
       filename = "s1_s2_conflict_all.png",
       path = str_c(main_dir, "/figures"), width = 16, height =6.5, units = "cm", dpi = 1200)
save(conflict_resolution_plot_s2,
     file = str_c(main_dir, "/figures/ggplot/s1_s2_conflict_all_ggplot.RData"))

## Patchwork ----
conflict_resolution_plot_s2 <- (conf_random_s2 | conf_copy_s2 | conf_disengage_s2) +
  plot_annotation(
    tag_levels = "A"
  ) &
  coord_flip() &
  labs(caption = NULL, x = NULL) &
  theme(plot.margin = unit(c(0,0.33,0,0), "cm"),
        plot.tag = element_text(size = 7),
        axis.text.y = element_text(hjust = 0.5, vjust = 0.5, angle = 90)) & x_id

ggsave(plot = conflict_resolution_plot_s2,
       filename = "s1_s2_conflict_res_all.png",
       path = str_c(main_dir, "/figures"), width = 16, height =6.5, units = "cm", dpi = 1200)
save(conflict_resolution_plot_s2,
     file = str_c(main_dir, "/figures/ggplot/s1_s2_conflict_res_ggplot.RData"))

rm(conf_random_s2, conf_copy_s2, conf_disengage_s2, conflict_plot_s2,
   conflict_resolution_plot_s2, conf_agg_s2, conf_sample_s2, conf_data_s2)
gc()

# === Study 2 only ====

conf_data_s2_only <- conf_data[sample > 150]

## conflict iterations aggregated ----
conflict_plot_s2 <-
  conf_data_s2_only[choice_exposure == "Choice",] [,.(prop_conf = mean(bi_conflict)), by = .(sample, who, identity)] |>
  ggbetween_homog_cus(x = identity,
                      y = prop_conf,
                      digits = 4,
                        title   = "S2: Proportion of conflicts across iterations.",
                        caption = "Points indicate agent-level proportions across iterations
      \nn indicates the number of agents across samples.",
                      ggtheme               = base_theme + theme(legend.position = "top"),
                      ylab                  = "Choice proportion",
                      xlab                  = "Political identity",
                      ggplot.component      = list(scale_color_manual(values = s2_cols[3:4]), ylim(0,1)))

ggsave(plot = conflict_plot_s2,
       filename = "s2_conflicts.png",
       path = str_c(main_dir, "/figures"), width = 16, height = 10, units = "cm", dpi = 1200)
save(conflict_plot_s2,
     file = str_c(main_dir, "/figures/ggplot/s2_conflict_ggplot.RData"))

## conflict resolution aggregated ----
conf_agg_s2_only <-
  conf_data_s2_only[choice_exposure == "Choice" &
                 conflicted == "Conflicted",
  ][, .("Random choice"       = mean(action_taken == "ran"),
        "Copy ingroup"         = mean(action_taken == "co"),
        "Moral disengagement"  = mean(action_taken == "dis")),
    by = .(sample, who, identity)] |>
  melt(measure.vars  = c("Random choice", "Copy ingroup", "Moral disengagement"),
       variable.name = "res",
       value.name    = "conflict_prop")

## Sample means for diamonds ----
conf_sample_s2_only <- conf_agg_s2_only[, .(conflict_prop = mean(conflict_prop)),
                              by = .(sample, identity, res)]

## Individual plots ----
conf_random_s2 <- make_conf_plot(
  x = identity, title = "S2: Random choice",
  ggplot.component = list(scale_color_manual(values = s2_cols[3:4]),
                          scale_fill_manual(values = s2_cols[3:4]), ylim(0, 1)),
  strategy   = "Random choice",
  agent_df   = conf_agg_s2_only,
  sample_df  = conf_sample_s2_only,
  iv = "identity"
)
conf_copy_s2   <- make_conf_plot(
  x = identity, title = "S2: Copy ingroup",
  ggplot.component = list(scale_color_manual(values = s2_cols[3:4]),
                          scale_fill_manual(values = s2_cols[3:4]), ylim(0, 1)),
  strategy = "Copy ingroup",
  agent_df = conf_agg_s2_only,
  sample_df  = conf_sample_s2_only,
  iv = "identity")

conf_disengage_s2 <- make_conf_plot(x = identity, title = "S2: Moral disengagement",
                                    ggplot.component = list(scale_color_manual(values = s2_cols[3:4]),
                                                            scale_fill_manual(values = s2_cols[3:4]), ylim(0, 1)),
                                    strategy = "Moral disengagement",
                                    agent_df = conf_agg_s2_only,
                                    sample_df  = conf_sample_s2_only,
                                    iv = "identity")

ggsave(plot = conf_random_s2,    filename = "s2_conflict_random.png",
       path = str_c(main_dir, "/figures"), width = 10, height = 10, units = "cm", dpi = 1200)
ggsave(plot = conf_copy_s2,      filename = "s2_conflict_copy.png",
       path = str_c(main_dir, "/figures"), width = 10, height = 10, units = "cm", dpi = 1200)
ggsave(plot = conf_disengage_s2, filename = "s2_conflict_disengage.png",
       path = str_c(main_dir, "/figures"), width = 10, height = 10, units = "cm", dpi = 1200)

save(conf_random_s2,    file = str_c(main_dir, "/figures/ggplot/s2_conflict_random_ggplot.RData"))
save(conf_copy_s2,      file = str_c(main_dir, "/figures/ggplot/s2_conflict_copy_ggplot.RData"))
save(conf_disengage_s2, file = str_c(main_dir, "/figures/ggplot/s2_conflict_disengage_ggplot.RData"))

## Patchwork ----
conflict_plot_s2 <- conflict_plot_s2 +
  labs(title = "Moral conflict")

conf_random_s2 <- conf_random_s2 +
  labs(title = "Random choice")
conf_copy_s2 <- conf_copy_s2 +
  labs(title = "Copy ingroup")
conf_disengage_s2 <- conf_disengage_s2 +
  labs(title = "Disengagement")

conflict_resolution_plot_s2 <- (conflict_plot_s2 | conf_random_s2 | conf_copy_s2 | conf_disengage_s2) +
  plot_annotation(
    tag_levels = "A"
  ) &
  coord_flip() &
  labs(caption = NULL, x = NULL) &
  theme(plot.margin = unit(c(0,0.15,0,0), "cm"),
        plot.tag = element_text(size = 7),
        axis.text.y = element_text(hjust = 0.5, vjust = 0.5, angle = 90)) & x_id

ggsave(plot = conflict_resolution_plot_s2,
       filename = "s2_conflict_all.png",
       path = str_c(main_dir, "/figures"), width = 16, height = 4.5, units = "cm", dpi = 1200)
save(conflict_resolution_plot_s2,
     file = str_c(main_dir, "/figures/ggplot/s2_conflict_all_ggplot.RData"))

## Patchwork ----
conflict_resolution_plot_s2 <- (conf_random_s2 | conf_copy_s2 | conf_disengage_s2) +
  plot_annotation(
    tag_levels = "A"
  ) &
  coord_flip() &
  labs(caption = NULL, x = NULL) &
  theme(plot.margin = unit(c(0,0.15,0,0), "cm"),
        plot.tag = element_text(size = 7),
        axis.text.y = element_text(hjust = 0.5, vjust = 0.5, angle = 90)) & x_id

ggsave(plot = conflict_resolution_plot_s2,
       filename = "s2_conflict_res_all.png",
       path = str_c(main_dir, "/figures"), width = 16, height = 4.5, units = "cm", dpi = 1200)
save(conflict_resolution_plot_s2,
     file = str_c(main_dir, "/figures/ggplot/s2_conflict_res_ggplot.RData"))

rm(conf_random_s2, conf_copy_s2, conf_disengage_s2, conflict_plot_s2,
   conflict_resolution_plot_s2, conf_agg_s2_only, conf_sample_s2_only, conf_data_s2_only)
gc()
