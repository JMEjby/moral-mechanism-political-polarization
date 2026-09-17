script <- "results_figures"
source("support-functions/local_full_setup.R") # switch to remote_setup.R for remote runs
source("support-functions/reduce_plots.R")

drop_leg <- guides(colour = "none", fill = "none", linetype = "none")
pol_theme_mod <- theme(plot.tag = element_text(size = title),
                       margins = unit(c(0,0,0,0), "cm"))
pol_scale <- scale_x_discrete(labels = c("Conservative", "Liberal"))

# s1 pol outcomes -----
#Added spaces, if we want to separate them again, the 3x2 grid was 8cm tall
load(str_c(main_dir, "/figures/ggplot/s1_accuracy_agg_ggplot.RData"))
load(str_c(main_dir, "/figures/ggplot/s1_actual_ingroup_agg_ggplot.RData"))
load(str_c(main_dir, "/figures/ggplot/s1_happy_ggplot.RData"))
load(str_c(main_dir, "/figures/ggplot/s1_perceived_ingroup_agg_ggplot.RData"))
load(str_c(main_dir, "/figures/ggplot/s1_homog_agg_ggplot.RData"))
load(str_c(main_dir, "/figures/ggplot/s1_prop_clustered_agg_ggplot.RData"))
load(str_c(main_dir, "/figures/ggplot/s1_space_ggplot.RData"))

# Prepare individual panels with cleaned labels
happy_plot <- happy_plot + labs(title = "Agent satisfaction", y = "Iteration proportion") + theme(plot.margin = unit(c(0,0.2,0,0), "cm"))
accuracy_agg_plot_s1 <- accuracy_agg_plot_s1 + labs(title = "Ingroup inference accuracy", y = "Accuracy index") + theme(plot.margin = unit(c(0.2,0,0,0), "cm"))
perceived_agg_plot_s1 <- perceived_agg_plot_s1 + labs(title = "Perceived ingroup neighbors", y = "Neighbor proportion") + theme(plot.margin = unit(c(0.2,0,0,0), "cm"))
actual_agg_plot_s1 <- actual_agg_plot_s1 + labs(title = "Actual ingroup neighbors", y = "Neighbor proportion") + theme(plot.margin = unit(c(0.2,0,0,0), "cm"))
homog_agg_plot_s1 <- homog_agg_plot_s1 + labs(title = "Cluster homogeneity", y = "Agent proportion") + theme(plot.margin = unit(c(0,0.2,0,0), "cm"))
prop_clustered_agg_plot_s1  <- prop_clustered_agg_plot_s1 + labs(title = "Proportion iterations in cluster", y = "Iteration proportion") + theme(plot.margin = unit(c(0,0,0,0), "cm"))
s1_space <- s1_space + labs(title = "Model space in example simulations") + theme(plot.margin = unit(c(0,0,0.2,0), "cm"))


## 3x2 variant (3 columns, 2 rows): satis/hom/prop_clustered | perc/act/acc ----
(
  s1_space / (
    (happy_plot + homog_agg_plot_s1  + prop_clustered_agg_plot_s1) /
      (perceived_agg_plot_s1  + actual_agg_plot_s1  + accuracy_agg_plot_s1)
  ) & theme(axis.text.y = element_text(angle = 90, hjust = 0.5, vjust = 0.5)) & coord_flip() & pol_scale
)+ 
  plot_layout(heights = c(50,50)) +
  plot_annotation(tag_levels = 'A') & pol_theme_mod & labs(caption = NULL)

ggsave(filename = "s1_polarisation_measures_3x2.png",
       path = str_c(main_dir, "/figures"),
       width = 16, height = 16.5, units = "cm", dpi = 1200)

rm(happy_plot, accuracy_agg_plot_s1, perceived_agg_plot_s1, actual_agg_plot_s1, homog_agg_plot_s1, prop_clustered_agg_plot_s1)
gc()

# s2 pol outcomes ----
load(str_c(main_dir, "/figures/ggplot/s2_accuracy_agg_ggplot.RData"))
load(str_c(main_dir, "/figures/ggplot/s2_actual_ingroup_agg_ggplot.RData"))
load(str_c(main_dir, "/figures/ggplot/s2_happy_ggplot.RData"))
load(str_c(main_dir, "/figures/ggplot/s2_perceived_ingroup_agg_ggplot.RData"))
load(str_c(main_dir, "/figures/ggplot/s2_homog_agg_ggplot.RData"))
load(str_c(main_dir, "/figures/ggplot/s2_prop_clustered_agg_ggplot.RData"))
load(str_c(main_dir, "/figures/ggplot/s2_space_ggplot.RData"))

# Prepare individual panels with cleaned labels
happy_plot_s2 <- happy_plot_s2 + labs(title = "Agent satisfaction", y = "Iteration proportion") + theme(plot.margin = unit(c(0,0.2,0,0), "cm"))
accuracy_agg_plot_s2_only <- accuracy_agg_plot_s2_only + labs(title = "Ingroup inference accuracy", y = "Accuracy index") + theme(plot.margin = unit(c(0.2,0,0,0), "cm"))
perceived_agg_plot_s2_only <- perceived_agg_plot_s2_only + labs(title = "Perceived ingroup neighbors", y = "Neighbor proportion") + theme(plot.margin = unit(c(0.2,0,0,0), "cm"))
actual_agg_plot_s2_only <- actual_agg_plot_s2_only + labs(title = "Actual ingroup neighbors", y = "Neighbor proportion") + theme(plot.margin = unit(c(0.2,0,0,0), "cm"))
homog_agg_plot_s2_only <- homog_agg_plot_s2_only + labs(title = "Cluster homogeneity", y = "Agent proportion") #+ theme(plot.margin = unit(c(0,0.2,0,0), "cm"))
prop_clustered_agg_plot_s2_only  <- prop_clustered_agg_plot_s2_only + labs(title = "Proportion iterations in cluster", y = "Iteration proportion") + theme(plot.margin = unit(c(0,0,0,0), "cm"))
s2_space <- s2_space + labs(title = "Model space in example simulations") + theme(plot.margin = unit(c(0,0,0.2,0), "cm"))
# Shared modifiers applied to all panels via patchwork &
pol_theme_mod <- theme(plot.tag = element_text(size = title),
                       margins = unit(c(0,0,0,0), "cm"))
pol_scale <- scale_x_discrete(labels = c("Conservative", "Liberal"))

## 3x2 variant (3 columns, 2 rows): satis/hom/prop_clustered | perc/act/acc ----
(
  s2_space / (
    (happy_plot_s2 + homog_agg_plot_s2_only  + prop_clustered_agg_plot_s2_only) /
      (perceived_agg_plot_s2_only  + actual_agg_plot_s2_only  + accuracy_agg_plot_s2_only)
  ) & theme(axis.text.y = element_text(angle = 90, hjust = 0.5, vjust = 0.5)) & coord_flip() & pol_scale
)+  
  plot_layout(heights = c(50,50)) +
  plot_annotation(tag_levels = 'A') & pol_theme_mod & labs(caption = NULL)

ggsave(filename = "s2_polarisation_measures_3x2.png",
       path = str_c(main_dir, "/figures"),
       width = 16, height = 16.5, units = "cm", dpi = 1200)

rm(happy_plot_s2, accuracy_agg_plot_s2_only, perceived_agg_plot_s2_only, actual_agg_plot_s2_only, homog_agg_plot_s2_only, prop_clustered_agg_plot_s2_only)
gc()

# s1 moral outcomes ----
load(str_c(main_dir, "/figures/ggplot/s1_moral_values_violin_ggplot.RData"))
load(str_c(main_dir, "/figures/ggplot/s1_drift_time_id_1x5_ggplot.RData"))
load(str_c(main_dir, "/figures/ggplot/s1_conflict_random_ggplot.RData"))
load(str_c(main_dir, "/figures/ggplot/s1_conflict_copy_ggplot.RData"))
load(str_c(main_dir, "/figures/ggplot/s1_conflict_disengage_ggplot.RData"))

p_time <- p_time +
  labs(title = "Moral values") +
  base_theme +
  theme(legend.position = "top") 

drift_time_id_plot_s1_c5 <- 
  drift_time_id_plot_s1_c5  +
  labs(title = "Moral drift") +
  base_theme + 
  theme(legend.position = "none")

conf_p <- (conf_copy_s1 + labs(title = "Disambiguiation via ingroup") | 
             conf_disengage_s1 + labs(title = "Moral disengagement") |
             conf_random_s1 + labs(title = "Random choice")) & 
  coord_flip() &
  labs(caption = NULL, y = "Proportion of conflicts") &
  base_theme &
  theme(axis.text.y = element_text(hjust = 0.5, vjust = 0.5, angle = 90),
        legend.position = "none") & 
  pol_scale 


((p_time / drift_time_id_plot_s1_c5) / conf_p) +
  plot_layout(heights = c(0.48, 0.2, 0.32)) +
  plot_annotation(tag_levels = "A") &
  pol_theme_mod

ggsave(filename = "s1_moral_measures.png",
       path = str_c(main_dir, "/figures"),
       width = 16, height = 14, units = "cm", dpi = 1200)

rm(p_time, drift_time_id_plot_s1_c5, conf_p, conf_copy_s1, conf_disengage_s1, conf_random_s1)
gc()

# s2 moral outcomes ----
load(str_c(main_dir, "/figures/ggplot/s2_moral_values_violin_ggplot.RData"))
load(str_c(main_dir, "/figures/ggplot/s2_drift_time_id_1x5_ggplot.RData"))
load(str_c(main_dir, "/figures/ggplot/s2_conflict_random_ggplot.RData"))
load(str_c(main_dir, "/figures/ggplot/s2_conflict_copy_ggplot.RData"))
load(str_c(main_dir, "/figures/ggplot/s2_conflict_disengage_ggplot.RData"))

p_time_2 <- p_time_2 +
  labs(title = "Moral values") +
  base_theme +
  theme(legend.position = "top") 

drift_time_id_plot_s2_c5 <- 
  drift_time_id_plot_s2_c5  +
  labs(title = "Moral drift") +
  base_theme + 
  theme(legend.position = "none")

conf_p <- (conf_copy_s2 + labs(title = "Disambiguiation via ingroup") | 
             conf_disengage_s2 + labs(title = "Moral disengagement") |
             conf_random_s2 + labs(title = "Random choice")) & 
  coord_flip() &
  labs(caption = NULL, y = "Proportion of conflicts") &
  base_theme &
  theme(axis.text.y = element_text(hjust = 0.5, vjust = 0.5, angle = 90),
        legend.position = "none") & 
  pol_scale 


((p_time_2 / drift_time_id_plot_s2_c5) / conf_p) +
  plot_layout(heights = c(0.48, 0.2, 0.32)) +
  plot_annotation(tag_levels = "A") &
  pol_theme_mod

ggsave(filename = "s2_moral_measures.png",
       path = str_c(main_dir, "/figures"),
       width = 16, height = 14, units = "cm", dpi = 1200)

rm(p_time_2, drift_time_id_plot_s2_c5, conf_p, conf_copy_s2, conf_disengage_s2, conf_random_s2)
gc()

# s1 pol time ----
load(str_c(main_dir, "/figures/ggplot/s1_homog_time_nofacet_ggplot.RData"))
homog_time_plot_s1_nofacet <- slim_ggplot(homog_time_plot_s1_nofacet)

load(str_c(main_dir, "/figures/ggplot/s1_time_happy_ggplot.RData"))
time_happy_plot <- slim_ggplot(time_happy_plot)

load(str_c(main_dir, "/figures/ggplot/s1_time_happy_perc_faceted_ggplot.RData"))
time_happy_perc_plot_faceted <- slim_ggplot(time_happy_perc_plot_faceted)

load(str_c(main_dir, "/figures/ggplot/s1_time_happy_split_ggplot.RData"))

load(str_c(main_dir, "/figures/ggplot/s1_perceived_ingroup_time_nofacet_ggplot.RData"))
perceived_time_plot_s1_nofacet <- slim_ggplot(perceived_time_plot_s1_nofacet)

load(str_c(main_dir, "/figures/ggplot/s1_actual_ingroup_time_nofacet_ggplot.RData"))
actual_time_plot_s1_nofacet <- slim_ggplot(actual_time_plot_s1_nofacet)

load(str_c(main_dir, "/figures/ggplot/s1_accuracy_time_nofacet_ggplot.RData"))
accuracy_time_plot_s1_nofacet <- slim_ggplot(accuracy_time_plot_s1_nofacet)

load(str_c(main_dir, "/figures/ggplot/s1_prop_clustered_time_faceted_ggplot.RData"))
prop_clustered_time_plot_s1_faceted <- slim_ggplot(prop_clustered_time_plot_s1_faceted)

time_happy_plot <- time_happy_plot + labs(title = "Agent satisfaction over time", y = "Agent proportion") + drop_leg
time_happy_perc_plot_faceted <- time_happy_perc_plot_faceted + labs(title = "Agent satisfaction percentiles over time", y = "Agent proportion") + drop_leg  + theme(plot.margin = unit(c(0,0.2,0,0), "cm"))
time_happy_plot_split <- time_happy_plot_split + labs(title = "Agent satisfaction by overall satisfaction over time", y = "Agent proportion")  + theme(plot.margin = unit(c(0,0.2,0,0), "cm"))
perceived_time_plot_s1_nofacet <- perceived_time_plot_s1_nofacet + labs(title = "Perceived ingroup neighbors over time", y = "Neighbor proportion") + drop_leg
actual_time_plot_s1_nofacet <- actual_time_plot_s1_nofacet+ labs(title = "Actual ingroup neighbors over time over time", y = "Neighbor proportion") + drop_leg
accuracy_time_plot_s1_nofacet <- accuracy_time_plot_s1_nofacet + labs(title = "Ingroup inference accuracy over time", y = "Accuracy index") + drop_leg + theme(plot.margin = unit(c(0,0.2,0,0), "cm"))
prop_clustered_time_plot_s1_faceted <- prop_clustered_time_plot_s1_faceted + labs(title = "Proportion agents in clusters over time", y = "Iteration proportion") + drop_leg
homog_time_plot_s1_nofacet <- homog_time_plot_s1_nofacet + labs(title = "Cluster homogeneity over time", y = "Proportion iterations in cluster") + drop_leg  + theme(plot.margin = unit(c(0,0.2,0,0), "cm"))

s1_time_layout <- "
ABB
CCC
DEF
GGH
"

(
  time_happy_plot + time_happy_perc_plot_faceted + time_happy_plot_split +
    perceived_time_plot_s1_nofacet + actual_time_plot_s1_nofacet + accuracy_time_plot_s1_nofacet +
    prop_clustered_time_plot_s1_faceted + homog_time_plot_s1_nofacet
) + 
  plot_annotation(tag_levels = 'A') + 
  plot_layout(design = s1_time_layout, guides = 'collect') & 
  theme(legend.position = "bottom") & 
  pol_theme_mod &
  labs(caption = NULL, color = "Agent political identity") 

ggsave(filename = "s1_polarisation_time_measures.png",
       path = str_c(main_dir, "/figures"),
       width = 16, height = 19, units = "cm", dpi = 1200)

rm(time_happy_plot, time_happy_perc_plot_faceted, time_happy_plot_split,
     perceived_time_plot_s1_nofacet, actual_time_plot_s1_nofacet, accuracy_time_plot_s1_nofacet,
     prop_clustered_time_plot_s1_faceted, homog_time_plot_s1_nofacet)
gc()

# s1-2 pol outcomes ----
load(str_c(main_dir, "/figures/ggplot/s1_s2_accuracy_agg_ggplot.RData"))
load(str_c(main_dir, "/figures/ggplot/s1_s2_actual_ingroup_agg_ggplot.RData"))
load(str_c(main_dir, "/figures/ggplot/s1_s2_happy_ggplot.RData"))
load(str_c(main_dir, "/figures/ggplot/s1_s2_perceived_ingroup_agg_ggplot.RData"))
load(str_c(main_dir, "/figures/ggplot/s1_s2_happy_ggplot.RData"))
load(str_c(main_dir, "/figures/ggplot/s1_s2_homog_agg_ggplot.RData"))
load(str_c(main_dir, "/figures/ggplot/s1_s2_prop_clustered_agg_ggplot.RData"))

# Prepare individual panels with cleaned labels
happy_plot <- happy_plot + labs(title = "Agent satisfaction", y = "Iteration proportion") + theme(plot.margin = unit(c(0,0.1,0,0), "cm"))
accuracy_agg_plot_s2 <- accuracy_agg_plot_s2 + labs(title = "Ingroup inference accuracy", y = "Accuracy index") + theme(plot.margin = unit(c(0.1,0,0,0), "cm"))
perceived_agg_plot_s2 <- perceived_agg_plot_s2 + labs(title = "Perceived ingroup neighbors", y = "Neighbor proportion") + theme(plot.margin = unit(c(0.1,0.1,0,0), "cm"))
actual_agg_plot_s2 <- actual_agg_plot_s2 + labs(title = "Actual ingroup neighbors", y = "Neighbor proportion") + theme(plot.margin = unit(c(0.1,0.1,0,0), "cm"))
homog_agg_plot_s2 <- homog_agg_plot_s2 + labs(title = "Cluster homogeneity", y = "Agent proportion") + theme(plot.margin = unit(c(0,0.1,0,0), "cm"))
prop_clustered_agg_plot_s2  <- prop_clustered_agg_plot_s2 + labs(title = "Proportion iterations in cluster", y = "Iteration proportion") + theme(plot.margin = unit(c(0,0,0,0), "cm"))

# Shared modifiers applied to all panels via patchwork &
pol_theme_mod <- theme(plot.tag = element_text(size = title),
                       axis.text.y = element_text(angle = 90, hjust = 0.5, vjust = 0.5),
                       margins = unit(c(0,0,0,0), "cm"))
pol_scale <- scale_x_discrete(labels = c("Conservative\nStudy 1", "Liberal\nStudy 1","Conservative\nStudy 2", "Liberal\nStudy 2"))

(
  (happy_plot + homog_agg_plot_s2  + prop_clustered_agg_plot_s2) /
    (perceived_agg_plot_s2  + actual_agg_plot_s2  + accuracy_agg_plot_s2)
) + plot_annotation(tag_levels = 'A') & pol_theme_mod & coord_flip() & pol_scale & labs(caption = NULL)

ggsave(filename = "s1_s2_polarisation_measures_3x2.png",
       path = str_c(main_dir, "/figures"),
       width = 16, height = 12, units = "cm", dpi = 1200)

# s2 pol time ----
load(str_c(main_dir, "/figures/ggplot/s2_time_happy_ggplot.RData"))
time_happy_plot_s2 <- slim_ggplot(time_happy_plot_s2)

load(str_c(main_dir, "/figures/ggplot/s2_time_happy_perc_ggplot.RData"))

load(str_c(main_dir, "/figures/ggplot/s2_time_happy_perc_faceted_ggplot.RData"))
time_happy_perc_plot_s2_faceted <- slim_ggplot(time_happy_perc_plot_s2_faceted)

load(str_c(main_dir, "/figures/ggplot/s2_perceived_ingroup_time_nofacet_ggplot.RData"))
perceived_time_plot_s2_only_nofacet <- slim_ggplot(perceived_time_plot_s2_only_nofacet)

load(str_c(main_dir, "/figures/ggplot/s2_actual_ingroup_time_nofacet_ggplot.RData"))
actual_time_plot_s2_only_nofacet <- slim_ggplot(actual_time_plot_s2_only_nofacet)

load(str_c(main_dir, "/figures/ggplot/s2_accuracy_time_nofacet_ggplot.RData"))
accuracy_time_plot_s2_only_nofacet <- slim_ggplot(accuracy_time_plot_s2_only_nofacet)

load(str_c(main_dir, "/figures/ggplot/s2_prop_clustered_time_faceted_ggplot.RData"))
prop_clustered_time_plot_s2_only_faceted <- slim_ggplot(prop_clustered_time_plot_s2_only_faceted)

load(str_c(main_dir, "/figures/ggplot/s2_homog_time_nofacet_ggplot.RData"))
homog_time_plot_s2_only_nofacet <- slim_ggplot(homog_time_plot_s2_only_nofacet)

time_happy_plot_s2 <- time_happy_plot_s2 + labs(title = "Agent satisfaction over time", y = "Agent proportion") + drop_leg
time_happy_perc_plot_s2_faceted <- time_happy_perc_plot_s2_faceted + labs(title = "Agent satisfaction percentiles over time", y = "Agent proportion") + drop_leg  + theme(plot.margin = unit(c(0,0.2,0,0), "cm"))
time_happy_plot_split_s2 <- time_happy_plot_split_s2 + labs(title = "Agent satisfaction by overall satisfaction over time", y = "Agent proportion")  + theme(plot.margin = unit(c(0,0.2,0,0), "cm"))
perceived_time_plot_s2_only_nofacet <- perceived_time_plot_s2_only_nofacet + labs(title = "Perceived ingroup neighbors over time", y = "Neighbor proportion") + drop_leg
actual_time_plot_s2_only_nofacet <- actual_time_plot_s2_only_nofacet+ labs(title = "Actual ingroup neighbors over time over time", y = "Neighbor proportion") + drop_leg
accuracy_time_plot_s2_only_nofacet <- accuracy_time_plot_s2_only_nofacet + labs(title = "Ingroup inference accuracy over time", y = "Accuracy index") + drop_leg + theme(plot.margin = unit(c(0,0.2,0,0), "cm"))
prop_clustered_time_plot_s2_only_faceted <- prop_clustered_time_plot_s2_only_faceted + labs(title = "Proportion agents in clusters over time", y = "Iteration proportion") + drop_leg
homog_time_plot_s2_only_nofacet <- homog_time_plot_s2_only_nofacet + labs(title = "Cluster homogeneity over time", y = "Proportion iterations in cluster") + drop_leg  + theme(plot.margin = unit(c(0,0.2,0,0), "cm"))

s1_time_layout <- "
ABB
CCC
DEF
GGH
"

(
  time_happy_plot_s2 + time_happy_perc_plot_s2_faceted + time_happy_plot_split_s2 +
    perceived_time_plot_s2_only_nofacet + actual_time_plot_s2_only_nofacet + accuracy_time_plot_s2_only_nofacet +
    prop_clustered_time_plot_s2_only_faceted + homog_time_plot_s2_only_nofacet
) + 
  plot_annotation(tag_levels = 'A') + 
  plot_layout(design = s1_time_layout, guides = 'collect') & 
  theme(legend.position = "bottom") & 
  pol_theme_mod &
  labs(caption = NULL, color = "Agent political identity") 

ggsave(filename = "s2_polarisation_time_measures.png",
       path = str_c(main_dir, "/figures"),
       width = 16, height = 19, units = "cm", dpi = 1200)

rm(time_happy_plot_s2 + time_happy_perc_plot_s2_faceted + time_happy_plot_split_s2 +
     perceived_time_plot_s2_only_nofacet + actual_time_plot_s2_only_nofacet + accuracy_time_plot_s2_only_nofacet +
     prop_clustered_time_plot_s2_only_faceted + homog_time_plot_s2_only_nofacet)
gc()
# s1-s2 moral start weights ----
load(str_c(main_dir, "/figures/ggplot/s1_values_start_ggplot.RData"))
load(str_c(main_dir, "/figures/ggplot/s2_values_start_ggplot.RData"))

(
  (start_plot_s1 + labs(title = "Study 1")) + 
  (start_plot_s2 + labs(title = "Study 2"))
) + 
  plot_annotation(tag_levels = 'A') + 
  plot_layout(guides = 'collect') & 
  theme(legend.position = "bottom", plot.tag = element_text(size = title)) 

ggsave(filename = "s1_s2_values_start.png",
       path = str_c(main_dir, "/figures"), width = 16, height = 10, units = "cm", dpi = 1200)

# s1-s2 moral signal ----
load(str_c(main_dir, "/figures/ggplot/s1_time_signal_id_ggplot.RData"))

time_signal_id_plot_s1 + 
  facet_wrap(~mf, ncol = 2) + 
  labs(caption = NULL,
       title = "Moral signals",
       color = "Political identity",
       fill = "Political identity",
       y = "Agent proportion") +
  theme(legend.position = "bottom") 

ggsave(filename = "s1_moral_signals_time.png",
       path = str_c(main_dir, "/figures"), width = 16, height = 18, units = "cm", dpi = 1200)

load(str_c(main_dir, "/figures/ggplot/s2_time_signal_id_ggplot.RData"))

time_signal_id_plot_s2 + 
  facet_wrap(~mf, ncol = 2) + 
  labs(caption = NULL,
       title = "Moral signals",
       color = "Political identity",
       fill = "Political identity",
       y = "Agent proportion") +
  theme(legend.position = "bottom") 

ggsave(filename = "s2_moral_signals_time.png",
       path = str_c(main_dir, "/figures"), width = 16, height = 18, units = "cm", dpi = 1200)
