script <- "conflict_alluvial"
source("support-functions/local_full_setup.R") # switch to remote_setup.R for remote runs

# settings ----
allu_theme <- 
  base_theme +
  theme(axis.ticks.x = element_blank(),
        legend.position = "top")

allu_label <- labs(y = "Proportion of agents",
                   fill = "Identity and \nstudy")

label_size <- 2.5
label_angle <- 90
scale_1 <-  1000
scale_2 <-  1000
scale_3 <-  1

# data ----
turtle_data <- turtle_cluster_data[tick > 0 & tick <= 201  ,][, 
                                                              "study":= fifelse(sample <= 150, 
                                                                                    "Study 1", 
                                                                                    "Study 2")] [, action_taken := factor(action_taken,
                                                                                                                                  levels = c("co", "dis", "ran", "cho", "NC"),
                                                                                                                                  labels = c("Copy \ningroup", "Disengage", "Choose \nrandomly", "Choose", "No choice"))]
turtle_data$identity <- factor(turtle_data$identity, labels = c("Conservative", "Liberal"))

conf_data <- turtle_data [, c("choice_exposure",
                              "conflicted") := .(fifelse(action_taken == "No choice", 
                                                         "No choice", 
                                                         "Choice"),
                                                 fifelse(conflicted == "true", 
                                                         "Conflicted", 
                                                         "Not conflicted"))]

rm(turtle_cluster_data); gc()

total_1 <- nrow(conf_data)

allu_1 <- conf_data[, .(Prop = .N/total_1), 
                    by =.(study,
                          identity,
                          choice_exposure, 
                          conflicted, 
                          action_taken)]|> 
  ggplot(
    aes(y = Prop,
        axis1 = identity, axis2 = choice_exposure, axis3 = conflicted, axis4 = action_taken)) +
  geom_alluvium(aes( fill = interaction(identity, study, sep = "\n")), 
                knot.pos = 0.5,
                alpha = 0.7,
                reverse = FALSE) +
  scale_fill_manual(values=s2_cols) +
  geom_stratum(reverse = FALSE) +
  geom_text(stat = "stratum",
            aes(label = after_stat(stratum),
                color = ifelse((after_stat(ymax) - after_stat(ymin)) < 0.1, NA, 'black')),
            reverse = FALSE,
            size = label_size,
            angle = label_angle) +
  geom_text_repel(stat = "stratum",
                  aes(label = after_stat(stratum),
                      color = ifelse((after_stat(ymax) - after_stat(ymin)) < 0.1 & x == 3, 'black', NA)),
                  reverse = FALSE,
                  segment.curvature = 0,
                  segment.size = 0.25,
                  hjust = "left",
                  nudge_x = -0.5,
                  nudge_y = 0.05,
                  direction = "y",
                  size = label_size
  )+
  geom_text_repel(stat = "stratum",
                  aes(label = after_stat(stratum),
                      color = ifelse((after_stat(ymax) - after_stat(ymin)) < 0.1 & x == 4, 'black', NA)),
                  reverse = FALSE,
                  segment.size = 0.25,
                  hjust = "right",
                  segment.curvature = 0,
                  nudge_x = -0.4,
                  nudge_y = 0.025,
                  direction = "y",
                  box.padding = 0.4,
                  size = label_size
  )+
  scale_color_identity()+
  scale_x_continuous(breaks = 1:4, 
                     limits = c(0.833,4.167),
                     labels = c("Identity", "Choice exposure", "Conflict state" ,"Action taken"),
                     expand = expansion(c(0,0))) +
  scale_y_continuous(limits = c(0,1), expand = expansion(c(0,0)),
                     sec.axis = sec_axis(~ . *total_1,
                                         name = "Number of million agents",
                                         breaks = c(0, total_1),
                                         labels = c(0,round(total_1 / scale_1,2)))) +
  allu_label+
  allu_theme

total_2 <- nrow(conf_data[choice_exposure == "Choice",])

allu_2 <- conf_data[choice_exposure == "Choice",][, .(Prop = .N/total_2), by =.(identity,
                                                                                         conflicted, 
                                                                                         action_taken, study)]|>
  ggplot(
    aes(y = Prop,
        axis1 = identity, axis2 = conflicted, axis3 = action_taken)) +
  geom_alluvium(aes( fill = interaction(identity, study, sep = "\n")), 
                knot.pos = 0.5,
                alpha = 0.7,
                reverse = FALSE) +
  scale_fill_manual(values=s2_cols) +
  geom_stratum(reverse = FALSE) +
  geom_text(stat = "stratum",
            aes(label = after_stat(stratum),
                color = ifelse(after_stat(y) < 0.1, NA, 'black')),
            reverse = FALSE,
            size = label_size,
            angle = label_angle) +
  geom_text_repel(stat = "stratum",
                  aes(label = after_stat(stratum),
                      color = ifelse(after_stat(y) < 0.1 & x == 3, 'black', NA)),
                  reverse = FALSE,
                  segment.size = 0.25,
                  hjust = "right",
                  segment.curvature = 0,
                  nudge_x = -0.4,
                  nudge_y = 0.025,
                  direction = "y",
                  box.padding = 0.4,
                  size = label_size
  )+
  geom_text_repel(stat = "stratum",
                  aes(label = after_stat(stratum),
                      color = ifelse(after_stat(y) < 0.1 & x == 2, 'black', NA)),
                  reverse = FALSE,
                  segment.size = 0.25,
                  hjust = "right",
                  segment.curvature = 0,
                  nudge_x = -0.4,
                  nudge_y = 0.025,
                  direction = "y",
                  box.padding = 0.4,
                  size = label_size
  )+
  scale_color_identity()+
  scale_x_continuous(breaks = 1:3, 
                     limits = c(0.833,3.167),
                     labels = c("Identity", "Conflict state" ,"Action taken"),
                     expand = expansion(c(0,0))) +
  scale_y_continuous(limits = c(0,1), expand = expansion(c(0,0)),
                     sec.axis = sec_axis(~ . * total_2,
                                         name = "Number of million agents",
                                         breaks = c(0,total_2),
                                         labels = c(0,round(total_2 / scale_2, 2))))+  
  allu_label +
  labs(title = "Choices with conflict") +
  allu_theme

total_3 <- nrow(conf_data[choice_exposure == "Choice" & 
                            conflicted == "Conflicted",])

allu_3 <- conf_data[choice_exposure == "Choice" & 
                      conflicted == "Conflicted",
                    .(Prop = .N/total_3),
                    keyby = .(action_taken,identity, study)]|>
  ggplot(
    aes(y = Prop,
        axis1 = identity, axis2 = action_taken)) +
  geom_alluvium(aes( fill = interaction(identity, study, sep = "\n")), 
                knot.pos = 0.5,
                alpha = 0.7,
                reverse = FALSE) +
  scale_fill_manual(values=s2_cols) +
  geom_stratum(reverse = FALSE) +
  geom_text(stat = "stratum",
            aes(label = after_stat(stratum)),
            reverse = FALSE,
            size = label_size) +
  scale_color_identity()+
  scale_y_continuous(limits = c(0,1), expand = expansion(c(0,0)),
                     sec.axis = sec_axis(~ . *total_3,
                                         name = "Number of thousand agents",
                                         breaks = c(0,total_3),
                                         labels = c(0,round(total_3 / scale_3, 2))))+ 
  scale_x_discrete(limits = c("identity", "action_taken"),
                   labels = c("Identity", "Action taken"),
                   expand = expansion(c(0,0)))+
  allu_label +
  labs(title = "Conflicting choices") +
  allu_theme

(allu_1 / allu_2 / allu_3) + plot_annotation(tag_levels = 'A') + plot_layout(guides = 'collect') & theme(legend.position = 'bottom')
ggsave("s1_s2_conflict_alluvial.png",path = str_c(main_dir, "/figures"), width = 16, height = 16, units = "cm", dpi = 1200)