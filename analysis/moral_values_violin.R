script <- "moral_violin"
source("support-functions/local_full_setup.R") # switch to remote_setup.R for remote runs

moral <- melt(turtle_cluster_data, 
              measure.vars = c("care_weight", 
                               "fairness_weight", 
                               "ingroup_loyalty_weight", 
                               "authority_weight", 
                               "purity_weight"),
              variable.name = "mf",
              value.name = "weight")

moral$identity <- factor(moral$identity, labels = c("Conservative", "Liberal"))
moral$mf <- factor(moral$mf, labels = c("Care", "Fairness", "Ingroup loyalty", "Authority", "Purity"))

moral <- moral[, study := fifelse(sample <= 150, "Study 1", "Study 2")][, c("Mean","sd") := .(mean(weight), sd(weight)),
               by = .(identity, 
                      tick,
                      mf, 
                      study
               )] [, .("sample_sd" = sd(weight),
                       "sample_mean_weight" = mean(weight)),
                   by = .(mf, 
                          identity, 
                          sample, 
                          tick,
                          study,
                          sd,
                          Mean
                   )]

moral_distr <- melt(turtle_cluster_data[tick %in% seq(0,200,50),], 
                    measure.vars = c("care_weight", 
                                     "fairness_weight", 
                                     "ingroup_loyalty_weight", 
                                     "authority_weight", 
                                     "purity_weight"),
                    variable.name = "mf",
                    value.name = "weight") [, study := fifelse(sample <= 150, "Study 1", "Study 2")]

moral_distr$identity <- factor(moral_distr$identity, labels = c("Conservative", "Liberal"))
moral_distr$mf <- factor(moral_distr$mf, labels = c("Care", "Fairness", "Ingroup loyalty", "Authority", "Purity"))
rm(turtle_cluster_data); gc()

# p_violin <- ggplot(data = moral_distr, aes(x=tick, y= weight))+
#   geom_violin(aes(fill = identity, group = interaction(identity,tick)), alpha = 0.3)+
#   scale_fill_manual(values = s1_cols)+
#   facet_wrap(~mf, nrow = 1) +
#   labs(y = "Moral value weight", x= "Number of iterations",
#        fill = "Political identity")+
#   ylim(0,5)+
#   base_theme + theme(legend.position = "top", strip.text = element_blank())

p_time <- ggplot(data = moral[sample<= 150,], aes(x=tick, y= Mean))+
  geom_violin(data = moral_distr, aes(x=tick, y= weight, fill = identity, group = interaction(identity,tick)), 
              alpha = rib_alpha, width = 25)+
  #geom_ribbon(alpha = rib_alpha, aes(ymin = Mean-sd, ymax = Mean+sd, fill = identity)) +
  geom_line(alpha = s_alpha, aes(y = sample_mean_weight, colour = identity,
                              group = interaction(sample, identity, mf)))+
  geom_line(aes(colour = identity), linewidth = m_line)+
  facet_grid(identity~mf)+
  scale_color_manual(values = s1_cols)+
  scale_fill_manual(values = s1_cols)+
  labs(y = "Moral value weight", x= "Number of iterations", color = "Political identity",
       fill = "Political identity")+
  scale_y_continuous(limits = c(0, 5),
                     expand = expansion(mult = c(0.05,0.05)))+
  scale_x_continuous(limits = c(-13, 213),
                     breaks = seq(1, 201, by = 50),
                     labels = function(x) x - 1,
                     expand = expansion(mult = c(0.05,0.05))) +
  base_theme + theme(legend.position = "none")

ggsave(filename = "s1_weights_violin.png",path = str_c(main_dir, "/figures"), width = 16, height = 5, units = "cm", dpi = 1200)
save(p_time,
     file = str_c(main_dir, "/figures/ggplot/s1_moral_values_violin_ggplot.RData"))
#p_time/p_violin + plot_layout(guides = 'collect')

p_time_2 <- ggplot(data = moral[sample> 150,], aes(x=tick, y= Mean))+
  geom_violin(data = moral_distr, aes(x=tick, y= weight, 
                                      fill = identity, group = interaction(identity,tick)), 
              alpha = 0.3, width = 25)+
  #geom_ribbon(alpha = 0.3, aes(ymin = Mean-sd, ymax = Mean+sd, fill = identity)) +
  geom_line(alpha = s_alpha, aes(y = sample_mean_weight, colour = identity,
                              group = interaction(sample, identity, mf)))+
  geom_line(aes(colour = identity), linewidth = m_line)+
  facet_grid(identity~mf)+
  scale_color_manual(values = s2_cols[3:4])+
  scale_fill_manual(values = s2_cols[3:4])+
  labs(y = "Moral value weight", x= "Number of iterations", color = "Political identity",
       fill = "Political identity")+
  scale_y_continuous(limits = c(0, 5),
                     expand = expansion(mult = c(0.05,0.05)))+
  scale_x_continuous(limits = c(-13, 213),
                     breaks = seq(1, 201, by = 50),
                     labels = function(x) x - 1,
                     expand = expansion(mult = c(0.05,0.05))) +
  base_theme + theme(legend.position = "none")

ggsave(filename = "s2_weights_violin.png",path = str_c(main_dir, "/figures"), width = 16, height = 5, units = "cm", dpi = 1200)
save(p_time_2,
     file = str_c(main_dir, "/figures/ggplot/s2_moral_values_violin_ggplot.RData"))

p_time_3 <- ggplot(data = moral, aes(x=tick, y= Mean))+
  geom_violin(data = moral_distr, aes(x=tick, y= weight, 
                                      fill = interaction(identity,study), 
                                      group = interaction(identity,tick, study)), 
              alpha = rib_alpha,
              width = 25)+
  #geom_ribbon(alpha = 0.3, aes(ymin = Mean-sd, ymax = Mean+sd, fill = identity)) +
  geom_line(alpha = s_alpha, aes(y = sample_mean_weight, colour = interaction(identity,study),
                                 group = interaction(sample, identity, mf, study),
                                 linetype = study))+
  geom_line(aes(colour = interaction(identity,study), linetype = study), linewidth = m_line)+
  facet_grid(interaction(identity, study, sep = "\n")~mf)+
  scale_color_manual(values = s2_cols)+
  scale_fill_manual(values = s2_cols)+
  scale_linetype_manual(values = s2_lines) +
  labs(y = "Moral value weight", x= "Number of iterations", color = "Political identity",
       fill = "Political identity")+
  scale_y_continuous(limits = c(0, 5),
                     expand = expansion(mult = c(0.05,0.05)))+
  scale_x_continuous(limits = c(-13, 213),
                     breaks = seq(1, 201, by = 50),
                     labels = function(x) x - 1,
                     expand = expansion(mult = c(0.05,0.05))) +
  base_theme + theme(legend.position = "none")

ggsave(filename = "s1_s2_weights_violin.png",path = str_c(main_dir, "/figures"), width = 16, height = 10, units = "cm", dpi = 1200)
save(p_time_3,
     file = str_c(main_dir, "/figures/ggplot/s1s2_moral_values_violin_ggplot.RData"))

ggplot(moral_distr[sample <= 150 & tick == 0,], 
                        aes(x=weight, y=sample, group = sample)) + 
  geom_density_ridges(aes(fill = identity), alpha = 0.5)+
  facet_grid(identity~mf) +
  scale_fill_manual(values = s1_cols)+
  labs(x = "Moral value weight", y= "Sample ID",
       fill = "Political identity",
       title = "Timepoint 0")+
  scale_x_continuous(limits = c(0, 5),
                     expand = expansion(mult = c(0.05,0.05)))+
  scale_y_continuous(limits = c(1, 155),
                     expand = expansion(mult = c(0,0)))+
  base_theme + theme(legend.position = "none")

ggsave(filename = "s1_weights_sample_ridgeline_t0.png",path = str_c(main_dir, "/figures"), width = 16, height = 16, units = "cm", dpi = 1200)

ggplot(moral_distr[sample <= 150 & tick == 50,], 
       aes(x=weight, y=sample, group = sample)) + 
  geom_density_ridges(aes(fill = identity), alpha = 0.5)+
  facet_grid(identity~mf) +
  scale_fill_manual(values = s1_cols)+
  labs(x = "Moral value weight", y= "Sample ID",
       fill = "Political identity",
       title = "Timepoint 50")+
  scale_x_continuous(limits = c(0, 5),
                     expand = expansion(mult = c(0.05,0.05)))+
  scale_y_continuous(limits = c(1, 155),
                     expand = expansion(mult = c(0,0)))+
  base_theme + theme(legend.position = "none")

ggsave(filename = "s1_weights_sample_ridgeline_t50.png",path = str_c(main_dir, "/figures"), width = 16, height = 16, units = "cm", dpi = 1200)

ggplot(moral_distr[sample <= 150 & tick == 100,], 
       aes(x=weight, y=sample, group = sample)) + 
  geom_density_ridges(aes(fill = identity), alpha = 0.5)+
  facet_grid(identity~mf) +
  scale_fill_manual(values = s1_cols)+
  labs(x = "Moral value weight", y= "Sample ID",
       fill = "Political identity",
       title = "Timepoint 100")+
  scale_x_continuous(limits = c(0, 5),
                     expand = expansion(mult = c(0.05,0.05)))+
  scale_y_continuous(limits = c(1, 155),
                     expand = expansion(mult = c(0,0)))+
  base_theme + theme(legend.position = "none")

ggsave(filename = "s1_weights_sample_ridgeline_t100.png",path = str_c(main_dir, "/figures"), width = 16, height = 16, units = "cm", dpi = 1200)

ggplot(moral_distr[sample <= 150 & tick == 150,], 
       aes(x=weight, y=sample, group = sample)) + 
  geom_density_ridges(aes(fill = identity), alpha = 0.5)+
  facet_grid(identity~mf) +
  scale_fill_manual(values = s1_cols)+
  labs(x = "Moral value weight", y= "Sample ID",
       fill = "Political identity",
       title = "Timepoint 150")+
  scale_x_continuous(limits = c(0, 5),
                     expand = expansion(mult = c(0.05,0.05)))+
  scale_y_continuous(limits = c(1, 155),
                     expand = expansion(mult = c(0,0)))+
  base_theme + theme(legend.position = "none")

ggsave(filename = "s1_weights_sample_ridgeline_t150.png",path = str_c(main_dir, "/figures"), width = 16, height = 16, units = "cm", dpi = 1200)

ggplot(moral_distr[sample <= 150 & tick == 200,], 
       aes(x=weight, y=sample, group = sample)) + 
  geom_density_ridges(aes(fill = identity), alpha = 0.5)+
  facet_grid(identity~mf) +
  scale_fill_manual(values = s1_cols)+
  labs(x = "Moral value weight", y= "Sample ID",
       fill = "Political identity",
       title = "Timepoint 200")+
  scale_x_continuous(limits = c(0, 5),
                     expand = expansion(mult = c(0.05,0.05)))+
  scale_y_continuous(limits = c(1, 155),
                     expand = expansion(mult = c(0,0)))+
  base_theme + theme(legend.position = "none")

ggsave(filename = "s1_weights_sample_ridgeline_t200.png",path = str_c(main_dir, "/figures"), width = 16, height = 16, units = "cm", dpi = 1200)

ggplot(moral_distr[sample > 150 & tick == 0,], aes(x=weight, y=sample, group = sample)) + 
  geom_density_ridges(aes(fill = identity), alpha = 0.5)+
  facet_grid(identity~mf) +
  scale_fill_manual(values = s2_cols[3:4])+
  labs(x = "Moral value weight", y= "Sample ID",
       fill = "Political identity",
       title = "Timepoint 0")+
  scale_x_continuous(limits = c(0, 5),
                     expand = expansion(mult = c(0.05,0.05)))+
  scale_y_continuous(limits = c(150, 305),
                     expand = expansion(mult = c(0.01,0)))+
  base_theme + theme(legend.position = "none")

ggsave(filename = "s2_weights_sample_ridgeline_t0.png",path = str_c(main_dir, "/figures"), width = 16, height = 16, units = "cm", dpi = 1200)

ggplot(moral_distr[sample > 150 & tick == 50,], aes(x=weight, y=sample, group = sample)) + 
  geom_density_ridges(aes(fill = identity), alpha = 0.5)+
  facet_grid(identity~mf) +
  scale_fill_manual(values = s2_cols[3:4])+
  labs(x = "Moral value weight", y= "Sample ID",
       fill = "Political identity",
       title = "Timepoint 50")+
  scale_x_continuous(limits = c(0, 5),
                     expand = expansion(mult = c(0.05,0.05)))+
  scale_y_continuous(limits = c(150, 305),
                     expand = expansion(mult = c(0.01,0)))+
  base_theme + theme(legend.position = "none")

ggsave(filename = "s2_weights_sample_ridgeline_t50.png",path = str_c(main_dir, "/figures"), width = 16, height = 16, units = "cm", dpi = 1200)

ggplot(moral_distr[sample > 150 & tick == 100,], aes(x=weight, y=sample, group = sample)) + 
  geom_density_ridges(aes(fill = identity), alpha = 0.5)+
  facet_grid(identity~mf) +
  scale_fill_manual(values = s2_cols[3:4])+
  labs(x = "Moral value weight", y= "Sample ID",
       fill = "Political identity",
       title = "Timepoint 100")+
  scale_x_continuous(limits = c(0, 5),
                     expand = expansion(mult = c(0.05,0.05)))+
  scale_y_continuous(limits = c(150, 305),
                     expand = expansion(mult = c(0.01,0)))+
  base_theme + theme(legend.position = "none")

ggsave(filename = "s2_weights_sample_ridgeline_t100.png",path = str_c(main_dir, "/figures"), width = 16, height = 16, units = "cm", dpi = 1200)

ggplot(moral_distr[sample > 150 & tick == 150,], aes(x=weight, y=sample, group = sample)) + 
  geom_density_ridges(aes(fill = identity), alpha = 0.5)+
  facet_grid(identity~mf) +
  scale_fill_manual(values = s2_cols[3:4])+
  labs(x = "Moral value weight", y= "Sample ID",
       fill = "Political identity",
       title = "Timepoint 150")+
  scale_x_continuous(limits = c(0, 5),
                     expand = expansion(mult = c(0.05,0.05)))+
  scale_y_continuous(limits = c(150, 305),
                     expand = expansion(mult = c(0.01,0)))+
  base_theme + theme(legend.position = "none")

ggsave(filename = "s2_weights_sample_ridgeline_t150.png",path = str_c(main_dir, "/figures"), width = 16, height = 16, units = "cm", dpi = 1200)

ggplot(moral_distr[sample > 150 & tick == 200,], aes(x=weight, y=sample, group = sample)) + 
  geom_density_ridges(aes(fill = identity), alpha = 0.5)+
  facet_grid(identity~mf) +
  scale_fill_manual(values = s2_cols[3:4])+
  labs(x = "Moral value weight", y= "Sample ID",
       fill = "Political identity",
       title = "Timepoint 200")+
  scale_x_continuous(limits = c(0, 5),
                     expand = expansion(mult = c(0.05,0.05)))+
  scale_y_continuous(limits = c(150, 305),
                     expand = expansion(mult = c(0.01,0)))+
  base_theme + theme(legend.position = "none")

ggsave(filename = "s2_weights_sample_ridgeline_t200.png",path = str_c(main_dir, "/figures"), width = 16, height = 16, units = "cm", dpi = 1200)

