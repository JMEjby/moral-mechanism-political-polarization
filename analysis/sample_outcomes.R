library(tidyverse)
library(ggstatsplot)
library(patchwork)
library(ggh4x)
source("support-functions/global_setup.R")

sample_data <- read_csv("figures/s1_space_samples_outcomes.csv") |>
  filter(iter %in% c("Iteration  1", "Iteration  25" , "Iteration  50","Iteration  100","Iteration  150","Iteration  200")) |>
  pivot_longer(cols= c("satisfaction", "perc_ingroup", "act_ingroup",
                       "accuracy", "prop_clustered", "homogeniety"),
               values_to = "values",
               names_to = "outcome") |>
  mutate(iter = as.numeric(gsub("Iteration ", "", iter)),
         sample = factor(sample, levels = paste("Sample ", sort(unique(as.numeric(gsub("Sample ", "", sample)))))),
         values = case_when(outcome == "accuracy" ~ (values +1)/2,
                              .default = values),
         outcome = factor(outcome, levels = c("satisfaction", "perc_ingroup", "act_ingroup",
                                              "accuracy",  "prop_clustered", "homogeniety")),
         outcome = recode(outcome,
                          "satisfaction" = "Satisfaction",
                          "perc_ingroup" = "Perceived ingroup",
                          "act_ingroup" = "Actual ingroup",
                          "accuracy" = "Accuracy",
                          "prop_clustered" = "Proportion clustered",
                          "homogeniety" = "Cluster homogeneity"))

ggplot(sample_data, aes(x = iter, y = values, colour = interaction(sample, identity))) +
  geom_point(aes(shape = sample)) +
  geom_line(aes(group = interaction(sample, identity))) +
  labs(x = "Iteration",
       y = "Outcome") +
  guides(color = "none")+
  scale_shape_manual(values = c(0,1,2), name = "Samples") +
  scale_color_manual(values=c(s1_red_grad, s1_blue_grad),
                     name = "Samples") +
  facet_grid(identity~outcome, scales = "free")+
  scale_y_continuous(limits = c(0, 1))+
  base_theme +
  theme(legend.position = "bottom")

ggsave("figures/s1_space_samples_outcomes.png", width = 16, height = 8, units = "cm", dpi = 1200)


sample_data <- read_csv("figures/s2_space_samples_outcomes.csv") |>
  filter(iter %in% c("Iteration  1", "Iteration  25" , "Iteration  50","Iteration  100","Iteration  150","Iteration  200")) |>
  pivot_longer(cols= c("satisfaction", "perc_ingroup", "act_ingroup",
                       "accuracy", "prop_clustered", "homogeniety"),
               values_to = "values",
               names_to = "outcome") |>
  mutate(iter = as.numeric(gsub("Iteration ", "", iter)),
         sample = factor(sample, levels = paste("Sample ", sort(unique(as.numeric(gsub("Sample ", "", sample)))))),
         values = case_when(outcome == "accuracy" ~ (values +1)/2,
                            .default = values),
         outcome = factor(outcome, levels = c("satisfaction", "perc_ingroup", "act_ingroup",
                                              "accuracy",  "prop_clustered", "homogeniety")),
         outcome = recode(outcome,
                          "satisfaction" = "Satisfaction",
                          "perc_ingroup" = "Perceived ingroup",
                          "act_ingroup" = "Actual ingroup",
                          "accuracy" = "Accuracy",
                          "prop_clustered" = "Proportion clustered",
                          "homogeniety" = "Cluster homogeneity"))

ggplot(sample_data, aes(x = iter, y = values, colour = interaction(sample, identity))) +
  geom_point(aes(shape = sample)) +
  geom_line(aes(group = interaction(sample, identity))) +
  labs(x = "Iteration",
       y = "Outcome") +
  guides(color = "none")+
  scale_shape_manual(values = c(0,1,2), name = "Samples") +
  scale_color_manual(values=c(s2_red_grad, s2_blue_grad),
                     name = "Samples") +
  facet_grid(identity~outcome, scales = "free")+
  scale_y_continuous(limits = c(0, 1))+
  base_theme +
  theme(legend.position = "bottom")

ggsave("figures/s2_space_samples_outcomes.png", width = 16, height = 8, units = "cm", dpi = 1200)