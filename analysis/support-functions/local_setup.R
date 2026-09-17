library(arrow)
library(data.table)
library(ggstatsplot)
library(ggrepel)
library(tidyverse)
library(ggalluvial)
library(patchwork)
library(ggridges)

main_dir <- str_c(getwd(), "/test")

source("support-functions/global_setup.R")

# Line and point args ----
s_alpha <- 0.3 #0.03
p_alpha <- 0.3 #0.05
homog_alpha <- 0.5 #0.1
s_m_p_alpha <- 0.5 #0.05

# data reading ----
data_path <- "../data/turtle_cluster.parquet"

set.seed(1917169)
s1_samples <- sample(1:150,   5)
s2_samples <- sample(151:300, 5)
toy_samples <- c(s1_samples, s2_samples)
toy_ticks   <- c(0:25, 50,51, 100,101, 150,151, 200,201)

if (exists("script")) {
  if (script == "results_figures") {
    # no data loading needed — assembles pre-saved plots only
  } else {
    col_select <- if (script == "moral_signals") {
      c("sample", "tick", "who", "identity",
        "past_choices_1", "past_choices_2", "past_choices_3", "past_choices_4", "past_choices_5")
    } else if (script == "ingroup_inference") {
      c("sample", "tick", "who", "identity", "cluster",
        "perceived_ingroup_n", "actual_ingroup_n", "total_neighbours",
        "hit_rate", "false_alarm")
    } else if (script %in% c("conflict_alluvial", "conflict_resolution")) {
      c("sample", "tick", "who", "identity", "action_taken", "conflicted")
    } else if (script == "stability") {
      c("sample", "tick", "who", "identity", "unhappy", "conflicted", "cluster")
    } else if (script == "space") {
      c("sample", "tick", "who", "x_cor", "y_cor", "identity", "unhappy", "cluster",
        "perceived_ingroup_n", "actual_ingroup_n", "total_neighbours", "d_prime_ingroup")
    } else if (script == "moral_clusters") {
      c("sample", "tick", "who", "identity", "cluster",
        "care_weight", "fairness_weight", "ingroup_loyalty_weight",
        "authority_weight", "purity_weight")
    } else if (script %in% c("moral_drift", "moral_values", "moral_violin")) {
      c("sample", "tick", "who", "identity",
        "care_weight", "fairness_weight", "ingroup_loyalty_weight",
        "authority_weight", "purity_weight")
    } else if (script == "radar") {
      c("sample", "tick", "identity", "unhappy",
        "world_size", "density", "conflict_range", "ch-prev", "ch_prop",
        "ch_inv", "homophily", "distol", "dt_offset")
    } else {
      NULL  # read all columns
    }

    turtle_cluster_data <- open_dataset(data_path) |>
      filter(tick %in% toy_ticks, sample %in% toy_samples) |>
      (\(d) if (is.null(col_select)) d else select(d, all_of(col_select)))() |>
      collect() |>
      setDT()
  }
}