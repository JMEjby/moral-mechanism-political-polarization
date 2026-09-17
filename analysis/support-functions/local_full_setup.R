main_dir <- getwd()
setwd(main_dir)

library(arrow)
library(data.table)
library(ggstatsplot)
library(ggrepel)
library(tidyverse)
library(ggalluvial)
library(patchwork)
library(ggridges)

source("support-functions/global_setup.R")

# data reading ----
data_path <- "../data/turtle_cluster.parquet"

if (exists("script")) {
  if (script == "results_figures") {
    # no data loading needed — assembles pre-saved plots only
  } else {
    turtle_cluster_data <- if (script == "moral_signals") {
      read_parquet(data_path, col_select = c(
        "sample", "tick", "who", "identity",
        "past_choices_1", "past_choices_2", "past_choices_3", "past_choices_4", "past_choices_5"
      ))
    } else if (script == "ingroup_inference") {
      read_parquet(data_path, col_select = c(
        "sample", "tick", "who", "identity", "cluster",
        "perceived_ingroup_n", "actual_ingroup_n", "total_neighbours",
        "hit_rate", "false_alarm"
      ))
    } else if (script %in% c("conflict_alluvial", "conflict_resolution")) {
      read_parquet(data_path, col_select = c(
        "sample", "tick", "who", "identity", "action_taken", "conflicted"
      ))
    } else if (script == "stability") {
      read_parquet(data_path, col_select = c(
        "sample", "tick", "who", "identity", "unhappy", "conflicted", "cluster"
      ))
    } else if (script == "space") {
      read_parquet(data_path, col_select = c(
        "sample", "tick", "who", "x_cor", "y_cor", "identity", "unhappy", "cluster",
        "perceived_ingroup_n", "actual_ingroup_n", "total_neighbours",
        "hit_rate", "false_alarm", "world_size", "density",
        "conflict_range", "ch-prev", "ch_prop", "ch_inv", "homophily", "distol", "dt_offset"
      ))
    } else if (script == "moral_clusters") {
      read_parquet(data_path, col_select = c(
        "sample", "tick", "who", "identity", "cluster",
        "care_weight", "fairness_weight", "ingroup_loyalty_weight",
        "authority_weight", "purity_weight"
      ))
    } else if (script %in% c("moral_drift", "moral_values")) {
      read_parquet(data_path, col_select = c(
        "sample", "tick", "who", "identity",
        "care_weight", "fairness_weight", "ingroup_loyalty_weight",
        "authority_weight", "purity_weight"
      ))
    } else if (script == "moral_violin") {
      read_parquet(data_path, col_select = c(
        "sample", "tick", "who", "identity",
        "care_weight", "fairness_weight", "ingroup_loyalty_weight",
        "authority_weight", "purity_weight"
      ))
    } else if (script == "radar") {
      read_parquet(data_path, col_select = c(
        "sample", "tick", "identity", "unhappy",
        "world_size", "density", "conflict_range", "ch-prev", "ch_prop",
        "ch_inv", "homophily", "distol", "dt_offset"
      ))
    } else {
      read_parquet(data_path)
    }
    setDT(turtle_cluster_data)
  }
}
