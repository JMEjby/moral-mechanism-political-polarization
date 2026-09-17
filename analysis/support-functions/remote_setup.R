main_dir <- "/exports/eddie/scratch/s1917169/moral-mechanism-political-polarization/analysis"
lib_dir <- "/exports/eddie/scratch/s1917169/libs"
setwd(main_dir)

library(arrow,       lib.loc = lib_dir)
library(data.table,  lib.loc = lib_dir)
library(ggstatsplot, lib.loc = lib_dir)
library(ggrepel,     lib.loc = lib_dir)
library(ggalluvial,  lib.loc = lib_dir)
library(ggridges,    lib.loc = lib_dir)
library(patchwork,   lib.loc = lib_dir)
library(tidyverse)

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
        "perceived_ingroup_n", "actual_ingroup_n", "total_neighbours", "d_prime_ingroup"
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