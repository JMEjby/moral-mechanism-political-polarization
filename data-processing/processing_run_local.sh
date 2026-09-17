#!/bin/sh
# Local runner for the data-processing stage (no Grid Engine / module loads).
#
# Usage: from inside the data-processing/ folder, run
#     sh processing_run_local.sh
#
# Prerequisites:
#   * R with data.table, stringr, igraph and arrow installed in your system
#     library
#   * csvkit (provides csvstack) for the combine step:  pip install csvkit
#
# Reads raw simulation output from ../sim-pack-full-model/outputs/, writes all
# intermediate data to intermediate/, and writes the final parquet to ../data/.

export RUN_MODE=local

Rscript 1-turtle_post_processing.R
Rscript 2-link_post_processing.R

# combine the per-tick LINK csvs into one raw file
csvstack ./intermediate-data/links/*.csv > ./intermediate-data/Link_data_raw.csv

Rscript 3-link_post_processing_2.R
Rscript 4-cluster_post_processing.R
Rscript 5-parquet_turtle_data.R
