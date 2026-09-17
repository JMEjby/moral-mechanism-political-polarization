source("processing_setup.R")

library(arrow)

load(paste0(interm, "Turtle_cluster.RData"))

write_parquet(turtle_cluster_data, paste0(data_dir, "turtle_cluster.parquet"))