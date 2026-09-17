run_mode <- Sys.getenv("RUN_MODE", unset = "local")

# Cluster (Eddie) paths ----
remote_loc <- "/exports/eddie/scratch/s1917169/moral-mechanism-political-polarization/data-processing"
lib_dir    <- "/exports/eddie/scratch/s1917169/libs"

if (run_mode == "remote") {
  setwd(remote_loc)
  .libPaths(c(lib_dir, .libPaths()))
}

# other locs ----
sim_out  <- "../sim-pack-full-model/outputs/"
interm   <- "intermediate-data/"
data_dir <- "../data/"

# Ensure the intermediate folders exist (they hold the non-shared middle stages)
dir.create(file.path(interm, "links"), recursive = TRUE, showWarnings = FALSE)
