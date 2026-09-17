run_mode <- Sys.getenv("RUN_MODE", unset = "local")

# Cluster (Eddie) paths ----
remote_loc <- "/exports/eddie/scratch/s1917169/moral-mechanism-political-polarization/sim-pack-full-model"
lib_dir     <- "/exports/eddie/scratch/s1917169/libs"

if (run_mode == "remote") {
  setwd(remote_loc)
  .libPaths(c(lib_dir, .libPaths()))
}
