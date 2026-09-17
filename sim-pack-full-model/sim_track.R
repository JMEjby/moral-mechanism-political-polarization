source("sim_setup.R")

sims <- read.table("gen_sim_params.csv", sep = ",", header = F)

if ( nrow(sims) > 1 & ncol(sims) > 1 ) { 
  current <- sims[1,]
  
  remainder <- sims[-1,]
  
  write.table(current, "curr_gen_sim_params.csv", sep=",", row.names = F, col.names = F)
  write.table(remainder, "gen_sim_params.csv", sep=",", row.names = F, col.names = F)
} else if (nrow(sims) == 1 & ncol(sims) > 1) {
  current <- sims[1,]
  
  remainder <- "stop"
  
  write.table(current, "curr_gen_sim_params.csv", sep=",", row.names = F, col.names = F)
  write.table(remainder, "gen_sim_params.csv", sep=",", row.names = F, col.names = F)
} else {
  file.remove("gen_sim_params.csv")
} 
