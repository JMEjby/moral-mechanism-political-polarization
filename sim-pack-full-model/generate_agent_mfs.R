source("sim_setup.R")

library(data.table)

load("mf_generation_data.RData")
sim <- read.table("curr_gen_sim_params.csv", sep = ",", header = F)[,1:3]

# data sets
lib_data <- mf_data[bin_pol == "liberal",]
con_data <- mf_data[bin_pol == "conservative",]

# sample data
n_agents <- round((sim$V2*2+1)^2*(sim$V3)/100/2*2)

rows <- sample(1:nrow(lib_data), n_agents)
sample_l <- lib_data[rows,2:6]
sample_l$sim <- rep(sim$V1, n_agents)

rows <- sample(1:nrow(con_data), n_agents)
sample_c <- con_data[rows,2:6]
sample_c$sim <- rep(sim$V1, n_agents)

# save
write.csv(sample_c, file = "cons_mfs.csv", row.names = F)
write.csv(sample_l, file = "lib_mfs.csv", row.names = F)