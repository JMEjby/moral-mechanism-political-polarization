source("sim_setup.R")

library(extraDistr)
options(scipen = 999)

# define sim properties ----
n_sims <- 300
n_ticks <- 201 

# define distributions ----
world <- c(20,2.5)
density <- c(70,5)
conflict <- c(0,3)
prevalence <-  c(50,10)
proportion <- c(75,5)
invariance_1 <- c(65,10) 
invariance_2 <- c(85,100)
tolerance <- c(30,2)
offset <- c(0, 2)
homophily <- c(0.2, 0.4, 0.4)

# draw param values ----
set.seed(1917169) # seed for reported sims
w <- round(rnorm(n_sims,world[1], world[2]), 0)
d <- rnorm(n_sims, density[1], density[2])
cr <- rdunif(n_sims,conflict[1],conflict[2])
pre <- rnorm(n_sims, prevalence[1], prevalence[2])
pro <- rnorm(n_sims, proportion[1], proportion[2])
inv_1 <- rnorm(n_sims/2,invariance_1[1],invariance_1[2])
inv_2 <- rdunif(n_sims/2,invariance_2[1],invariance_2[2])
t <- rnorm(n_sims, tolerance[1], tolerance[2])
o <- rdunif(n_sims,offset[1],offset[2])
h <- rcat(n_sims,homophily)+2

inv <- c(inv_1, inv_2) 

param_df <- as.data.frame(
  cbind(sim = 1:n_sims,
        w,d,cr,
        t,o,h,
        pre,pro,
        inv,
        n_ticks
  )
) |> round(5)

write.table(param_df, "gen_sim_params.csv", sep=",", row.names = F, col.names = F)
write.table(param_df, "gen_sim_params_fixed.csv", sep=",", row.names = F)