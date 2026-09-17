source("processing_setup.R")

library(data.table)
library(stringr)
library(igraph)

# file location
csv_loc <- interm

link_data <- setDT(fread(str_c(csv_loc, "Link_data_raw.csv")))

load(str_c(csv_loc, "Links_clean.RData"))
load(str_c(csv_loc, "Turtle_data.RData"))

cluster_links <- link_data[breed == "cluster",subset(.SD,select =c("end1","end2", "u_tick"))]
full_turtle_cluster_map <- data.table()

for (t in levels(factor(link_data$u_tick))) {
  t_clusters <- cluster_links[u_tick == t,]
  
  cluster_net <- graph_from_data_frame(t_clusters, directed = F, vertices = NULL)
  clusters <- cluster_louvain(cluster_net, resolution = 0)
  
  turtle_cluster_map <- data.table(cbind(clusters$names, 
                                         clusters$membership)) [, c("who",
                                                                    "cluster",
                                                                    "V1", 
                                                                    "V2") := 
                                                                  .(as.numeric(V1),
                                                                    str_c("cluster_", V2),
                                                                    NULL,
                                                                    NULL)] [, count := .N, by = cluster
                                                                    ] [count > 4,] [, c("u_tick", 
                                                                                        "count") := .(t,
                                                                                                      NULL)]
  
  no_cluster_turtles <- setdiff(turtle_data[u_tick == t]$who, turtle_cluster_map$who)
  turtle_cluster_map <- rbind(turtle_cluster_map,
                              data.table(who = no_cluster_turtles,
                                         cluster = rep("no_cluster", length(no_cluster_turtles)),
                                         u_tick = t))
  
  full_turtle_cluster_map <- data.table(rbind(full_turtle_cluster_map, 
                                              turtle_cluster_map))   
}

turtle_cluster_data <- turtle_data[full_turtle_cluster_map, on = .(u_tick, who)]  

setorder(turtle_cluster_data, sample, tick, who)
save(turtle_cluster_data, file = str_c(csv_loc, "Turtle_cluster.RData")) 

