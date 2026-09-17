source("processing_setup.R")

library(data.table)
library(stringr)

extract_numbers <- function(text) {
  as.numeric(unlist(regmatches(text, gregexpr("[0-9]+", text))))
}

# file locations ----
csv_loc <- interm

link_data <- setDT(fread(str_c(csv_loc, "Link_data_raw.csv")))

# organise and clean
setnames(link_data, old = colnames(link_data), new = gsub("\\.", "_", colnames(link_data)))

l_cols <- c("sim", "end1", "end2" , "breed", "ticks_distant", "age" , "tick")

link_data <- link_data [, subset(.SD, 
                                 select = l_cols)] [, c("end1", 
                                                        "end2",
                                                        "breed",
                                                        "u_tick") := .(extract_numbers(end1), 
                                                                       extract_numbers(end2), 
                                                                       str_extract(breed, "(ingroup|cluster)"),
                                                                       interaction(sim, tick, sep = "_")
                                                        )] 

setorder(link_data, sim, tick, breed, end1, end2)
save(link_data, file = str_c(csv_loc, "Links_clean.RData")) 
