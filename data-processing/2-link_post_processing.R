source("processing_setup.R")

library(data.table)
library(stringr) 

extract_numbers <- function(text) {
  as.numeric(unlist(regmatches(text, gregexpr("[0-9]+", text))))
}

# file locations ----
txt_loc <- str_c(sim_out, "raw_txt/")
csv_loc <- interm

# extract .txt files except 0s
raw_txt <- list.files(path = txt_loc, pattern = "^Sample\\d+_tick\\d+\\.txt$")
tick_zeros <- list.files(path = txt_loc, pattern = "^Sample\\d+_tick0.txt$")
proc_txt <- list.files(path = str_c(csv_loc, "links/"),
                       pattern = "^Sample\\d+_tick\\d+\\_LINKS.csv$") 

for (t in 1:length(proc_txt)){
  proc_txt[t] <- str_replace(proc_txt[t], fixed("_LINKS.csv"), ".txt")
}

raw_txt <- setdiff(raw_txt, tick_zeros)
raw_txt <- setdiff(raw_txt, proc_txt)

rm(tick_zeros, proc_txt)

l_cols <- c("sim", "end1", "end2" , "breed", "ticks_distant", "age" , "tick")

# clean links data ----
for(f in raw_txt) {
  # for debugging
  print(str_c(f, "started"))
  
  # read the .txt
  data <- read.delim2(str_c(txt_loc,f), colClasses = "character", nrows = -1)
  data <- setDT(data)
  data <- na.omit(data)
  colnames(data) <- "c1"
  
  # sim info
  f_tick <- as.numeric(str_match(f, "tick\\s*(\\d+)")[, 2])
  f_sample <- as.numeric(str_match(f, "Sample\\s*(\\d+)")[, 2])
  
  # find link rows
  links_start <- cbind(row(data), data)[c1 == "LINKS"]$V1
  links_end <- cbind(row(data), data)[c1 == "PLOTS"]$V1
  
  header_row <- data[links_start + 1,]$c1
  col_names <- unlist(strsplit(header_row, split=","))
  
  # links
  data <- data[(links_start +2): (links_end-1),]$c1
  
  links_data <- data.table(do.call(rbind, strsplit(data, split=",")))
  rm(data)
  
  if (ncol(links_data) < length(col_names)) {
    links_data <- links_data[, "ticks-distant" := NA] 
  }
  
  setnames(links_data, col_names)
  setnames(links_data, old = colnames(links_data), new = gsub("\\-", "_", colnames(links_data)))
  
  links_data <- links_data[,c("sim","tick") := .(f_sample, f_tick)]
  links_data <- links_data [, subset(.SD, 
                                   select = l_cols)] [, c("end1", 
                                                          "end2",
                                                          "breed",
                                                          "age",
                                                          "ticks_distant",
                                                          "u_tick") := .(extract_numbers(end1), 
                                                                         extract_numbers(end2), 
                                                                         str_extract(breed, "(ingroup|cluster)"),
                                                                         as.numeric(age),
                                                                         as.numeric(ticks_distant),
                                                                         interaction(sim, tick, sep = "_")
                                                          )] 
  
  setorder(links_data, sim, tick, breed, end1, end2)
  
  # save links data
  lloc <- str_c(csv_loc, "links/", str_replace(f, fixed(".txt"), "_LINKS.csv"))
  fwrite(links_data, lloc, sep = ",")
  
  # clean up
  rm(links_start, links_end, f_tick, f_sample, links_data, header_row, col_names, lloc)
  gc()
  
  print(str_c(f, "finished"))
}
