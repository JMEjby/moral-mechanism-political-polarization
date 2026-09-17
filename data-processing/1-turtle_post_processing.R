source("processing_setup.R") # pulls data locations

library(data.table)
library(stringr)

turtle_data <- setDT(fread(str_c(sim_out, "combined_data/Turtle_data_raw.csv")))

turtle_data <- turtle_data[, c("identity",
                               "decision",
                               "action_taken",
                               "conflicted",
                               "unhappy",
                               "past_choices_1",
                               "past_choices_2",
                               "past_choices_3",
                               "past_choices_4",
                               "past_choices_5",
                               "conflict_range",
                               "homophily",
                               "inv_level",
                               "hit_rate",
                               "false_alarm") := .(factor(identity), 
                                                   factor(decision), 
                                                   factor(action_taken),
                                                   factor(conflicted),
                                                   factor(unhappy),
                                                   factor(past_choices_1),
                                                   factor(past_choices_2),
                                                   factor(past_choices_3),
                                                   factor(past_choices_4),
                                                   factor(past_choices_5),
                                                   factor(conflict_range),
                                                   factor(homophily),
                                                   fifelse(sim > 150, "Moderate",
                                                           "High"),
                                                   fifelse(perceived_ingroup_n > 0, 
                                                           (ingroup_accuracy)/total_neighbours,
                                                           0),
                                                   fifelse(perceived_ingroup_n > 0, 
                                                           (perceived_ingroup_n-ingroup_accuracy)/total_neighbours,
                                                           0))] 

turtle_data <- turtle_data[, d_prime_ingroup := scale(hit_rate)-scale(false_alarm), by = .(sample, who)]

save(turtle_data, file = str_c(interm, "Turtle_data.RData"))