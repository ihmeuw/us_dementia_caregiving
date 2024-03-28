# Number plugging - results section 3.2
# Authors: Amy Lastuka & Michael Breshock

rm(list = ls())

# load in libraries
library(dplyr)
library(data.table)


source("FILEPATH/helper_functions.R")
source("FILEPATH/get_location_metadata.R")

# load in location set (51 US states)
locs_full <- get_location_metadata(location_set_id = 35, release_id = 6)
locs <- locs_full[(location_id==102) | (parent_id == 102), .(state = location_name, location_id)] 

# load data:
AF = fread(paste0(FILEPATH,"attributable_fraction_draws.csv"))
rc_draws = fread(paste0(FILEPATH,"annual_cost_of_care_draws_top_code_112.csv"))
oc_draws = fread(paste0(FILEPATH,"annual_cost_of_care_draws_top_code_40.csv"))

# Sentence 1: We found that XX% (XX-XX) of caregiving hours were attributable to dementia.

AF_sum = AF[,.(af_mean = mean(dementia),
               af_lower = quantile(dementia, 0.025), 
               af_upper = quantile(dementia, 0.975))]

AF_mean = round(AF_sum$af_mean*100)
AF_lower = round(AF_sum$af_lower*100)
AF_upper = round(AF_sum$af_upper*100)

paste0("We found that ", AF_mean, "% (", AF_lower, "-", AF_upper, ") of ",
       "caregiving hours were attributable to dementia.")

# Sentence 2: The national estimates for total indirect spending cost per patient 
# in 2019 were $XX (XX–XX) using replacement cost and $XX (XX–XX) for forgone wages.

# summarize RC and OC draws: 
rc = rc_draws[,.(cost_mean = mean(cost_attr),
                 cost_lower = quantile(cost_attr, 0.025), 
                 cost_upper = quantile(cost_attr, 0.975)),
              by = c("year_id", "location_id", "location_name")]

oc = oc_draws[,.(cost_mean = mean(cost_attr),
                 cost_lower = quantile(cost_attr, 0.025), 
                 cost_upper = quantile(cost_attr, 0.975)),
              by = c("year_id", "location_id", "location_name")]

US_RC2019 = rc[location_id == 102 & year_id == 2019]
US_OC2019 = oc[location_id == 102 & year_id == 2019]

rc_mean = round(US_RC2019$cost_mean)
rc_lower = round(US_RC2019$cost_lower)
rc_upper = round(US_RC2019$cost_upper)

oc_mean = round(US_OC2019$cost_mean)
oc_lower = round(US_OC2019$cost_lower)
oc_upper = round(US_OC2019$cost_upper)

paste0("The national estimates for total indirect spending cost per patient in 2019 were $",
       rc_mean, " (", rc_lower, "-", rc_upper, ") using replacement cost and $", 
       oc_mean, " (", oc_lower, "-", oc_upper, ") for forgone wages.")

# Sentence 3: The replacement cost per case ranged from $XX (XX-XX) in the [least expensive state] 
# to $XX (XX-XX) in [most expensive state].

rc_temp = rc[year_id == 2019]
setorder(rc_temp, cols = "cost_mean")
head(rc_temp)

least_rc_state = rc_temp[1]$location_name

least_rc_mean = round(rc_temp[location_name == least_rc_state]$cost_mean)
least_rc_lower = round(rc_temp[location_name == least_rc_state]$cost_lower)
least_rc_upper = round(rc_temp[location_name == least_rc_state]$cost_upper)

most_rc_state = rc_temp[nrow(rc_temp)]$location_name

most_rc_mean = round(rc_temp[location_name == most_rc_state]$cost_mean)
most_rc_lower = round(rc_temp[location_name == most_rc_state]$cost_lower)
most_rc_upper = round(rc_temp[location_name == most_rc_state]$cost_upper)

paste0("The replacement cost per case ranged from $", 
       least_rc_mean, " (", least_rc_lower, "-", least_rc_upper, ") in ", least_rc_state,
       " to $", most_rc_mean, " (", most_rc_lower, "-", most_rc_upper, ") in ", most_rc_state)

# Sentence 4: The forgone wage cost per case varied from $XX (XX-XX) in [least expensive state] 
# to $XX (XX-XX) in [most expensive state].

oc_temp = oc[year_id == 2019]
setorder(oc_temp, cols = "cost_mean")
head(oc_temp)

least_oc_state = oc_temp[1]$location_name

least_oc_mean = round(oc_temp[location_name == least_oc_state]$cost_mean)
least_oc_lower = round(oc_temp[location_name == least_oc_state]$cost_lower)
least_oc_upper = round(oc_temp[location_name == least_oc_state]$cost_upper)

most_oc_state = oc_temp[nrow(oc_temp)]$location_name

most_oc_mean = round(oc_temp[location_name == most_oc_state]$cost_mean)
most_oc_lower = round(oc_temp[location_name == most_oc_state]$cost_lower)
most_oc_upper = round(oc_temp[location_name == most_oc_state]$cost_upper)

paste0("The forgone wage cost per case varied from $", 
       least_oc_mean, " (", least_oc_lower, "-", least_oc_upper, ") in ", least_oc_state,
       " to $", most_oc_mean, " (", most_oc_lower, "-", most_oc_upper, ") in ", most_oc_state)


