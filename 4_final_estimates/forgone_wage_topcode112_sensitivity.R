## Run Forgone Wage model with topcode 112 hours/week -> sensitivity analysis
## Date: 10/24/2023
## Author: Michael Breshock 

# clear environment
rm(list = ls())

# load libraries 
library(ggplot2)
library(dplyr)
library(data.table)
library(tidyr)
library(usmap)
source("FILEPATH/get_location_metadata.R")


# load data:
# initialize the care hours top-code
top_code = 112
# initialize the Care Hours ST-GPR model number you want to make estimates from
hours_model_num = "04"
# initialize the Care Wages ST-GPR model number you want to make estimates from
wages_model_num = 12 
# initialize if this is a run with or without outliers
keep_outliers = FALSE
outlier = ifelse(keep_outliers, "_keep_outliers", "")

# initialize file paths to data: 
# st_gpr hours estimates
care_hours_path = "FILEPATH"
# attributable fraction
AF_path = "FILEPATH"
# st_gpr wages estimates
care_wages_path = "FILEPATH"

# load data:
# stgpr hours draws
stgpr_hours = fread(paste0(care_hours_path,"US_caregiving_hours_run_0",
                           hours_model_num,outlier,"_draws.csv"))
# attributable fraction
AF = fread(paste0(AF_path, "attributable_fraction_draws.csv"))
# Wages from opportunity cost ST-GPR output
stgpr_wages = fread(paste0(care_wages_path, "US_wages_run_00",
                           wages_model_num,outlier,"_draws.csv"))

# merge in attributable fraction with hours: 
setnames(AF, old = "dementia", new = "dem_AF")
care_hours = merge(stgpr_hours, AF[,.(draw, dem_AF)], by = "draw")


# calculate care hours attributable to dementia
care_hours[, dem_hours := care_hours*dem_AF]

# get location meta data
loc_meta = get_location_metadata(location_set_id = 22, release_id = 6)
locs = loc_meta[, .(location_id, location_name)]
# merge in location names and parent ids with STGPR output
care_hours_named = merge(care_hours,locs, by = "location_id")
stgpr_wages_named = merge(stgpr_wages,locs, by = "location_id")

# merge wages and hours
US_cost = merge(care_hours_named, stgpr_wages_named, 
                by = c("year_id", "location_id", "draw", "location_name"))

# estimate annual cost (52 weeks/year * $$$ cost/week = $$$ cost/year)
US_cost[, ":=" (cost_total = care_hours*wage*52,
                cost_attr = dem_hours*wage*52)] 

# save out annual opportunity cost
out_path = "FILEPATH"
fwrite(US_cost, paste0(out_path, "annual_cost_of_care_draws_top_code_", 
                       top_code, outlier,"_sensitive.csv"))
