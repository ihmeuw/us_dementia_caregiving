# Script to aggregate ST-GPR draws for US care hours (112 topcode)
# Author: Michael Breshock
# Date: 10/19/2023

# clear environment
rm(list = ls())

# load libraries
library(dplyr)
library(data.table)
library(tidyr)


# model index number used for st_gpr run configuration - dont forget to update!
model_num = 4
# initialize if this is a run with or without outliers
keep_outliers = F
outlier = ifelse(keep_outliers, "_keep_outliers", "")

# stgpr output path
stgpr_out_path = "FILEPATH"

draws = fread(paste0(stgpr_out_path, "stgpr_cg_draws_run_00",model_num,outlier,".csv"))

################# AGGREGATE US NATIONAL ESTIMATES FROM STATES ##################
# filter to just US states
states = draws[location_id %in% c(523:573)] # 523-573 US states
# exponentiate values out of log space
states[, value := exp(value)]
# change names for clarity/merging
setnames(states, old = c("variable", "value"), new = c("draw", "care_hours"))

# get number of dementia cases by state for population weighting
dem_cases = fread(file = paste0(FILEPATH,"total_draws_1990_2019.csv"))

states_cases = merge(states, dem_cases, by = c("year_id", "location_id", "draw"))

# get national means from state data in draw space:
national = states_cases[,.(care_hours = weighted.mean(care_hours, w = cases)),
                        by = c("year_id", "draw")]
# add location ID = 102 for US national estimates:
national[, location_id := 102]

# combine national estimates with state estimates
final_estimates = rbind(states, national)

# save out aggregated US draws
fwrite(final_estimates,paste0(FILEPATH,model_num, outlier,"_draws.csv"))
