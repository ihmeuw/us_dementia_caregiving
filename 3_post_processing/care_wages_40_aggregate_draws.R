# Script to aggregate ST-GPR draws for US care wages (40 topcode)
# Author: Michael Breshock
# Date: 10/19/2023

# clear environment
rm(list = ls())

# load libraries
library(dplyr)
library(data.table)
library(tidyr)


source("FILEPATH/get_population.R")

# model index number used for st_gpr run configuration - dont forget to update!
model_num = 12
# initialize if this is a run with or without outliers
keep_outliers = FALSE
outlier = ifelse(keep_outliers, "_keep_outliers", "")

# stgpr output path
stgpr_out_path = "FILEPATH"

draws = fread(paste0(stgpr_out_path, "stgpr_cost_run_00",model_num,outlier,".csv"))

################# AGGREGATE US NATIONAL ESTIMATES FROM STATES ##################
# filter to just US states
states = draws[location_id %in% c(523:573)] # 523-573 US states
# exponentiate values out of log space
states[, value := exp(value)]
# change names for clarity/merging
setnames(states, old = c("variable", "value"), new = c("draw", "wage"))

# get US population for weights
US_pop <- get_population(release_id = 6, location_id = c(523:573), 
                         year_id = c(2010:2019))
US_pop[,run_id := NULL] # remove unnecessary column

states_pop = merge(states, US_pop, by = c("year_id", "location_id"))

# get national means from state data in draw space:
national = states_pop[,.(wage = weighted.mean(wage, w = population)),
                        by = c("year_id", "draw")]
# add location ID = 102 for US national estimates:
national[, location_id := 102]

# combine national estimates with state estimates
final_estimates = rbind(states, national)

# save out aggregated US draws
fwrite(final_estimates,paste0(stgpr_out_path, "aggregated/US_wages_run_00", 
                              model_num, outlier, "_draws.csv"))
