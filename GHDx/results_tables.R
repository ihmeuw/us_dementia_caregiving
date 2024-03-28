### Author: Michael Breshock
### Date: 01/24/2024
### Project: US dementia informal care cost
### Purpose: Create Results Tables for GHDx Record

rm(list=ls()) 

library(data.table)
library(dplyr)
library("metR", lib.loc = "FILEPATH")
source("FILEPATH/get_population.R")
source("FILEPATH/get_location_metadata.R")

############################ RETROACTIVE ESTIMATES #############################

# load data
# RC file created in: US_annual_replacement_cost_of_dementia_informal_care_per_case.R
rc_draws = fread(paste0(FILEPATH,"annual_cost_of_care_draws_top_code_112.csv"))
# OC file created in: US_annual_opportunity_cost_of_dementia_informal_care_per_case.R
oc_draws = fread(paste0(FILEPATH,"annual_cost_of_care_draws_top_code_40.csv"))
# demenita prevalence and total cases 
dem_cases = fread(paste0(FILEPATH,"rate_and_total_cases_1990_2019.csv"))
# get US population: 
US_pop <- get_population(release_id = 6, location_id = c(102, 523:573),
                         year_id = c(2010:2019), sex_id = 3, age_group_id = 22)

# process replacement cost per case draws: 
rc = rc_draws[,.(model_output = "Replacement cost per case",
                 val = mean(cost_attr), 
                 upper = quantile(cost_attr, 0.975),
                 lower = quantile(cost_attr, 0.025)), 
              by = c("year_id", "location_id", "location_name")]
# process forgone wage cost per case draws:
oc = oc_draws[,.(model_output = "Forgone wage cost per case",
                 val = mean(cost_attr), 
                 upper = quantile(cost_attr, 0.975),
                 lower = quantile(cost_attr, 0.025)), 
              by = c("year_id", "location_id", "location_name")]

# rbind RC and OC together: 
current_cost = rbind(rc, oc)

# merge in location abbreviation names
hierarchy <- get_location_metadata(location_set_id = 35, release_id = 6)
short_names = hierarchy[parent_id==102 | location_id==102, .(location_id, local_id)]
short_names[, location_short_name := case_when(
  location_id != 102 ~ substr(local_id,4,5),
  TRUE ~ local_id)]
current_cost = merge(current_cost, 
                     short_names[,.(location_id, location_short_name)],
                     by = "location_id")

# merge in population: 
current_cost = merge(current_cost, US_pop[,.(location_id, year_id, population)],
                     by = c("location_id", "year_id"))

# merge in dementia prevalent cases: 
current_cost = merge(current_cost, dem_cases[,.(location_id, year_id, cases)],
                     by = c("location_id", "year_id"))

# change dementia cases variable name: 
setnames(current_cost, old = "cases", new = "dementia_cases")

# change column order: 
setcolorder(current_cost, c("year_id","location_id","location_name", 
                            "location_short_name","population","dementia_cases", 
                            "model_output", "val", "upper", "lower"))

# save out 2010-2019 cost estimates: 
fwrite(current_cost, 
       file = paste0(FILEPATH,"IHME_DEMENTIA_US_INFORMAL_COST_2010_2019_DATA_Y2024M01D24.csv"))

############################ FORECAST ESTIMATES #############################

# load in low growth cost forecast draws
low_growth = fread(file = paste0(FILEPATH,"low_growth_cost_draws.csv"))
# load in high growth cost forecast draws
high_growth = fread(file = paste0(FILEPATH,"high_growth_cost_draws.csv"))

# save 2019 values for low growth per case estimates
low2019 = low_growth[year_id == 2019]
high2019 = high_growth[year_id == 2019]

# remove 2019 estimates from forecasts
low_growth = low_growth[year_id != 2019]
high_growth = high_growth[year_id != 2019]

# process low growth population/cases draws:
low_pop = low_growth[,.(population = mean(population),
                        dementia_cases = mean(cases)),
                        by = c("year_id", "location_id")]

# process low growth per case cost draws: 
low_cost = low2019[,.(rc = mean(rc),
                      rc_upper = quantile(rc, 0.975),
                      rc_lower = quantile(rc, 0.025),
                      oc = mean(oc),
                      oc_upper = quantile(oc, 0.975),
                      oc_lower = quantile(oc, 0.025)),
                   by = c("location_id")]

## merge in low growth costs per case with population and dementia cases:
# need to do it this way since the cost per case draws in the low growth scenario
# were kept static, with all draws = mean(RC2019) value or mean(OC2019) value.
# So, if we took the mean, lower, and upper of the draws for year >= 2030,
# they would all be the same value

low_sum = merge(low_pop, low_cost, by = "location_id")

# melt to long format: 
low_long = melt(low_sum, 
                id.vars = c("year_id", "location_id", 
                            "population", "dementia_cases"), 
                measure = list(val = c("rc", # creating three value columns
                                       "oc"),
                               upper = c("rc_upper", 
                                         "oc_upper"),
                               lower = c("rc_lower",
                                         "oc_lower")), 
                variable.name = "model_output")

# recode model_output: 
low_long[, model_output := case_when(model_output == 1 ~ "Replacement cost per case (low growth)", 
                                               TRUE ~ "Forgone wage cost per case (low growth)")]

# process high growth draws:
high_sum = high_growth[,.(population = mean(population),
                          dementia_cases = mean(cases),
                          rc = mean(rc),
                          rc_upper = quantile(rc, 0.975),
                          rc_lower = quantile(rc, 0.025),
                          oc = mean(oc),
                          oc_upper = quantile(oc, 0.975),
                          oc_lower = quantile(oc, 0.025)),
                       by = c("year_id", "location_id")]

# melt to long format:
high_long = melt(high_sum, 
                 id.vars = c("year_id", "location_id", 
                             "population", "dementia_cases"), 
                 measure = list(val = c("rc", # creating three value columns
                                        "oc"),
                                upper = c("rc_upper", 
                                          "oc_upper"),
                                lower = c("rc_lower",
                                          "oc_lower")), 
                 variable.name = "model_output")

# recode model_output:
high_long[, model_output := case_when(model_output == 1 ~ "Replacement cost per case (high growth)", 
                                      TRUE ~ "Forgone wage cost per case (high growth)")]

# rbind low and high growth together:
forecast_cost = rbind(low_long, high_long)

# add location names: 
forecast_cost[,":=" (location_name = "United States of America", 
                     location_short_name = "USA")]

# change column order:
setcolorder(forecast_cost, c("year_id","location_id","location_name", 
                             "location_short_name","population","dementia_cases", 
                             "model_output", "val", "upper", "lower"))

# order by location first, then year:
forecast_cost = forecast_cost[order(location_id, year_id)]

# save out forecast cost estimates:
fwrite(forecast_cost, 
       file = paste0(FILEPATH,"IHME_DEMENTIA_US_INFORMAL_COST_2030_2050_DATA_Y2024M01D24.csv"))


