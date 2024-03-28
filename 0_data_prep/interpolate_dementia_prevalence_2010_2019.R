## Interpolate 2010-2019 Dementia Prevalence Envelope from MEID 24351 - Unadjusted dementia (post-mortality)
## 02/17/2023
## Author: Michael Breshock

## Purpose: Get dementia prevalence estimates from DisMod outputs - pre final GBD squeezing
# We need to get the estimates from the Dismod outputs instead of the final estimates from 
# get_outputs because the final dementia estimates in get_outputs are filtered to be
# only people with just dementia, no other comorbidities 
# For our project, we want all people with dementia, regardless if they have other 
# pathologies or not, so we need to pull from the dementia envelope rather than the final output. 
# IHME does not estimate the envelope numbers for every year. get_draws only gives 
# the years 2010, 2015, 2017, and 2019, so we need to interpolate for the missing years in between.

rm(list = ls())

# load in libraries
library(dplyr)
library(data.table)
source("/ihme/cc_resources/libraries/current/r/interpolate.R")
source("/ihme/cc_resources/libraries/current/r/get_population.R")

# specify directory roots
if (Sys.info()["sysname"] == "Linux") {
  j_root <- "/home/j/" 
  h_root <- "~/"
  l_root <- "/ihme/limited_use/"
  functions_dir <- "/ihme/cc_resources/libraries/current/r/"
} else { 
  j_root <- "J:/"
  h_root <- "H:/"
  l_root <- "L:/"
  functions_dir <- "K:/libraries/current/r/"
}

######################### INTERPOLATE DEMENTIA DRAWS ##########################
# commenting this section out because interpolate takes a long time to run
# this output has been saved to a .csv file so it can be loaded from there
# only run this if the interpolate outputs need to be updated,
# otherwise just load from the .csv file (see below)

# get_draws has data for 2010, 2015, 2017, 2019, using interpolate function to 
# fill missing years of data inbetween
# dementia prevalence interpolated: 
# dem_prev_int <- interpolate(gbd_id_type = "modelable_entity_id", # MEID 24351
#                             gbd_id = 24351, # Dementia prevalence from DisMod, post-mortality modeling
#                             location_id = c(102, 523:573), # US + states
#                             measure_id = 5, # prevalence
#                             source = "epi",
#                             sex_id = 3, # all sex
#                             reporting_year_start = 1990,
#                             reporting_year_end = 2019,
#                             interp_method = "linear",
#                             downsample = TRUE, # get 100 draws instead of 1000
#                             age_group_id = 22, # age_group_id = 22: all ages
#                             num_workers = 50,
#                             release_id = 6) # GBD 2019

# note: central comp said the function is running into issues with aggregating
# but using the most detailed sex_ids and age_group_ids will work
# - just need to aggregate ourselves

# save out raw interpolated results
# fwrite(dem_prev_int, file = paste0(j_root,"Project/IRH/Informal_Care_AD/US_dementia_spending_2022/output/dementia_prevalence/dementia_prevalence_interpolate_1990_2019.csv"))

# load interpolate outputs from csv file: 
dem_prev_int = fread(file = paste0(j_root,"Project/IRH/Informal_Care_AD/US_dementia_spending_2022/output/dementia_prevalence/dementia_prevalence_interpolate_1990_2019.csv"))

# select just the columns we need: 
draw_cols = paste0("draw_", c(0:999))
select_cols = c("year_id", "location_id", "age_group_id", "sex_id", draw_cols)
# pivot longer to have on draw per row: 
dem_draws = melt(dem_prev_int[,..select_cols],
                 id.vars = c("year_id", "location_id", 
                             "age_group_id", "sex_id"), 
                 variable.name = "draw", value.name = "prevalence")
# get populations by age and sex group for total dementia cases
US_pop <- get_population(release_id = 6, location_id = c(102, 523:573),
                         year_id = c(1990:2019), sex_id = 3,
                         age_group_id = 22)
# remove unneccesary run_id column
US_pop[, run_id := NULL]

# merge population numbers with prevalence draws: 
dem_cases = merge(dem_draws, US_pop, 
                  by = c("year_id", "location_id", "age_group_id", "sex_id"))
# calculate total cases
dem_cases[, cases := prevalence*population]

fwrite(dem_cases, file = paste0(j_root,"Project/IRH/Informal_Care_AD/US_dementia_spending_2022/output/dementia_prevalence/total_draws_1990_2019.csv"))


################## get mean and confidence interval from draws #################
# remove empty version_id and model_version_id columns
dem_prev_int[, ":=" (version_id = NULL,
                     model_version_id = NULL)]

# melt draws to long format
draws_melt = melt(dem_prev_int, id.vars = c("year_id", "location_id", "measure_id",
                                        "metric_id", "sex_id", "age_group_id", 
                                        "modelable_entity_id"))
# summarise mean, upper and lower bounds, and variance
dementia_prevalence = draws_melt[, .(prevalence = mean(value),
                               upper_prevalence = quantile(value, 0.975),
                               lower_prevalence = quantile(value, 0.025),
                               prevalence_variance = var(value)),
                           by = c("year_id", "location_id")]

# get populations by age and sex group for total dementia cases
US_pop <- get_population(release_id = 6, location_id = c(102, 523:573),
                         year_id = c(1990:2019), sex_id = 3,
                         age_group_id = 22)
# remove unneccesary run_id column
US_pop[, run_id := NULL]

dementia_cases = merge(dementia_prevalence, US_pop, 
                       by = c("year_id", "location_id"))

# calculate cases: prevalence*population 
dementia_cases[, ":=" (
  cases = prevalence*population,
  upper_cases = upper_prevalence*population,
  lower_cases = lower_prevalence*population
)]

# save out data set
fwrite(dementia_cases, file = paste0(j_root,"Project/IRH/Informal_Care_AD/US_dementia_spending_2022/output/dementia_prevalence/rate_and_total_cases_1990_2019.csv"))


######### aggregate interpolated results for all sexes and age groups #########

# dont need to do this anymore now that interpolate function works for 
# age_group_id = 22 and sex_id = 3
# leaving this section commented out for documentation purposes

# filter to just the age groups we want
# age_group_id 5-20 -> age 1-79
# age_group_id 30-32 -> age 80-94
# age_group_id 235 -> age 95 plus
# dem_prev_age = dem_prev_int[age_group_id %in% c(5:20, 30:32, 235)]
# 
# 
# # merge population data into dementia draws
# dem_prev_pop = merge(dem_prev_age, US_pop[,-"run_id"], # removing run_id column from get_population output
#                      by = c("year_id", "location_id", "age_group_id", "sex_id")) 
# 
# # aggregate age and sex groups by taking population weighted.mean of all prevalence draws
# dem_prev = data.table(
#   dem_prev_pop %>% group_by(year_id, location_id, measure_id, metric_id, # everything but location and year are the same
#                             model_version_id, modelable_entity_id) %>% # including the other groups to keep the variables in the dataset
#     summarise(across(draw_0:draw_999, ~ weighted.mean(.x, w = population)),
#               pop = sum(population))
# )
# 
# # add sex_id and age_group_id to keep consistent with GBD outputs
# dem_prev[,":=" (
#   sex_id = 3,
#   age_group_id = 22
# )]


