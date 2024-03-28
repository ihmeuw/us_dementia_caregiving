# Number plugging - discussion section
# Author: Amy Lastuka

rm(list = ls())

# load in libraries
library(dplyr)
library(data.table)


source("FILEPATH/helper_functions.R")

# load data
# RC file created in: US_annual_replacement_cost_of_dementia_informal_care_per_case.R
rc_draws = fread(paste0(FILEPATH,"annual_cost_of_care_draws_top_code_112.csv"))
# OC file created in: US_annual_opportunity_cost_of_dementia_informal_care_per_case.R
oc_draws = fread(paste0(FILEPATH,"annual_cost_of_care_draws_top_code_40.csv"))

# summarize RC and OC draws: 
rc = rc_draws[,.(cost_mean = mean(cost_attr),
                 cost_lower = quantile(cost_attr, 0.025), 
                 cost_upper = quantile(cost_attr, 0.975)),
              by = c("year_id", "location_id", "location_name")]

oc = oc_draws[,.(cost_mean = mean(cost_attr),
                 cost_lower = quantile(cost_attr, 0.025), 
                 cost_upper = quantile(cost_attr, 0.975)),
              by = c("year_id", "location_id", "location_name")]

# # # # # # Paragraph 4 # # # # # #

# Sentence 1: Our estimates of the cost per prevalent case for 2019 were $XX (XX–XX) 
# for replacement cost and $XX (XX–XX) for forgone wages

# the national estimates for indirect cost per patient in 2019 was 
# $XX (XX-XX) using replacement cost and $XX (XX-XX) for forgone wages
US_RC2019 = rc[location_id == 102 & year_id == 2019]
US_OC2019 = oc[location_id == 102 & year_id == 2019]

paste0("Our estimates of the cost per prevalent case for 2019 were $",
       round(US_RC2019$cost_mean), " (", 
       round(US_RC2019$cost_lower), "-", 
       round(US_RC2019$cost_upper), ") for replacement cost and $", 
       round(US_OC2019$cost_mean), " (", 
       round(US_OC2019$cost_lower), "-", 
       round(US_OC2019$cost_upper), ") for forgone wages.")

# Sentence 2: When we scaled our estimates to reflect only ADL and IADL support, 
# they were $XX (XX–XX) for replacement cost and $XX (XX–XX) for forgone wage cost

# ADL/IADL/Supervision Time Fractions 
care_time_fractions = fread(paste0(FILEPATH,"care_time_fractions.csv"))
# create low, medium, high estimates based on activity fractions
# fractions from GERAS data:
low_frac = care_time_fractions[care_type == "ADL"]$percents # GERAS ADL fraction
med_frac = care_time_fractions[care_type == "ADL"]$percents + 
  care_time_fractions[care_type == "IADL"]$percents # GERAS ADL + IADL

# US RC 2019
RC_2019 = rc[year_id == 2019 & location_id == 102]
RC_2019[, type := "Replacement Cost"] # adding group variable

# US OC 2019
OC_2019 = oc[year_id == 2019 & location_id == 102]
OC_2019[, type := "Forgone Wage"] # adding group variable

# bind data together 

RC_OC = rbind(RC_2019, OC_2019)
RC_OC[, ":=" (
  # ADL only estimates: 
  ADL_only_lower = cost_lower*low_frac, 
  ADL_only_mean = cost_mean*low_frac, 
  ADL_only_upper = cost_upper*low_frac,
  # per case medium estimates + bounds + with and without AF
  ADL_IADL_lower = cost_lower*med_frac, 
  ADL_IADL_mean = cost_mean*med_frac, 
  ADL_IADL_upper = cost_upper*med_frac,
  # per case high estimates + bounds + with and without A
  All_care_lower = cost_lower, 
  All_care_mean = cost_mean, 
  All_care_upper = cost_upper)]


rc_fracs = RC_OC[type == "Replacement Cost"]
oc_fracs = RC_OC[type == "Forgone Wage"]

# Medium: ADL+IADL support
paste0("When we scaled our estimates to reflect only ADL and IADL support, they were $",
       round(rc_fracs$ADL_IADL_mean), " (", 
       round(rc_fracs$ADL_IADL_lower), "-", 
       round(rc_fracs$ADL_IADL_upper), ") for replacement cost and $",
       round(oc_fracs$ADL_IADL_mean), " (", 
       round(oc_fracs$ADL_IADL_lower), "-", 
       round(oc_fracs$ADL_IADL_upper)," for forgone wage cost")


# # # # # # Paragraph 5 # # # # # #

# Sentence 1: "The most expensive state (XX) had a replacement cost 
# that was XX times higher than the least expensive state (XX).

min_cost = min(rc[year_id == 2019]$cost_mean)
min_state = rc[year_id == 2019 & cost_mean == min_cost]$location_name
max_cost = max(rc[year_id == 2019]$cost_mean)
max_state = rc[year_id == 2019 & cost_mean == max_cost]$location_name

paste0("The most expensive state (", max_state,") had a replacement cost that was ",
       round_num((max_cost/min_cost), type = "decimal"),
       " times higher than the least expensive state (", min_state,").")


# Sentence 2: Using forgone wage approach led to similar variation in cost by state,
# with XX (the most expensive state) being XX times more expensive than XX. 

min_cost = min(oc[year_id == 2019]$cost_mean)
min_state = oc[year_id == 2019 & cost_mean == min_cost]$location_name
max_cost = max(oc[year_id == 2019]$cost_mean)
max_state = oc[year_id == 2019 & cost_mean == max_cost]$location_name


paste0("Using forgone wage approach led to similar variation in cost by state with (",
       max_state,") being ",round_num((max_cost/min_cost), type = "decimal"),
       " times more expensive than (", min_state,").")


# Sentence 3: with our replacement cost model, which allows for up to 16 hours a day of care,
#    the estimated hours of care per case in 2019 were XX (XX-XX) for Washington D.C. and XX (XX-XX) for Kentucky

rc_hours = rc_draws[, .(hours_mean = mean(dem_hours), 
                        hours_lower = quantile(dem_hours, 0.025),
                        hours_upper = quantile(dem_hours, 0.975)), 
                    by = c("year_id", "location_id", "location_name")]

# DC = location_id 531; Kentucky = 540
dc_hours = rc_hours[(year_id == 2019) & (location_id==531)]$hours_mean
dc_min = rc_hours[(year_id == 2019) & (location_id==531)]$hours_lower
dc_max = rc_hours[(year_id == 2019) & (location_id==531)]$hours_upper

ky_hours = rc_hours[(year_id == 2019) & (location_id==540)]$hours_mean
ky_min = rc_hours[(year_id == 2019) & (location_id==540)]$hours_lower
ky_max = rc_hours[(year_id == 2019) & (location_id==540)]$hours_upper

paste0("the estimated hours of care per case in 2019 were ",
       round_num(dc_hours, type = "decimal",lancet = F)," (",
       round_num(dc_min, type = "decimal",lancet = F),"-",
       round_num(dc_max, type = "decimal",lancet = F),") for Washington D.C. and ",
       round_num(ky_hours, type = "decimal",lancet = F)," (",
       round_num(ky_min, type = "decimal",lancet = F),"-",
       round_num(ky_max, type = "decimal",lancet = F),") for Kentucky")


# sentence 4 - want a few high-income states with low RC costs
temp <- rc[year==2019,]
setorder(temp, cols = "care_cost_mean_AF")  
head(temp)

# lowest RC costs are DC, Connecticut, Maryland, Massachusetts, Florida, and New Jersey