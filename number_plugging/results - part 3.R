# Number plugging - results section 3.3
# Author: Michael Breshock

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

# Sentence 1: The replacement cost per prevalent case ranged from $XX (XX-XX) 
# for ADL support only to $XX (XX-XX) for all caregiving activities

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
oc_fracs = RC_OC[type == "Opportunity Cost"]
# print sentence: 
paste0(" The replacement cost per prevalent case ranged from ",
    round(rc_fracs$ADL_only_mean), " (", 
    round(rc_fracs$ADL_only_lower), "-", 
    round(rc_fracs$ADL_only_upper), ") for ADL support only to ",
    round(rc_fracs$All_care_mean), " (", 
    round(rc_fracs$All_care_lower), "-", 
    round(rc_fracs$All_care_upper), " for all caregiving activities.")
    
