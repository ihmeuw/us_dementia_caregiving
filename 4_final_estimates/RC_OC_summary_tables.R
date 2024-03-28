# GV Dementia Update March 2023 
# RC + OC Summary Table
# Author: Michael Breshock
# Date: 01/26/2023

# clear environment
rm(list = ls())

# load libraries
library(ggplot2)
library(dplyr)
library(data.table)
library(tidyr)
library(gt, lib.loc = "FILEPATH")


# annual replacement + opportunity cost of care 
RC = fread(file = paste0(FILEPATH,"aggregated_annual_cost_of_care.csv"))
OC = fread(file = paste0(FILEPATH,"aggregated_annual_cost_of_care.csv"))

# ADL/IADL/Supervision Time Fractions 
care_time_fractions = fread(paste0(FILEPATH,"care_time_fractions.csv"))

# demenita prevalence and total cases 
dem_cases = fread(file = paste0(FILEPATH,"rate_and_total_cases_1990_2019.csv"))

# Dementia subtypes
dx_subtypes = fread(file = paste0(FILEPATH,"dementia_subtype_proportions_022423.csv"))

# filtering to national US data for 2019 and final estimate columns
# US RC 2019
vars = c("year", "state",
         "care_cost_lower", "care_cost_lower_AF", "care_cost_mean",
         "care_cost_mean_AF", "care_cost_upper", "care_cost_upper_AF")

RC_2019 = RC[year == 2019 & state == "USA", .SD, .SDcols = vars]
RC_2019[, type := "Replacement Cost"] # adding group variable

# US OC 2019
OC_2019 = OC[year == 2019 & state == "USA", .SD, .SDcols = vars]
OC_2019[, type := "Opportunity Cost"] # adding group variable

# bind data together 

RC_OC = rbind(RC_2019, OC_2019)

# create low, medium, high estimates based on activity fractions
# fractions from GERAS data:
low_frac = care_time_fractions[care_type == "ADL"]$percents # GERAS ADL fraction
med_frac = care_time_fractions[care_type == "ADL"]$percents + 
  care_time_fractions[care_type == "IADL"]$percents # GERAS ADL + IADL

RC_OC[, ":=" (
  # per case lower estimates + bounds + with and without AF
  per_case_low_lower = care_cost_lower*low_frac,
  per_case_low_lower_AF = care_cost_lower_AF*low_frac, 
  per_case_low_mean = care_cost_mean*low_frac, 
  per_case_low_mean_AF = care_cost_mean_AF*low_frac,
  per_case_low_upper = care_cost_upper*low_frac, 
  per_case_low_upper_AF = care_cost_upper_AF*low_frac,
  # per case medium estimates + bounds + with and without AF
  per_case_med_lower = care_cost_lower*med_frac,
  per_case_med_lower_AF = care_cost_lower_AF*med_frac, 
  per_case_med_mean = care_cost_mean*med_frac, 
  per_case_med_mean_AF = care_cost_mean_AF*med_frac,
  per_case_med_upper = care_cost_upper*med_frac, 
  per_case_med_upper_AF = care_cost_upper_AF*med_frac,
  # per case high estimates + bounds + with and without A
  per_case_high_lower = care_cost_lower,
  per_case_high_lower_AF = care_cost_lower_AF,
  per_case_high_mean = care_cost_mean,
  per_case_high_mean_AF = care_cost_mean_AF,
  per_case_high_upper = care_cost_upper, 
  per_case_high_upper_AF = care_cost_upper_AF
)]

# print low, med, high estimates for RC and OC using care time fractions: 
rc_fracs = RC_OC[type == "Replacement Cost"]
oc_fracs = RC_OC[type == "Opportunity Cost"]
# Low: ADL support
cat("ADL support:\n", "Replacement cost | $", 
    round(rc_fracs$per_case_low_mean_AF), " (", 
    round(rc_fracs$per_case_low_lower_AF), "-", 
    round(rc_fracs$per_case_low_upper_AF), ")\n",
    "Foregone wage cost | $", 
    round(oc_fracs$per_case_low_mean_AF), " (", 
    round(oc_fracs$per_case_low_lower_AF), "-", 
    round(oc_fracs$per_case_low_upper_AF), ")", sep = "")
# Medium: ADL+IADL support
cat("ADL+IADL support:\n", "Replacement cost | $", 
    round(rc_fracs$per_case_med_mean_AF), " (", 
    round(rc_fracs$per_case_med_lower_AF), "-", 
    round(rc_fracs$per_case_med_upper_AF), ")\n",
    "Foregone wage cost | $", 
    round(oc_fracs$per_case_med_mean_AF), " (", 
    round(oc_fracs$per_case_med_lower_AF), "-", 
    round(oc_fracs$per_case_med_upper_AF), ")", sep = "")
# High: All caregiving
cat("All caregiving:\n", "Replacement cost | $", 
    round(rc_fracs$per_case_high_mean_AF), " (", 
    round(rc_fracs$per_case_high_lower_AF), "-", 
    round(rc_fracs$per_case_high_upper_AF), ")\n",
    "Foregone wage cost | $", 
    round(oc_fracs$per_case_high_mean_AF), " (", 
    round(oc_fracs$per_case_high_lower_AF), "-", 
    round(oc_fracs$per_case_high_upper_AF), ")", sep = "")

# get total US dementia cases in 2019
US_cases2019 = dem_cases[year_id == 2019 & location_id == 102]

# merge in dementia cases
# add columns with right names for merging
US_cases2019[, ":=" (year = 2019, 
                     state = "USA")] 
total_RC_OC = merge(RC_OC, US_cases2019, by = c("year", "state"))

# calculate total costs (dividing by 1 billion to have number reported in billions)
total_RC_OC[, ":=" (
  # per case lower estimates + bounds + with and without AF
  total_low_lower = per_case_low_lower*cases,
  total_low_lower_AF = per_case_low_lower_AF*cases, 
  total_low_mean = per_case_low_mean*cases, 
  total_low_mean_AF = per_case_low_mean_AF*cases,
  total_low_upper = per_case_low_upper*cases, 
  total_low_upper_AF = per_case_low_upper_AF*cases,
  # per case medium estimates + bounds + with and without AF
  total_med_lower = per_case_med_lower*cases,
  total_med_lower_AF = per_case_med_lower_AF*cases, 
  total_med_mean = per_case_med_mean*cases, 
  total_med_mean_AF = per_case_med_mean_AF*cases,
  total_med_upper = per_case_med_upper*cases, 
  total_med_upper_AF = per_case_med_upper_AF*cases,
  # per case high estimates + bounds + with and without AF
  total_high_lower = per_case_high_lower*cases,
  total_high_lower_AF = per_case_high_lower_AF*cases,
  total_high_mean = per_case_high_mean*cases,
  total_high_mean_AF = per_case_high_mean_AF*cases,
  total_high_upper = per_case_high_upper*cases, 
  total_high_upper_AF = per_case_high_upper_AF*cases
)]

# calculate total costs for just alzheimer's (dividing by 1 billion to have number reported in billions)
# get percent of total dementia cases that are from alzheimer's 
AD_percent = dx_subtypes[age_group_name == "All Age" & sex == "Both" & 
                           type_label == "Alzheimer's disease"]$proportion
# multiply total high costs estimates + bounds by percent of dementia that is AD 
total_RC_OC[, ":=" (
  AD_lower = total_high_lower_AF*AD_percent,
  AD_mean = total_high_mean_AF*AD_percent,
  AD_upper = total_high_upper_AF*AD_percent
)]

# print values for easier number plugging: 
RC_costs = total_RC_OC[type == "Replacement Cost"]
OC_costs = total_RC_OC[type == "Opportunity Cost"]

## Per-Case Table: 
# Per-case Low (ADL):
# RC
paste0(round(RC_costs$per_case_low_mean), " (", 
       round(RC_costs$per_case_low_lower), " - ",
       round(RC_costs$per_case_low_upper), ")")
# OC
paste0(round(OC_costs$per_case_low_mean), " (", 
       round(OC_costs$per_case_low_lower), " - ",
       round(OC_costs$per_case_low_upper), ")")

# Per-case Low Attributed (ADL):
# RC
paste0(round(RC_costs$per_case_low_mean_AF), " (", 
       round(RC_costs$per_case_low_lower_AF), " - ",
       round(RC_costs$per_case_low_upper_AF), ")")
# OC
paste0(round(OC_costs$per_case_low_mean_AF), " (", 
       round(OC_costs$per_case_low_lower_AF), " - ",
       round(OC_costs$per_case_low_upper_AF), ")")

# Per-case Medium (ADL+IADL):
# RC
paste0(round(RC_costs$per_case_med_mean), " (", 
       round(RC_costs$per_case_med_lower), " - ",
       round(RC_costs$per_case_med_upper), ")")
# OC
paste0(round(OC_costs$per_case_med_mean), " (", 
       round(OC_costs$per_case_med_lower), " - ",
       round(OC_costs$per_case_med_upper), ")")

# Per-case Medium Attributed (ADL+IADL):
# RC
paste0(round(RC_costs$per_case_med_mean_AF), " (", 
       round(RC_costs$per_case_med_lower_AF), " - ",
       round(RC_costs$per_case_med_upper_AF), ")")
# OC
paste0(round(OC_costs$per_case_med_mean_AF), " (", 
       round(OC_costs$per_case_med_lower_AF), " - ",
       round(OC_costs$per_case_med_upper_AF), ")")

# Per-case High (All Care):
# RC
paste0(round(RC_costs$per_case_high_mean), " (", 
       round(RC_costs$per_case_high_lower), " - ",
       round(RC_costs$per_case_high_upper), ")")
# OC
paste0(round(OC_costs$per_case_high_mean), " (", 
       round(OC_costs$per_case_high_lower), " - ",
       round(OC_costs$per_case_high_upper), ")")

# Per-case High Attributed (All Care):
# RC
paste0(round(RC_costs$per_case_high_mean_AF), " (", 
       round(RC_costs$per_case_high_lower_AF), " - ",
       round(RC_costs$per_case_high_upper_AF), ")")
# OC
paste0(round(OC_costs$per_case_high_mean_AF), " (", 
       round(OC_costs$per_case_high_lower_AF), " - ",
       round(OC_costs$per_case_high_upper_AF), ")")

## Totals Table: 
# Total Low (ADL):
# RC
paste0(round(RC_costs$total_low_mean/1e9), " (", 
       round(RC_costs$total_low_lower/1e9), " - ",
       round(RC_costs$total_low_upper/1e9), ")")
# OC
paste0(round(OC_costs$total_low_mean/1e9), " (", 
       round(OC_costs$total_low_lower/1e9), " - ",
       round(OC_costs$total_low_upper/1e9), ")")

# Total Low Attributed (ADL):
# RC
paste0(round(RC_costs$total_low_mean_AF/1e9), " (", 
       round(RC_costs$total_low_lower_AF/1e9), " - ",
       round(RC_costs$total_low_upper_AF/1e9), ")")
# OC
paste0(round(OC_costs$total_low_mean_AF/1e9), " (", 
       round(OC_costs$total_low_lower_AF/1e9), " - ",
       round(OC_costs$total_low_upper_AF/1e9), ")")

# Total Medium (ADL+IADL):
# RC
paste0(round(RC_costs$total_med_mean/1e9), " (", 
       round(RC_costs$total_med_lower/1e9), " - ",
       round(RC_costs$total_med_upper/1e9), ")")
# OC
paste0(round(OC_costs$total_med_mean/1e9), " (", 
       round(OC_costs$total_med_lower/1e9), " - ",
       round(OC_costs$total_med_upper/1e9), ")")

# Total Medium Attributed (ADL+IADL):
# RC
paste0(round(RC_costs$total_med_mean_AF/1e9), " (", 
       round(RC_costs$total_med_lower_AF/1e9), " - ",
       round(RC_costs$total_med_upper_AF/1e9), ")")
# OC
paste0(round(OC_costs$total_med_mean_AF/1e9), " (", 
       round(OC_costs$total_med_lower_AF/1e9), " - ",
       round(OC_costs$total_med_upper_AF/1e9), ")")

# Total High (All Care):
# RC
paste0(round(RC_costs$total_high_mean/1e9), " (", 
       round(RC_costs$total_high_lower/1e9), " - ",
       round(RC_costs$total_high_upper/1e9), ")")
# OC
paste0(round(OC_costs$total_high_mean/1e9), " (", 
       round(OC_costs$total_high_lower/1e9), " - ",
       round(OC_costs$total_high_upper/1e9), ")")

# Total High Attributed (All Care):
# RC
paste0(round(RC_costs$total_high_mean_AF/1e9), " (", 
       round(RC_costs$total_high_lower_AF/1e9), " - ",
       round(RC_costs$total_high_upper_AF/1e9), ")")
# OC
paste0(round(OC_costs$total_high_mean_AF/1e9), " (", 
       round(OC_costs$total_high_lower_AF/1e9), " - ",
       round(OC_costs$total_high_upper_AF/1e9), ")")

## Total Cost All dementia vs Alzheimer's Only estimates: 
# All Dementia 
# RC
paste0(round(RC_costs$total_high_mean_AF/1e9), " (", 
       round(RC_costs$total_high_lower_AF/1e9), " - ",
       round(RC_costs$total_high_upper_AF/1e9), ")")
# OC
paste0(round(OC_costs$total_high_mean_AF/1e9), " (", 
       round(OC_costs$total_high_lower_AF/1e9), " - ",
       round(OC_costs$total_high_upper_AF/1e9), ")")
# Alzheimer's Only
# RC
paste0(round(RC_costs$AD_mean/1e9), " (", 
       round(RC_costs$AD_lower/1e9), " - ",
       round(RC_costs$AD_upper/1e9), ")")
# OC
paste0(round(OC_costs$AD_mean/1e9), " (", 
       round(OC_costs$AD_lower/1e9), " - ",
       round(OC_costs$AD_upper/1e9), ")")

##################### CREATE PRETTY TABLE WITH GT PACKAGE ######################
# *not really using this anymore, package wasn't that helpful*
# *decided to just number plug into table made in powerpoint instead* ^^^
# if need to use again, code is old and likely needs to updated to most recent
# variable names / estimates


# create estimate strings with lower and upper bounds to use in table:
# replacement cost: 
RC_per_case_low_str = paste0(RC_per_case$per_case_low_mean, " (",
                             RC_per_case$per_case_low_lower, " - ", 
                             RC_per_case$per_case_low_upper, ")")
RC_per_case_low_AF_str = paste0(RC_per_case$per_case_low_mean_AF, " (",
                             RC_per_case$per_case_low_lower_AF, " - ", 
                             RC_per_case$per_case_low_upper_AF, ")")

RC_per_case_med_str = paste0(RC_per_case$per_case_med_mean, " (",
                             RC_per_case$per_case_med_lower, " - ", 
                             RC_per_case$per_case_med_upper, ")")
RC_per_case_med_AF_str = paste0(RC_per_case$per_case_med_mean_AF, " (",
                                RC_per_case$per_case_med_lower_AF, " - ", 
                                RC_per_case$per_case_med_upper_AF, ")")

RC_per_case_high_str = paste0(RC_per_case$per_case_high_mean, " (",
                             RC_per_case$per_case_high_lower, " - ", 
                             RC_per_case$per_case_high_upper, ")")
RC_per_case_high_AF_str = paste0(RC_per_case$per_case_high_mean_AF, " (",
                                RC_per_case$per_case_high_lower_AF, " - ", 
                                RC_per_case$per_case_high_upper_AF, ")")

# opportunity cost:
OC_per_case = RC_OC[type == "Opportunity Cost"]

OC_per_case_low_str = paste0(OC_per_case$per_case_low_mean, " (",
                             OC_per_case$per_case_low_lower, " - ", 
                             OC_per_case$per_case_low_upper, ")")
OC_per_case_low_AF_str = paste0(OC_per_case$per_case_low_mean_AF, " (",
                                OC_per_case$per_case_low_lower_AF, " - ", 
                                OC_per_case$per_case_low_upper_AF, ")")

OC_per_case_med_str = paste0(OC_per_case$per_case_med_mean, " (",
                             OC_per_case$per_case_med_lower, " - ", 
                             OC_per_case$per_case_med_upper, ")")
OC_per_case_med_AF_str = paste0(OC_per_case$per_case_med_mean_AF, " (",
                                OC_per_case$per_case_med_lower_AF, " - ", 
                                OC_per_case$per_case_med_upper_AF, ")")

OC_per_case_high_str = paste0(OC_per_case$per_case_high_mean, " (",
                              OC_per_case$per_case_high_lower, " - ", 
                              OC_per_case$per_case_high_upper, ")")
OC_per_case_high_AF_str = paste0(OC_per_case$per_case_high_mean_AF, " (",
                                 OC_per_case$per_case_high_lower_AF, " - ", 
                                 OC_per_case$per_case_high_upper_AF, ")")

table = data.table(type = c("Replacement Cost", "Opportunity Cost"), 
                   Per_Case_Low = c(RC_per_case_low_str, OC_per_case_low_str),
                   Per_Case_Low_Attributed = c(RC_per_case_low_AF_str, OC_per_case_low_AF_str),
                   Per_Case_Medium = c(RC_per_case_med_str, OC_per_case_med_str),
                   Per_Case_Medium_Attributed = c(RC_per_case_med_AF_str, OC_per_case_med_AF_str),
                   Per_Case_High = c(RC_per_case_high_str, OC_per_case_high_str),
                   Per_Case_High_Attributed = c(RC_per_case_high_AF_str, OC_per_case_high_AF_str)
                   )

pretty_table = table %>% gt(rowname_col = "type") %>% 
  tab_header(title = "Yearly Cost per Person Attributed to Dementia, in 2019 Dollars.") %>%
  fmt_markdown(columns = c(Per_Case_Low, Per_Case_Low_Attributed,
                           Per_Case_Medium, Per_Case_Medium_Attributed, 
                           Per_Case_High, Per_Case_High_Attributed))

