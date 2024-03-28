# NHATS OP Caregiving Estimates w/ SP weights
# 01/12/2023
# Author: Michael Breshock

setwd("FILEPATH")
rm(list = ls())

# load in libraries
library(dplyr)
library(data.table)
library(tidyr)
library(haven)
library(Hmisc)

# load all data

# weekly caregiving hours by spid from OP file
cg_hours = fread(paste0(FILEPATH,"SP_caregiving_hours.csv"))
# this file made in this script: NHATS_OP_caregiving_hours.R

# top-code hours: 
top_code = 40 # leave this as NA if not top-coding
if(!is.na(top_code)){
  cg_hours[,care_hours_week := pmin(care_hours_week, top_code)]
  top_code_str = paste0("_top_code_",top_code)
} else{
  top_code_str = ""
}

# create 56 estimates using 56 repeated weights 
# save all estimates to single data frame 
cg_estimates = data.table() # initiate blank data 
for(rep in c(1:56)){
  pt_weight = paste0("anfinwgt", rep) # patient weight variable
  
  # first group by YEAR and SP ID (will group by just year next, this is to avoid double counting)
  # sum the weighted number of care hours per week for each SP + Year combo we have
  # also sum the weight by itself to get the dementia population represented by each SP + Year combo
  cg_by_spid = data.table(cg_hours[!is.na(care_hours_week)] %>% group_by(year, spid) %>%
                            # sum alive and deceased caregiver weights for final weight since one will always be zero
                            summarise(sum_weekly_hours = sum(care_hours_week*eval(parse(text = pt_weight)),na.rm = T),
                                      sum_dementia_pop = sum(eval(parse(text = pt_weight)))))
  
  # now group by Year alone and summarise for total weekly hours and dementia population nationally
  cg_total = data.table(cg_by_spid %>% group_by(year) %>% 
                          summarise(total_weekly_hours = sum(sum_weekly_hours),
                                    total_population = sum(sum_dementia_pop),
                                    samp_size = n()))
  cg_total[, draw := rep] # record draw num
  cg_estimates = rbind(cg_estimates, cg_total) # bind estimates together
}
# getting mean and upper and lower bounds from draws
cg_summary = cg_estimates[, .(cg_mean = mean(total_weekly_hours), 
                              cg_lower = quantile(total_weekly_hours, 0.025),
                              cg_upper = quantile(total_weekly_hours, 0.975),
                              cg_var = var(total_weekly_hours),
                              dementia_pop = round(mean(total_population)),
                              sample_size = mean(samp_size)), by = c("year")]
# adding per case variables 
cg_summary[, ":=" (mean_per_case = cg_mean / dementia_pop,
                   lower_per_case = cg_lower / dementia_pop, 
                   upper_per_case = cg_upper / dementia_pop, 
                   cg_var_per_case = cg_var / (dementia_pop*dementia_pop))]

# adding sample variance to estimates
cg_variance = cg_hours[dementia == 1] %>% group_by(year) %>% 
  summarise(samp_variance = wtd.var(care_hours_week, weights = anfinwgt0))

cg_summary[, samp_var_per_case := cg_variance$samp_variance]

# save to model_inputs directory
fwrite(cg_summary, 
       paste0(FILEPATH,"NHATS_OP_hours_2019",top_code_str,".csv"))
