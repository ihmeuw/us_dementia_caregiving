## National Hourly Opportunity Cost of Care Estimates using OP caregiving hours 
## 01/13/2023
## Author: Michael Breshock

setwd("FILEPATH")
rm(list = ls())

# load in libraries
library(dplyr)
library(data.table)
library(tidyr)
library(haven)



# load all data
# CPS average wages by demographic
cps = fread(paste0(FILEPATH,"CPS_wages_LFP_2010_21.csv"))

# get weekly caregiving hours by year, spid, and opid
OP_hours = fread(paste0(FILEPATH,"OP_caregiving_hours.csv"), 
                 colClasses = list(character=c("opid"))) # need to do this so it doesnt read opid as an integer
# change name of age column so not confused with OP age
setnames(OP_hours, old = "age", new = "SP_age")


top_code = 40 # set this value to top-code care hours 
# top-code caregiving hours 
OP_hours[, care_hours_week := pmin(care_hours_week, top_code)]

# get OP demographic data
OP_demo = fread(paste0(FILEPATH,"OP_age_sex_education.csv"), 
                colClasses = list(character=c("opid"))) # need to do this so it doesnt read opid as an integer

# set blank values as NAs
OP_demo[age_bins == ""]$age_bins <- NA
OP_demo[education == ""]$education <- NA

# get national mean wages by year, age, sex, and education from CPS data
natCPS = cps[year %in% c(2011:2019)] %>% 
  group_by(year, pesex, age, education) %>% 
  summarise(nat_wage_mean = weighted.mean(mean_wage, w = population_weight),
            nat_LFP = weighted.mean(LFP, w = population_weight)) %>% 
  rename(age_bins = age, sex = pesex)
setDT(natCPS)

# merge OP_hours with OP_demographics
OP = merge(OP_hours, OP_demo, by = c("year", "spid", "opid"))

# filter to only OPs that provided all demographic data for age sex and education
OP_full = OP[!is.na(age_bins) & !is.na(sex) & !is.na(education)] # this gives better estimates than using not-full data

# create 56 estimates using 56 repeated weights 
# save all estimates to single data frame 
cost_estimates = data.table() # initiate blank data 
for(rep in c(1:56)){
  pt_weight = paste0("anfinwgt", rep) # patient weight variable
  # sum caregiving hours by demographic 
  NHATS_sum <- OP_full[, .(cg_mean = weighted.mean(care_hours_week, w = eval(parse(text = pt_weight))), 
                              samp_size = .N, pop_weight = sum(eval(parse(text = pt_weight)))), 
                          by=c("year", "education","age_bins","sex")]
  
  # merge in wage data from CPS into NHATS care data
  wage_cg = merge(NHATS_sum, natCPS, by = c("year", "age_bins", "sex", "education"), 
                  all.x = T, allow.cartesian = T)
  # avg weekly cost of care by demographic: 
  # mean care hours * population weight * mean wage per hour * labor force participation rate
  wage_cg[, weekly_cost := cg_mean*pop_weight*nat_wage_mean*nat_LFP] 
  
  # sum up cost over all demographic groups
  nhats_cost_final <- wage_cg[,.(total_cost=sum(weekly_cost,na.rm=T),total_hours=sum(cg_mean*pop_weight,na.rm=T), 
                                 sample_size = sum(samp_size), pop = sum(pop_weight)), by=c("year")]
  nhats_cost_final[, avg_cost_per_hour:= total_cost/total_hours] 
  nhats_cost_final[, draw := rep]
  # bind estimates together
  cost_estimates = rbind(cost_estimates, nhats_cost_final) 
}

cost_summary = cost_estimates[, .(weekly_hours_lower = quantile(total_hours, 0.025),
                                  weekly_hours_mean = mean(total_hours),
                                  weekly_hours_upper = quantile(total_hours, 0.975),
                                  weekly_cost_lower = quantile(total_cost, 0.025),
                                  weekly_cost_mean = mean(total_cost), 
                                  weekly_cost_upper = quantile(total_cost, 0.975),
                                  hourly_cost_lower = quantile(avg_cost_per_hour, 0.025),
                                  hourly_cost_mean = mean(avg_cost_per_hour),
                                  hourly_cost_upper = quantile(avg_cost_per_hour, 0.975),
                                  population = mean(pop), 
                                  N = mean(sample_size)), by = c("year")]

# add GBD deflater: 
# read in the GDP deflator data
gdp_deflator <- fread(paste0(FILEPATH,"gdp_deflator.csv"))
gdp_deflator <- gdp_deflator[, .(year, factor2019)]

# adjust caregiving costs to all be in 2019 dollars
final_cost <- merge(cost_summary, gdp_deflator, by = "year")
final_cost[, ":=" (hourly_cost_lower2019 = hourly_cost_lower*factor2019,
                   hourly_cost_mean2019 = hourly_cost_mean*factor2019, 
                   hourly_cost_upper2019 = hourly_cost_upper*factor2019)]

# save to .csv file
fwrite(final_cost, paste0(FILEPATH,"NHATS_OP_caregiving_opportunity_costs_topcode_",top_code,".csv"))


