## Create CPS data file grouped by gender, year, age, and state to be merged with HRS
# Date: 01/20/23
# Author: Michael Breshock, Will Sogge

# clear environment
rm(list = ls())

# load libraries
library(dplyr)
library(data.table)
library(tidyr)


# load data
cps = fread(file = paste0(FILEPATH,"CPS_wages_LFP_2010_21.csv"))

# group data by just year, sex, and age and take weighted average wage 
nat_cps_sex_age = cps[year %in% c(2010, 2012, 2014, 2016, 2018)] %>%
  group_by(year, pesex, age) %>%
  summarise(nat_wage_mean = weighted.mean(mean_wage, w = population_weight),
            nat_LFP = weighted.mean(LFP, w = population_weight)) %>% 
  rename(age_bins = age, sex = pesex)

# group by just year and sex
nat_cps_sex = cps[year %in% c(2010, 2012, 2014, 2016, 2018)] %>%
  group_by(year, pesex) %>%
  summarise(nat_wage_mean = weighted.mean(mean_wage, w = population_weight),
            nat_LFP = weighted.mean(LFP, w = population_weight)) %>% 
  rename(sex = pesex)

# group by year, sex, age, and education
nat_cps_sex_age_education = cps[year %in% c(2010, 2012, 2014, 2016, 2018)] %>%
  group_by(year, pesex, age, education) %>%
  summarise(nat_wage_mean = weighted.mean(mean_wage, w = population_weight),
            nat_LFP = weighted.mean(LFP, w = population_weight)) %>% 
  rename(age_bins = age, sex = pesex, educ_bins = education)


# save wages to file
fwrite(nat_cps_sex_age, file = paste0(FILEPATH,"wages_by_sex_age_2010_2018.csv"))
fwrite(nat_cps_sex, file = paste0(FILEPATH,"wages_by_sex_2010_2018.csv"))
fwrite(nat_cps_sex_age_education, file = paste0(FILEPATH,"wages_by_sex_age_education_2010_2018.csv"))


#For HRS, combine all of these into one file for national wages depending on the demographic
HRS_nat_cps <- merge(nat_cps_sex, nat_cps_sex_age, by=c("year", "sex", "nat_wage_mean", "nat_LFP"), all=TRUE)
HRS_nat_cps <- merge(HRS_nat_cps, nat_cps_sex_age_education, by=c("year", "sex", "age_bins", "nat_wage_mean", "nat_LFP"), all=TRUE)
HRS_nat_cps[is.na(HRS_nat_cps)] <- -8

fwrite(HRS_nat_cps, file=paste0(FILEPATH,"wages_for_HRS_2010_2018.csv"))
