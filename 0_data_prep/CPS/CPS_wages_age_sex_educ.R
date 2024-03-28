## Create CPS data file grouped by gender, education, age, state and year
# Date: 01/20/23
# Author: Michael Breshock

# clear environment
rm(list = ls())

# load libraries
library(ggplot2)
library(readxl)
library(dplyr)
library(data.table)
library(plotly)
library(tidyr)
library("cdlTools", lib.loc = "FILEPATH")



# load data from the csv file created in download_monthly_CPS_code.R
cps = fread(paste0(FILEPATH,"monthly_cps_update_08_22_22.csv"))

#-------------------------------------------------------------------------------
# mean wage by age sex and education:
# age groups based on Hurd paper
age_bins = cps[, age := case_when(prtage >= 18 & prtage <= 34 ~ "18-34",
                                  prtage >= 35 & prtage <= 44 ~ "35-44",
                                  prtage >= 45 & prtage <= 54 ~ "45-54",
                                  prtage >= 55 & prtage <= 64 ~ "55-64",
                                  prtage >= 65 & prtage <= 74 ~ "65-74",
                                  prtage >= 75 ~ "75+",
                                  TRUE ~ "Under 18")]

# creating education bins
cps_bins = age_bins[, education := case_when(peeduca >= 31 & peeduca <= 39 ~ "High school or less",
                                             peeduca >= 40 & peeduca <= 42 ~ "Some college",
                                             peeduca >= 43 ~ "Bachelor's degree or more",
                                             TRUE ~ "???")]
# converting education to factor class
cps_bins[, education := factor(education, levels = c("High school or less", "Some college", "Bachelor's degree or more"))]

# creating num of hours_per_week column based on specific criteria
cps_bins[, hours_per_week := case_when(peernhro > 0 & peernuot == 1 ~ as.numeric(peernhro), # receives overtime pay, take hourly rate as is
                                       peernhro < 0 & prhrusl == 1 ~ 10, # 1 = 0 - 20 hrs
                                       peernhro < 0 & prhrusl == 2 ~ 28, # 2 = 21 - 34 hrs
                                       peernhro < 0 & prhrusl == 3 ~ 37, # 3 = 35 - 39 hrs
                                       peernhro < 0 & prhrusl == 4 ~ 40,# 4 = 40 hrs
                                       peernhro < 0 & prhrusl == 5 & peernuot == 1 ~ 45, # 5 = 41 - 49 hrs & receives overtime pay
                                       peernhro < 0 & prhrusl == 6 & peernuot == 1 ~ 60, # 6 = 50 or more hrs & receives overtime pay
                                       peernhro < 0 & prhrusl %in% c(5,6) & peernuot != 1 ~ 40, # works more than 40 hours but does not receive overtime pay
                                       peernhro < 0 & prhrusl == 7 ~ 40, # 7 = varies - full time -> assuming this means around 40
                                       peernhro < 0 & prhrusl == 8 ~ 20, # 8 = varies - part time -> assuming this means around 20
                                       peernhro < 0 & prhrusl < 0 & prftlf == 1 ~ 40, # 1 = full time labor force
                                       peernhro < 0 & prhrusl < 0 & prftlf == 2 ~ 20, # 2 = part time labor force
                                       TRUE ~ pmin(peernhro, 40))] 
# capping hours per week worked at 40 for those who did not report receiving overtime pay

# calculating final hourly wage variable to include those who only reported weekly salary
cps_bins[, all_hourly := case_when(pternhly < 0 & pternwa > 0 & hours_per_week > 0 ~ pternwa / hours_per_week,
                                   pternhly < 0 & pternwa > 0 & hours_per_week <= 0 ~ pternwa / 40, # assuming they work 40 hrs a week if hours not reported
                                   TRUE ~ pternhly)]

# calculating weighted mean hourly pay for each state, gender, age, education, and year 
avg_wage = data.table(cps_bins[age != "Under 18" & all_hourly >= 0] %>% group_by(gestfips, pesex, age, education, year) %>%
                        summarise(mean_wage = weighted.mean(all_hourly, pwsswgt),
                                  population_weight = sum(pwsswgt)))

#-------------------------------------------------------------------------------
# investigating data: 
# investigate how many people did not report a hourly wage:
cps18 = cps_bins[age != "Under 18"]

cps18[year == 2019, sum(.SD < 0), .SDcols = "all_hourly"] / nrow(cps18[year == 2019])

cps18 %>% group_by(year) %>% 
  summarise(n_missing = sum(all_hourly < 0),
            per_missing = sum(all_hourly < 0) / length(all_hourly))

# investigate how many people reported receiving overtime
cps18[pternhly < 0 & pternwa > 0] %>% group_by(peernuot) %>% summarise(n = n())
# 1 = YES
# 2 = NO
# 77652	/ (77652 +	712644) = 0.098

#-------------------------------------------------------------------------------
# calculating labor force participation rate by state, age, gender, education and year
lfp = cps18 %>% group_by(gestfips, pesex, age, education, year) %>%
  summarise(LFP = sum(prwkstat > 1) / sum(prwkstat > 0))

# sanity check: https://www.bls.gov/emp/tables/civilian-labor-force-participation-rate.htm
# cps18 %>% group_by(age, year) %>%
#   summarise(LFP = sum(prwkstat > 1) / sum(prwkstat > 0))


# merging wage and lfp data and saving to single .csv
wage_lfp = data.table(merge(lfp, avg_wage, by = c("gestfips", "pesex", "age", "education", "year"), all.y = TRUE))
wage_lfp[, state := fips(gestfips, to = "Name")] # add state name column from FIPS code
fwrite(wage_lfp, file = paste0(FILEPATH,"CPS_wages_LFP_2010_21.csv"))


#-------------------------------------------------------------------------------
# estimate national average hours worked, wage, and LFP by age/sex/education and year:
avg_hours = cps18[hours_per_week > 0,
                  .(mean_weekly_hours = weighted.mean(hours_per_week, pwsswgt),
                    population_weight = sum(pwsswgt)), 
                  by = c("year", "pesex", "age", "education")]

avg_wage_nat = cps18[all_hourly >= 0, 
                     .(mean_wage = weighted.mean(all_hourly, pwsswgt)),
                     by = c("year", "pesex", "age", "education")]

lfp_nat = cps18[,.(LFP = sum(prwkstat > 1) / sum(prwkstat > 0)),
                by = c("year", "pesex", "age", "education")]

# merge all together: 
avg_hours_wage_lfp = merge(avg_hours, avg_wage_nat,
                           by = c("year", "pesex", "age", "education"))
avg_hours_wage_lfp = merge(avg_hours_wage_lfp, lfp_nat, 
                           by = c("year", "pesex", "age", "education"))

# check that aggregated means make sense: 
avg_hours_wage_lfp[, .(mean_hours = mean(mean_weekly_hours)), by = "year"]

# save out average hours file: 
fwrite(avg_hours_wage_lfp, file = paste0(FILEPATH,"CPS_work_hours_age_sex_educ_2010_21.csv"))


