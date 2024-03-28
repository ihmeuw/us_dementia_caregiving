# Number plugging - results section 3.1
# Authors: Amy Lastuka and Michael Breshock

rm(list = ls())

# load in libraries
library(dplyr)
library(data.table)


source("FILEPATH/helper_functions.R")
source("FILEPATH/get_location_metadata.R")

# load in location set (51 US states)
locs_full <- get_location_metadata(location_set_id = 35, gbd_round_id = 6, release_id = 6)
locs <- locs_full[(location_id==102) | (parent_id == 102), .(state = location_name, location_id)] 

run_num_wage <- "0014" #this run number corresponds to a top-code of 40 hours
run_num_112 <- "004"
run_num_40 <- "04"

# Sentence 1: In 2019 US dollars, the national average hourly cost was $XX (XX-XX) in 2010 and rose to $XX (XX-XX) in 2019

hha = fread(paste0(FILEPATH,"HHA_wages_cost_PPA_2019_draws.csv"))

# summarize draws:
hha_sum = hha[,.(cost_mean = mean(H_COST_PPA2019), 
                 cost_lower = quantile(H_COST_PPA2019, 0.025), 
                 cost_upper = quantile(H_COST_PPA2019, 0.975)), 
              by = c("YEAR", "STATE", "location_id")]

hha_mean_2010 <- hha_sum[(YEAR==2010) & (location_id==102)]$cost_mean
hha_lower_2010 <- hha_sum[(YEAR==2010) & (location_id==102)]$cost_lower
hha_upper_2010 <- hha_sum[(YEAR==2010) & (location_id==102)]$cost_upper

hha_mean_2019 <- hha_sum[(YEAR==2019) & (location_id==102)]$cost_mean
hha_lower_2019 <- hha_sum[(YEAR==2019) & (location_id==102)]$cost_lower
hha_upper_2019 <- hha_sum[(YEAR==2019) & (location_id==102)]$cost_upper

paste0("In 2019 US dollars, the national average hourly cost was $",
       round(hha_mean_2010,2), " (", 
       round(hha_lower_2010,2), "-", round(hha_upper_2010,2), ") in 2010 and rose to $", 
       round(hha_mean_2019,2), " (", 
       round(hha_lower_2019,2), "-", round(hha_upper_2019,2), ") in 2019.")


# Sentence 2: The expected wage of a caregiver also increased over time; rising from $XX (XX-XX) in 2010 to $XX (XX-XX) in 2019

OC_wages = fread(paste0(FILEPATH,"US_wages_run_",run_num_wage,"_draws.csv"))

wages_sum = OC_wages[,.(wage_mean = mean(wage), 
                        wage_lower = quantile(wage, 0.025), 
                        wage_upper = quantile(wage, 0.975)), 
                     by = c("year_id", "location_id")]

oc_wage_2010 <- wages_sum[(year_id==2010) & (location_id==102)]$wage_mean
oc_lower_2010 <- wages_sum[(year_id==2010) & (location_id==102)]$wage_lower 
oc_upper_2010 <- wages_sum[(year_id==2010) & (location_id==102)]$wage_upper


oc_wage_2019 <- wages_sum[(year_id==2019) & (location_id==102)]$wage_mean
oc_lower_2019 <- wages_sum[(year_id==2019) & (location_id==102)]$wage_lower 
oc_upper_2019 <- wages_sum[(year_id==2019) & (location_id==102)]$wage_upper

paste0("The expected wage of a caregiver also increased over time; rising from $",
       round_num(oc_wage_2010, type="decimal", decimal_places = 2, lancet = F)," (",
       round_num(oc_lower_2010, type="decimal", decimal_places = 2, lancet = F),"-",
       round_num(oc_upper_2010, type="decimal", decimal_places = 2, lancet = F),") in 2010 to $",
       round_num(oc_wage_2019, type="decimal", decimal_places = 2, lancet = F)," (",
       round_num(oc_lower_2019, type="decimal", decimal_places = 2, lancet = F),"-",
       round_num(oc_upper_2019, type="decimal", decimal_places = 2, lancet = F),") in 2019.")


# Sentence 3: When allowing up to 112 hours per week of care, caregiving hours 
# increased from XX (XX-XX) in 2010 to XX (XX-XX) per week in 2019.

# load data
# want the attributable hours, which are available in the cost of care draws files: 
# RC file created in: US_annual_replacement_cost_of_dementia_informal_care_per_case.R
rc_draws = fread(paste0(FILEPATH,"annual_cost_of_care_draws_top_code_112.csv"))

# summarize attributable hours from draws: 
hours_112_sum = rc_draws[, .(hours_mean = mean(dem_hours), 
                             hours_lower = quantile(dem_hours, 0.025),
                             hours_upper = quantile(dem_hours, 0.975)), 
                         by = c("year_id", "location_id", "location_name")]


RC_care_hours_2010 <- hours_112_sum[(year_id==2010) & (location_id==102)]$hours_mean
RC_care_lower_2010 <- hours_112_sum[(year_id==2010) & (location_id==102)]$hours_lower
RC_care_upper_2010 <- hours_112_sum[(year_id==2010) & (location_id==102)]$hours_upper

RC_care_hours_2019 <- hours_112_sum[(year_id==2019) & (location_id==102)]$hours_mean
RC_care_lower_2019 <- hours_112_sum[(year_id==2019) & (location_id==102)]$hours_lower
RC_care_upper_2019 <- hours_112_sum[(year_id==2019) & (location_id==102)]$hours_upper

paste0("When allowing up to 112 hours per week of care, caregiving hours increased from ",
round_num(RC_care_hours_2010, type="decimal", lancet = F)," (",
round_num(RC_care_lower_2010, type="decimal", lancet = F),"-",
round_num(RC_care_upper_2010, type="decimal", lancet = F),") in 2010 to ",
round_num(RC_care_hours_2019, type="decimal", lancet = F)," (",
round_num(RC_care_lower_2019, type="decimal", lancet = F),"-",
round_num(RC_care_upper_2019, type="decimal", lancet = F),") per week in 2019.")

# Sentence 3.5: When capping caregiving time at 40 hours per week, caregiving 
# hours increased from XX (95% UI XX-XX) in 2010 to XX (95% UI XX-XX) per week in 2019

# OC file created in: US_annual_opportunity_cost_of_dementia_informal_care_per_case.R
oc_draws = fread(paste0(FILEPATH,"annual_cost_of_care_draws_top_code_40.csv"))

hours_40_sum = oc_draws[, .(hours_mean = mean(dem_hours), 
                            hours_lower = quantile(dem_hours, 0.025),
                            hours_upper = quantile(dem_hours, 0.975)), 
                        by = c("year_id", "location_id", "location_name")]

OC_care_hours_2010 <- hours_40_sum[(year_id==2010) & (location_id==102)]$hours_mean
OC_care_lower_2010 <- hours_40_sum[(year_id==2010) & (location_id==102)]$hours_lower
OC_care_upper_2010 <- hours_40_sum[(year_id==2010) & (location_id==102)]$hours_upper

OC_care_hours_2019 <- hours_40_sum[(year_id==2019) & (location_id==102)]$hours_mean
OC_care_lower_2019 <- hours_40_sum[(year_id==2019) & (location_id==102)]$hours_lower
OC_care_upper_2019 <- hours_40_sum[(year_id==2019) & (location_id==102)]$hours_upper

paste0("When capping caregiving time at 40 hours per week, caregiving hours increased from ",
       round_num(OC_care_hours_2010, type="decimal", lancet = F)," (",
       round_num(OC_care_lower_2010, type="decimal", lancet = F),"-",
       round_num(OC_care_upper_2010, type="decimal", lancet = F),") in 2010 to ",
       round_num(OC_care_hours_2019, type="decimal", lancet = F)," (",
       round_num(OC_care_lower_2019, type="decimal", lancet = F),"-",
       round_num(OC_care_upper_2019, type="decimal", lancet = F),") per week in 2019.")

# Sentence 4: In 2019, the cost of a home health aide varied 
# from $XX (XX-XX) in [cheapest state] to $XX (XX-XX) in [most expensive state]

temp <- hha_sum[YEAR==2019,]
setorder(temp, cols = "cost_mean")  
head(temp)

cheapest_state <- temp[1,]$STATE
most_expensive_state <- temp[nrow(temp),]$STATE

cheapest_mean <- temp[STATE==cheapest_state]$cost_mean
cheapest_lower <- temp[STATE==cheapest_state]$cost_lower
cheapest_upper <- temp[STATE==cheapest_state]$cost_upper

expensive_mean <- temp[STATE==most_expensive_state]$cost_mean
expensive_lower <- temp[STATE==most_expensive_state]$cost_lower
expensive_upper <- temp[STATE==most_expensive_state]$cost_upper

paste0("In 2019, the cost of a home health aide varied from $", 
       round_num(cheapest_mean,type="decimal",decimal_places = 2,lancet = F)," (",
       round_num(cheapest_lower,type="decimal",decimal_places = 2,lancet = F), "-",
       round_num(cheapest_upper,type="decimal",decimal_places = 2,lancet = F),") in ",
       cheapest_state, " to $", 
       round_num(expensive_mean,type="decimal",decimal_places = 2,lancet = F)," (",
       round_num(expensive_lower,type="decimal",decimal_places = 2,lancet = F), "-",
       round_num(expensive_upper,type="decimal",decimal_places = 2,lancet = F),") in ",
       most_expensive_state, ".")

# Sentence 5: Caregivers’ expected hourly wage varied from $XX (XX-XX) in
# [cheapest state] to $XX (XX-XX) in [most expensive state]. 

wages_temp <- wages_sum[year_id==2019]
wages_temp <- merge(wages_temp, locs, by="location_id")
setorder(wages_temp, cols = "wage_mean")

cheapest_wage_state <- wages_temp[1,]$state

cheapest_wage_mean <- wages_temp[state==cheapest_wage_state]$wage_mean
cheapest_wage_lower <- wages_temp[state==cheapest_wage_state]$wage_lower
cheapest_wage_upper <- wages_temp[state==cheapest_wage_state]$wage_upper


expensive_wage_state <- wages_temp[nrow(wages_temp),]$state

expensive_wage_mean <- wages_temp[state==expensive_wage_state]$wage_mean
expensive_wage_lower <- wages_temp[state==expensive_wage_state]$wage_lower
expensive_wage_upper <- wages_temp[state==expensive_wage_state]$wage_upper

paste0("Caregivers’ expected hourly wage varied from $",
       round_num(cheapest_wage_mean,type="decimal",decimal_places = 2,lancet = F)," (",
       round_num(cheapest_wage_lower,type="decimal",decimal_places = 2,lancet = F),"-",
       round_num(cheapest_wage_upper,type="decimal",decimal_places = 2,lancet = F),") in ",
       cheapest_wage_state," to ",
       round_num(expensive_wage_mean,type="decimal",decimal_places = 2,lancet = F)," (",
       round_num(expensive_wage_lower,type="decimal",decimal_places = 2,lancet = F),"-",
       round_num(expensive_wage_upper,type="decimal",decimal_places = 2,lancet = F),") in ",
       expensive_wage_state,".")
      


# Sentence 6: Weekly caregiving hours per case ranged from XX (XX-XX) in 
# [state with least hours] to XX (XX-XX) in [state with most hours]. 
hours_temp <- hours_112_sum[year_id==2019]
#hours_temp <- merge(hours_temp, locs, by="location_id") - names are already in there

setorder(hours_temp, cols = "hours_mean")

least_hours_state <- hours_temp[1,]$location_name

least_hours_mean <- hours_temp[location_name==least_hours_state]$hours_mean
least_hours_lower <- hours_temp[location_name==least_hours_state]$hours_lower
least_hours_upper <- hours_temp[location_name==least_hours_state]$hours_upper

most_hours_state <- hours_temp[nrow(hours_temp),]$location_name

most_hours_mean <- hours_temp[location_name==most_hours_state]$hours_mean
most_hours_lower <- hours_temp[location_name==most_hours_state]$hours_lower
most_hours_upper <- hours_temp[location_name==most_hours_state]$hours_upper

paste0("Weekly caregiving hours per case ranged from ",
       round_num(least_hours_mean, type="decimal",lancet=F)," (",
       round_num(least_hours_lower, type="decimal",lancet=F),"-",
       round_num(least_hours_upper, type="decimal",lancet=F),") in ",
       least_hours_state," to ",
       round_num(most_hours_mean, type="decimal",lancet=F)," (",
       round_num(most_hours_lower, type="decimal",lancet=F),"-",
       round_num(most_hours_upper, type="decimal",lancet=F),") in ",
       most_hours_state,".")

# forgone wage hours: 
hours40_temp <- hours_40_sum[year_id==2019]

setorder(hours40_temp, cols = "hours_mean")

least_hours_state40 <- hours40_temp[1,]$location_name

least_hours_mean40 <- hours40_temp[location_name==least_hours_state]$hours_mean
least_hours_lower40 <- hours40_temp[location_name==least_hours_state]$hours_lower
least_hours_upper40 <- hours40_temp[location_name==least_hours_state]$hours_upper

most_hours_state40 <- hours40_temp[nrow(hours_temp),]$location_name

most_hours_mean40 <- hours40_temp[location_name==most_hours_state]$hours_mean
most_hours_lower40 <- hours40_temp[location_name==most_hours_state]$hours_lower
most_hours_upper40 <- hours40_temp[location_name==most_hours_state]$hours_upper

paste0("Weekly caregiving hours per case ranged from ",
       round_num(least_hours_mean40, type="decimal",lancet=F)," (",
       round_num(least_hours_lower40, type="decimal",lancet=F),"-",
       round_num(least_hours_upper40, type="decimal",lancet=F),") in ",
       least_hours_state40," to ",
       round_num(most_hours_mean40, type="decimal",lancet=F)," (",
       round_num(most_hours_lower40, type="decimal",lancet=F),"-",
       round_num(most_hours_upper40, type="decimal",lancet=F),") in ",
       most_hours_state40,".")

