## US Paper Number Plugging Script
## 06/12/2023
## Author: Michael Breshock

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

# summarize mean, lower, and upper estimates from draws: 
rc = rc_draws[,.(cost_mean = mean(cost_attr), 
                 cost_lower = quantile(cost_attr, 0.025), 
                 cost_upper = quantile(cost_attr, 0.975)), 
              by = c("year_id", "location_id", "location_name")]
oc = oc_draws[,.(cost_mean = mean(cost_attr), 
                 cost_lower = quantile(cost_attr, 0.025), 
                 cost_upper = quantile(cost_attr, 0.975)), 
              by = c("year_id", "location_id", "location_name")]

# the national estimates for indirect cost per patient in 2019 was 
# $XX (XX-XX) using replacement cost and $XX (XX-XX) for forgone wages
US_RC2019 = rc[location_id == 102 & year_id == 2019]
US_OC2019 = oc[location_id == 102 & year_id == 2019]

paste0("$", round(US_RC2019$cost_mean), " (", 
       round(US_RC2019$cost_lower), "-", 
       round(US_RC2019$cost_upper), ") using replacement cost and $", 
       round(US_OC2019$cost_mean), " (", 
       round(US_OC2019$cost_lower), "-", 
       round(US_OC2019$cost_upper), ") for opportunity cost.")

# the estimated cost of caregiving varied substantially by state, 
# ranging from $XX (XX-XX) in STATE to $XX (XX-XX) in STATE.
# RC:
min_cost = min(rc[year == 2019]$care_cost_mean_AF)
min_state = rc[year == 2019 & care_cost_mean_AF == min_cost]
max_cost = max(rc[year == 2019]$care_cost_mean_AF)
max_state = rc[year == 2019 & care_cost_mean_AF == max_cost]

paste0("The estimated cost of caregiving varied substantially by state, ", 
       "ranging from $", round(min_state$care_cost_mean_AF), " (",
       round(min_state$care_cost_lower_AF), "-", 
       round(min_state$care_cost_upper_AF), ") in ", min_state$state, " to $", 
       round(max_state$care_cost_mean_AF), " (",
       round(max_state$care_cost_lower_AF), "-", 
       round(max_state$care_cost_upper_AF), ") in ", max_state$state)
#OC:
min_cost_oc = min(oc[year == 2019]$care_cost_mean_AF)
min_state_oc = oc[year == 2019 & care_cost_mean_AF == min_cost_oc]
max_cost_oc = max(oc[year == 2019]$care_cost_mean_AF)
max_state_oc = oc[year == 2019 & care_cost_mean_AF == max_cost_oc]

paste0("The estimated cost of caregiving varied substantially by state, ", 
       "ranging from $", round(min_state_oc$care_cost_mean_AF), " (",
       round(min_state_oc$care_cost_lower_AF), "-", 
       round(min_state_oc$care_cost_upper_AF), ") in ", min_state_oc$state, " to $", 
       round(max_state_oc$care_cost_mean_AF), " (",
       round(max_state_oc$care_cost_lower_AF), "-", 
       round(max_state_oc$care_cost_upper_AF), ") in ", max_state_oc$state)


# the estimates in Hurd (2013) which were $XX (in 2019 dollars) for replacement 
# cost and $XX for opportunity cost
# load deflator table to convert from 2010 USD to 2019: 
gdp_deflator <- fread("FILEPATH/gdp_deflator.csv")
factor2019 = gdp_deflator[year == 2010]$factor2019
# Hurd RC and OC per case costs in 2010 USD: 
Hurd_RC = round(27789*factor2019)
Hurd_RC_lower = round(21112*factor2019)
Hurd_RC_upper = round(34466*factor2019)
Hurd_OC = round(13188*factor2019)
Hurd_OC_lower = round(9636*factor2019)
Hurd_OC_upper = round(16740*factor2019)

paste0("The estimates in Hurd (2013) which were $", Hurd_RC, " (", 
       Hurd_RC_lower, " - ", Hurd_RC_upper, ")",
       " (in 2019 dollars) for replacement cost and $", Hurd_OC, " (",
       Hurd_OC_lower, " - ", Hurd_OC_upper, ")", " for opportunity cost.")


# The most expensive state (XX STATE) had a replacement cost that was XX times
# higher than the least expensive state (XX STATE)

paste0("The most expensive state (", max_state$state, ") had a replacement cost that ",
"was ", round(max_cost/min_cost,1), " times higher than the least expensive state (",
min_state$state, ")")

# Using a foregone wage approach led to similar variation in cost by state, 
# with XX (most expensive state) being XX times more expensive than XX (cheapest state)

paste0(max_state_oc$state, " (most expensive state) being ", round(max_cost_oc/min_cost_oc,1),
       " times more expensive than ", min_state_oc$state, " (cheapest state)")

## DISCUSSION: 

# Our estimates for indirect costs in 2016 were $XX billion [XX-XX] 
# for the replacement cost and $XX billion [XX-XX] for the forgone wage cost.

# dementia prevalence and total cases draws
prev_draws = fread(paste0(FILEPATH,"total_draws_1990_2019.csv"))

# RC:
# merge cases in with per-case cost draws: 
rc_total_draws = merge(rc_draws, prev_draws, 
                       by = c("year_id", "location_id", "draw"))
# calculate total costs:
rc_total_draws[, ":=" (total_cost = cost_attr*cases)]
# summarize draws: 
rc_total = rc_total_draws[,.(total_cost_mean = mean(total_cost),
                             total_cost_lower = quantile(total_cost, 0.025),
                             total_cost_upper = quantile(total_cost, 0.975)), 
                          by = c("year_id", "location_id", "location_name")]

rc_total_mean2016 = rc_total[location_id == 102 & year_id == 2016]$total_cost_mean
rc_total_lower2016 = rc_total[location_id == 102 & year_id == 2016]$total_cost_lower
rc_total_upper2016 = rc_total[location_id == 102 & year_id == 2016]$total_cost_upper

# OC: 
oc_total_draws = merge(oc_draws, prev_draws, 
                       by = c("year_id", "location_id", "draw"))
# calculate total costs:
oc_total_draws[, ":=" (total_cost = cost_attr*cases)]
# summarize draws: 
oc_total = oc_total_draws[,.(total_cost_mean = mean(total_cost),
                             total_cost_lower = quantile(total_cost, 0.025),
                             total_cost_upper = quantile(total_cost, 0.975)), 
                          by = c("year_id", "location_id", "location_name")]

oc_total_mean2016 = oc_total[location_id == 102 & year_id == 2016]$total_cost_mean
oc_total_lower2016 = oc_total[location_id == 102 & year_id == 2016]$total_cost_lower
oc_total_upper2016 = oc_total[location_id == 102 & year_id == 2016]$total_cost_upper

paste0("Our estimates for indirect costs in 2016 were $", 
       round_num(rc_total_mean2016,type="bill",lancet=F), " billion (", 
       round_num(rc_total_lower2016,type="bill",lancet=F),"-",
       round_num(rc_total_upper2016,type="bill",lancet=F),") for the replacement cost",
       "and $", 
       round_num(oc_total_mean2016,type="bill",lancet=F), " billion (", 
       round_num(oc_total_lower2016,type="bill",lancet=F),"-",
       round_num(oc_total_upper2016,type="bill",lancet=F),") for the foregone wage cost")

# These estimates indicate that with a replacement cost framework the 
# indirect costs of dementia account for XX% [XX-XX] of the total cost and with 
# a forgone wage framework the indirect costs are XX% [XX-XX] of the total cost. 

DEX_estimate = 79.2*10^9 # 79.2 billion

rc_pct_total = rc_total_mean2016 / (rc_total_mean2016 + DEX_estimate)
rc_pct_total_lower = rc_total_lower2016 / (rc_total_lower2016 + DEX_estimate)
rc_pct_total_upper = rc_total_upper2016 / (rc_total_upper2016 + DEX_estimate)

paste0("These estimates indicate that with a replacement cost framework the ", 
       "indirect costs of dementia account for ", 
       round(rc_pct_total*100),"% (",round(rc_pct_total_lower*100),"-", 
       round(rc_pct_total_upper*100),") of the total cost")

oc_pct_total = oc_total_mean2016 / (oc_total_mean2016 + DEX_estimate)
oc_pct_total_lower = oc_total_lower2016 / (oc_total_lower2016 + DEX_estimate)
oc_pct_total_upper = oc_total_upper2016 / (oc_total_upper2016 + DEX_estimate)

paste0("and with a forgone wage framework the indirect costs are ",
       round(oc_pct_total*100),"% (",round(oc_pct_total_lower*100),"-", 
       round(oc_pct_total_upper*100),") of the total cost")

