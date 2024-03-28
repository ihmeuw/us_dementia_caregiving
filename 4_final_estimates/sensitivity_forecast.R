## Forecasting - Sensitivity Analysis
## forecast forgone wage with top-code 112 hours/week
## Author: Michael Breshock
## Date: 10/24/2023

# ** This is a sensitivity analysis for the forgone wage approach **
# ** Here the caregiving hours are top-coded to 112 hours/week, **
# ** instead of 40 hours/week, which is used in the base case estimates. **

### Forcasting under two scenarios:
## Scenario #1 -> Low Growth
# Costs per case are kept constant at the mean 2019 rate.

## Scenario #2 -> High Growth 
# Costs per case are forcasted with a time trend. 
# Costs per case are modeled over time using linear regression with the 2010-2019 data.
# This model is then used to predict costs per case for 2020-2050. 

rm(list=ls())

i
source("FILEPATH/get_population.R")

library(ggplot2)
library(viridis)
library(data.table)
library("metR", lib.loc = "FILEPATH")
library(MASS)

# the file name includes the location_id - we only need location 102 (US)

# draws from 2020 - 2030
filename1 <- 'FILEPATH/collapseddraws_102.rds'
prev1 <- readRDS(filename1)
setDT(prev1)

# draws from 2031 - 2050
filename2 <- 'FILEPATH/collapseddraws_102.rds'
prev2 <- readRDS(filename2)
setDT(prev2)

# we only need 2030, 2040, and 2050 and only need age group = 22
prev1 <- prev1[(year_id==2030) & (age_group_id==22),]
prev2 <- prev2[(year_id==2040 | year_id==2050) & (age_group_id==22),]

prev_dt <- rbind(prev1,prev2)

# combine sex 1 and 2 - get total count of cases and overall prevalence for both sexes
prev_draws <- prev_dt[, .(cases = sum(num), prevalence = weighted.mean(prev,num)),
                      by = c("draw","year_id")]

# read in 2019 dementia prevalence draws: 
dem_cases = fread(paste0(FILEPATH,"total_draws_1990_2019.csv"))
prev2019 = dem_cases[year_id == 2019 & location_id == 102, 
                     .(year_id, location_id, draw, prevalence, cases)]

# restructure prevalence forecast data to match 2019 data: 
prev_draws[, location_id := 102] # add US national location id
prev_draws[, draw := paste0("draw_", draw - 1)] # match 2019 draw variable coding

prev_draws = rbind(prev2019, prev_draws)

############################## LOW GROWTH SCENARIO #############################
## cost per case is held constant (mean value from 2010-2019)
##forecast forgone wages: 

# load forgone wage estimates @ 112 topcode
# OC file created in: US_annual_opportunity_cost_of_dementia_informal_care_per_case.R
oc_draws = fread(paste0(FILEPATH,"annual_cost_of_care_draws_top_code_112_sensitive.csv"))

US_oc = oc_draws[location_id == 102]

# add per-case costs: 
cost_forecasts = copy(prev_draws)
cost_forecasts[,oc := mean(US_oc$cost_attr)]

# change 2019 data to use direct estimates:
cost_forecasts[year_id == 2019, oc := US_oc[year_id == 2019]$cost_attr]

# calculate total costs (cost per case * number of cases): 
cost_forecasts[, total_oc := oc*cases]

## per-capita forecasts: 
# get US population for 2019: 
US_pop2019 <- get_population(release_id = 6, location_id = 102,
                             year_id = 2019, sex_id = 3,
                             age_group_id = c(9:20, 30:32, 235)) # ages 20+ 

US_pop2019 = US_pop2019[, .(population = sum(population)), 
                        by = c("year_id", "location_id")]

# get US population forecasts for 2030, 2040, and 2050: 
forecast_file = "FILEPATH/population.nc"

# load population forecast data: 
# see variables/dimensions in nc file: 
GlanceNetCDF(forecast_file)
pop_forecast = ReadNetCDF(forecast_file, 
                          subset = list(year_id = c(2019, 2050), 
                                        location_id = 102))[year_id %in% c(2019, 2030, 2040, 2050)]

# aggregate values for all sex and ages 20+
pop_forecast_all = pop_forecast[age_group_id %in% c(9:20, 30:32, 235),
                                .(population = sum(population)), 
                                by = c("year_id","location_id","scenario","draw")]

# filter to reference scenario = 0
pop_ref = pop_forecast_all[scenario == 0]

# use 2019 population from get_population output instead of forecast file: 
pop_ref[year_id == 2019]$population = US_pop2019$population

# remove scenario variable from pop forecast data
pop_ref[, scenario := NULL]
# recode draw variable for merging: 
pop_ref[, draw := paste0("draw_", draw)]

# add US total population variable to cost forecasting data.table: 
cost_forecasts = merge(cost_forecasts, pop_ref, 
                       by = c("year_id", "location_id", "draw"))

# calculate per capita costs: 
cost_forecasts[, per_cap_oc := total_oc / population]

# save out low growth cost forecast draws
fwrite(cost_forecasts, file = paste0(FILEPATH,"sensitivity_low_growth_cost_draws.csv"))

############################# HIGH GROWTH FORECAST #############################
## Forgone wage model: 
# create regression model to forecast per case costs: 
## loop through each draw and create a regression model for each:
set.seed(123)
# initialize empty df to store draws in 
oc_pred = data.table()
for (d in paste0("draw_", c(0:999))) {
  model_df = US_oc[draw == d]
  oc_lm = lm(cost_attr ~ year_id, data = model_df) # create regression model for draw
  # pull a random draw of coefficients from the variance-covariance matrix
  betas = mvrnorm(1, oc_lm$coefficients, vcov(oc_lm)) 
  intercept = betas[["(Intercept)"]] # extracting coefficient values
  year_coef = betas[["year_id"]] 
  # make predictions using regression coefficients: 
  pred_df = data.table(year_id = c(2010:2019, 2030, 2040, 2050))
  pred_df[, cost_attr := year_id*year_coef + intercept]
  pred_df[, draw := d] # add current draw num to data
  # save predictions to dataframe:
  oc_pred = rbind(oc_pred, pred_df)
}

# add source indicators for plotting 
US_oc[, source := "actual"]
oc_pred[, source := "predicted"]

compare_OC = rbind(US_oc[,.(year_id, draw, cost_attr, source)],
                   oc_pred)

# summarize draws for plotting: 
compare_OC_sum = compare_OC[,.(cost_mean = mean(cost_attr), 
                               cost_lower = quantile(cost_attr, 0.025),
                               cost_upper = quantile(cost_attr, 0.975)), 
                            by = c("year_id", "source")]

# plot predicted and actual: 
ggplot(compare_OC_sum, aes(x = year_id, y = cost_mean, ymin = cost_lower, 
                           ymax = cost_upper, fill = source)) + 
  geom_ribbon(alpha = 0.2) + geom_point(aes(color = source)) + 
  scale_y_continuous(labels = scales::comma_format()) + 
  labs(title = "Forgone Wage Cost Forecasting")
# calculate root mean squared error: 
Metrics::rmse(US_oc$cost_attr, oc_pred[year_id %in% c(2010:2019)]$cost_attr)

## calculate total costs 2019-2050 using per-case cost linear trend:

# merge predicted per-case costs with number of cases: 
setnames(oc_pred, old = "cost_attr", new = "oc")
# merge predicted forgone wage cost per case: 
trend_forecasts = merge(prev_draws, oc_pred[,.(year_id, draw, oc)], 
                        by = c("year_id", "draw"))

# change 2019 data to use direct estimates:
trend_forecasts[year_id == 2019, oc := US_oc[year_id==2019]$cost_attr]

# calculate total costs (cost per case * number of cases): 
trend_forecasts[, total_oc := oc*cases]

## per-capita costs: 
# add US total population variable to cost forecasting data.table: 
trend_forecasts = merge(trend_forecasts, pop_ref, 
                        by = c("year_id", "location_id", "draw"))

# calculate per capita costs: 
trend_forecasts[, per_cap_oc := total_oc / population]

# save out high growth cost forecast draws
fwrite(trend_forecasts, file = paste0(FILEPATH,"sensitivity_high_growth_cost_draws.csv"))

