# Script to aggregate and make plots of wage ST-GPR results - OPPORTUNITY COST MODEL
# these are the wages a person forgoes due to caregiving
# Author: Michael Breshock
# Date: 5/01/2023

# clear environment
rm(list = ls())

# load libraries
library(ggplot2)
library(dplyr)
library(data.table)
library(tidyr)
source("FILEPATH/get_population.R")



# model index number used for st_gpr run configuration - dont forget to update!
model_num = 14

# read in data

raw_input = fread(paste0(FILEPATH,"caregiving_data_opportunity_cost_40.csv"))
stage1_input = fread(paste0(FILEPATH,"caregiving_predictions_opportunity_cost_40.csv"))
gpr_means = fread(paste0(FILEPATH,"stgpr_cost_gpr_run_00", model_num, ".csv"))



# transform data from log space 
raw_input[, ':=' (val = exp(val),
                  cost_lower_2019 = exp(cost_lower_2019),
                  cost_upper_2019 = exp(cost_upper_2019))]
stage1_input[, cv_custom_stage_1 := exp(cv_custom_stage_1)]
gpr_means[, ':=' (wages = exp(val),
                  wages_lower = exp(lower),
                  wages_upper = exp(upper))]


################# AGGREGATE US NATIONAL ESTIMATES FROM STATES ##################
# filter to just US national and state data
state_gpr = gpr_means[location_id %in% c(523:573)] # 523-573 US states

# get US population for weights
US_pop <- get_population(release_id = 6, location_id = c(523:573), 
                         year_id = c(2010:2019))

# merge in US population with ST-GPR outputs
state_gpr_pop = merge(state_gpr, US_pop[,.(year_id, location_id, population)], 
                      by = c("year_id", "location_id"))


# get national means from state data
nat_state_mean = data.table(
  state_gpr_pop %>% group_by(year_id) %>%
    summarise(state_care_wages = weighted.mean(wages, w = population),
              state_care_wages_lower = weighted.mean(wages_lower, w = population),
              state_care_wages_upper = weighted.mean(wages_upper, w = population))
)

# combine US national means with state data
final_estimates = gpr_means[location_id %in% c(102, 523:573)]
# replace US care hours estimates from GPR with aggregate means: 
final_estimates[location_id == 102]$wages = nat_state_mean$state_care_wages
final_estimates[location_id == 102]$wages_lower = nat_state_mean$state_care_wages_lower
final_estimates[location_id == 102]$wages_upper = nat_state_mean$state_care_wages_upper

# save out aggregated US data
fwrite(final_estimates, file = paste0(FILEPATH, model_num, "_keep_outliers.csv"))

# combine data
setnames(raw_input, c("val", "cost_lower_2019", "cost_upper_2019"), 
         c("wages", "wages_lower", "wages_upper"))
setnames(stage1_input, "cv_custom_stage_1", "wages")


raw_input[, data_type := "raw"]
stage1_input[, ':=' (data_type = "custom_stage1",
                     wages_lower = NA,
                     wages_upper = NA)]
final_estimates[, data_type := "aggregated"]
gpr_means[, data_type := "gpr"]

raw_df = raw_input[, .(year_id, location_id, wages, wages_lower, wages_upper, data_type)]
stage1_df = stage1_input[, .(year_id, location_id, wages, wages_lower, wages_upper, data_type)]
final_df = final_estimates[, .(year_id, location_id, wages, wages_lower, wages_upper, data_type)]
gpr_df = gpr_means[, .(year_id, location_id, wages, wages_lower, wages_upper, data_type)]

wage_data = rbind(raw_df, stage1_df, final_df, gpr_df)

############# plot time series for all 50 states #############

# create dictionary of US State location_ids
state_dict <- c("US"="102", "AL"="523","AK"="524","AZ"="525","AR"="526","CA"="527","CO"="528","CT"="529","DE"="530",
                "DC"="531","FL"="532","GA"="533","HI"="534","ID"="535","IL"="536","IN"="537","IA"="538","KS"="539",
                "KY"="540","LA"="541","ME"="542", "MD"="543","MA"="544", "MI"="545","MN"="546",
                "MS"="547","MO"="548","MT"="549","NE"="550","NV"="551","NH"="552","NJ"="553","NM"="554",
                "NY"="555","NC"="556","ND"="557","OH"="558","OK"="559","OR"="560","PA"="561","RI"="562",
                "SC"="563","SD"="564","TN"="565","TX"="566","UT"="567","VT"="568","VA"="569","WA"="570",
                "WV"="571","WI"="572","WY"="573")

# used this to see plotting output before making all state plots
# loc_id = state_dict[["OR"]]
# state_df = cg_data[location_id == loc_id]
# ggplot(state_df, aes(x = as.integer(year_id), y = cg_hours, ymin = cg_lower,
# ymax = cg_upper, color = data_type)) +
#   geom_line() + geom_point() + 
#   geom_ribbon(data = state_df[data_type == "stgpr"], alpha=0.15) +
#   geom_errorbar(data = state_df[data_type == "raw"]) + 
#   labs(x = "Year",y = "Caregiving Hours (per case)",
#        title = paste("OR", "Average Per Case Caregiving Hours Estimates over time")) +
#   scale_x_continuous(limits = c(2010, 2019), breaks = seq(2010, 2019, 1))

data_dir = "FILEPATH"
# filter to just US locations
wage_data = wage_data[location_id %in% state_dict]
# get min and max y values for y axis scale:
y_min = floor(min(wage_data$wages_lower, na.rm = T))
y_max = ceiling(max(wage_data$wages_upper, na.rm = T))

# remember to update the run number or old plots may be overwritten
pdf(paste0(data_dir,"brfss_cost_estimates_states_run",model_num,"_aggregated_keep_outliers.pdf"))
for(state_key in names(state_dict)){
  loc_id = state_dict[[state_key]]
  state_df = wage_data[location_id == loc_id]
  
  p = ggplot(state_df, aes(x = year_id, y = wages, ymin = wages_lower, 
                           ymax = wages_upper, color = data_type)) +
    geom_line(data = state_df[data_type != "raw"]) + geom_point() +  # lines only for st-gpr and custom stage 1 data
    geom_ribbon(data = state_df[data_type %in% c("gpr", "aggregated")], alpha=0.15) + # ribbon confidence interval only for st-gpr
    geom_errorbar(data = state_df[data_type == "raw"]) + # error bars only for raw data
    labs(x = "Year", y = "Caregiving Cost per Hour (per case)", 
         title = paste(state_key, "Average Hourly Caregiving Cost per Case Over Time")) + 
    scale_x_continuous(limits = c(2010, 2019), breaks = seq(2010, 2019, 1)) + 
    ylim(y_min, y_max)
  
  print(p)
}
dev.off()
