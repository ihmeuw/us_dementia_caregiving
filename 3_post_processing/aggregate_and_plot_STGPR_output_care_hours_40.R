# Script to aggregate and plot ST-GPR results for US care hours (40 topcode)
# Author: Michael Breshock
# Date: 10/17/2023

# clear environment
rm(list = ls())

# load libraries
library(ggplot2)
library(dplyr)
library(data.table)
library(tidyr)


# model index number used for st_gpr run configuration - dont forget to update!
model_num = 4

# read in data

raw_input = fread(paste0(FILEPATH,"caregiving_data_40.csv"))
stage1_input = fread(paste0(FILEPATH,"caregiving_predictions_40.csv"))
gpr_means = fread(paste0(FILEPATH,'stgpr_cg_gpr_run_0', model_num, '.csv'))

# transform data from log space 
raw_input[, ":=" (val = exp(val),
                  lower_per_case = exp(lower_per_case),
                  upper_per_case = exp(upper_per_case))]
stage1_input[, cv_custom_stage_1 := exp(cv_custom_stage_1)]
gpr_means[, ':=' (care_hours = exp(val),
                  care_hours_lower = exp(lower),
                  care_hours_upper = exp(upper))]

################# AGGREGATE US NATIONAL ESTIMATES FROM STATES ##################
# filter to just US national and state data
state_gpr = gpr_means[location_id %in% c(523:573)] # 523-573 US states

# get number of dementia cases by state for population weighting
dem_cases = fread(file = paste0(FILEPATH,"rate_and_total_cases_1990_2019.csv"))

# merge in dementia prevalence/cases data
state_gpr_cases = merge(state_gpr, dem_cases[,.(year_id, location_id, cases)], 
                        by = c("year_id", "location_id"))

# get national means from state data
nat_state_mean = data.table(
  state_gpr_cases %>% group_by(year_id) %>%
    summarise(state_care_hours = weighted.mean(care_hours, w = cases),
              state_care_hours_lower = weighted.mean(care_hours_lower, w = cases),
              state_care_hours_upper = weighted.mean(care_hours_upper, w = cases))
)

# combine US national means with state data
final_estimates = gpr_means[location_id %in% c(102, 523:573)]
# replace US care hours estimates from GPR with aggregate means: 
final_estimates[location_id == 102]$care_hours = nat_state_mean$state_care_hours
final_estimates[location_id == 102]$care_hours_lower = nat_state_mean$state_care_hours_lower
final_estimates[location_id == 102]$care_hours_upper = nat_state_mean$state_care_hours_upper

# save out aggregated US data
fwrite(final_estimates, file = paste0(FILEPATH, model_num, ".csv"))

# combine data
setnames(raw_input, c("val", "lower_per_case", "upper_per_case"), 
         c("cg_hours", "cg_lower", "cg_upper"), skip_absent = T)
setnames(stage1_input, "cv_custom_stage_1", "cg_hours", skip_absent = T)
setnames(final_estimates, c("care_hours", "care_hours_lower", "care_hours_upper"), 
         c("cg_hours", "cg_lower", "cg_upper"), skip_absent = T)
setnames(gpr_means, c("care_hours", "care_hours_lower", "care_hours_upper"), 
         c("cg_hours", "cg_lower", "cg_upper"), skip_absent = T)

raw_input[, data_type := "raw"]
stage1_input[, ':=' (data_type = "custom_stage1",
                     cg_lower = NA,
                     cg_upper = NA)]
final_estimates[, data_type := "aggregated"]
gpr_means[, data_type := "gpr"]

raw_df = raw_input[, .(year_id, location_id, cg_hours, cg_lower, cg_upper, data_type)]
stage1_df = stage1_input[, .(year_id, location_id, cg_hours, cg_lower, cg_upper, data_type)]
final_df = final_estimates[, .(year_id, location_id, cg_hours, cg_lower, cg_upper, data_type)]
gpr_df = gpr_means[, .(year_id, location_id, cg_hours, cg_lower, cg_upper, data_type)]

cg_data = rbind(raw_df, stage1_df, final_df, gpr_df)

############# plot time series for all 50 states #############

# create dictionary of US State location_ids
state_dict <- c("US" = "102", "AL"="523","AK"="524","AZ"="525","AR"="526","CA"="527","CO"="528","CT"="529","DE"="530",
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
cg_data = cg_data[location_id %in% state_dict]
# get min and max y values for y axis scale:
y_min = floor(min(cg_data$cg_lower, na.rm = T))
y_max = ceiling(max(cg_data$cg_upper, na.rm = T))

# remember to update the run number or old plots may be overwritten
pdf(paste0(data_dir,"US_caregiving_hours_run_0",model_num,"_aggregated.pdf"))
for(state_key in names(state_dict)){
  loc_id = state_dict[[state_key]]
  state_df = cg_data[location_id == loc_id]
  
  p = ggplot(state_df, aes(x = year_id, y = cg_hours, ymin = cg_lower, 
                           ymax = cg_upper, color = data_type)) +
    geom_line(data = state_df[data_type != "raw"]) + geom_point() +  # lines only for st-gpr and custom stage 1 data
    geom_ribbon(data = state_df[data_type %in% c("gpr", "aggregated")], alpha=0.15) + # ribbon confidence interval only for st-gpr
    geom_errorbar(data = state_df[data_type == "raw"]) + # error bars only for raw data
    labs(x = "Year", y = "Caregiving Hours (per case)", 
         title = paste(state_key, "Average Per Case Caregiving Hours over time")) + 
    scale_x_continuous(limits = c(2010, 2019), breaks = seq(2010, 2019, 1)) + 
    ylim(0, y_max)
  
  print(p)
}
dev.off()
