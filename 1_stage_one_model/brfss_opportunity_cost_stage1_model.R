##########################################################################
### Author: Amy Lastuka
### Date: 08/30/2022
### Project: US dementia spending
### Purpose: run first-stage model for caregiver expected wages
###          (used for the forgone wage method)
##########################################################################

# This script is used to create the stage1 model for caregiver wages, 
# The stage1 model creates predictions of caregivers' expected hourly wage
# for all US states and years. The stage1 model is used as input
# to ST=GPR along with the raw data. 

rm(list=ls())

library(haven)
library(data.table)
library("cdlTools", lib.loc = "/FILEPATH/rlibs")
library(usmap)
library(ggplot2)
library(foreign)
library(pbapply)
library(ggpubr)
library(stargazer)

out_dir <- 'FILEPATH'
st_gpr_dir <- 'FILEPATH'
output_dir <- 'FILEPATH'
census_dir <- 'FILEPATH'

# using topcode of 40 hrs per week for forgone wages
top_code = "40"
outlier = 1

# read in the BRFSS state level caregiving data 
brfss_cost_dt <- fread(paste0(out_dir,"dementia_caregiving_total_cost_",top_code,".csv"))

# read in the GDP deflator data
gdp_deflator <- fread("FILEPATH/gdp_deflator.csv")
gdp_deflator <- gdp_deflator[, .(year, factor2019)]

# adjust caregiving costs to all be in 2019 dollars
brfss_cost_dt <- merge(brfss_cost_dt, gdp_deflator, by = "year")

brfss_cost_dt[, cost_per_hour_2019:= cost_mean*factor2019]
brfss_cost_dt[, cost_lower_2019:= cost_lower*factor2019]
brfss_cost_dt[, cost_upper_2019:= cost_upper*factor2019]

# only keep the columns we need to merge with the other data sources
brfss_cost_dt <- brfss_cost_dt[, .(year,state, sample_size, cost_per_hour_2019, cost_lower_2019, cost_upper_2019,sample_var = cost_var*sample_size )]

# also add NHATS data NHATS_caregiving_hours_2011_2017 (IMPORTANT: cost adjustment was already done in that file)
if(top_code=="168"){
  nhats_costs <- fread('FILEPATH/NHATS_OP_caregiving_opportunity_costs_2019_SP_weights.csv')
} else if(top_code==112){
  nhats_costs <- fread('FILEPATH/NHATS_OP_caregiving_opportunity_costs_topcode_112.csv')
} else{
  nhats_costs <- fread('FILEPATH/NHATS_OP_caregiving_opportunity_costs_topcode_40.csv')
}

nhats_costs[, state:="United States of America"] #this matches the naming from get_location_metadata
setnames(nhats_costs, c("hourly_cost_mean2019","hourly_cost_lower2019","hourly_cost_upper2019","N"), c("cost_per_hour_2019","cost_lower_2019","cost_upper_2019","sample_size"))
nhats_costs[, sample_var:= (cost_upper_2019 - cost_lower_2019)/(2*1.96)*sample_size]

# TODO add HRS data - need to update to 2019 dollars and get names of columns set up correctly
hrs_costs <- fread(paste0('FILEPATH/HRS_cost_estimate_2010_2018_55up_',top_code,'.csv'))
hrs_costs[, state:="United States of America"] #this matches the naming from get_location_metadata
hrs_costs <- merge(hrs_costs, gdp_deflator, by = "year")
hrs_costs[, cost_per_hour_2019:= cost_mean*factor2019]
hrs_costs[, cost_lower_2019:= cost_lower*factor2019]
hrs_costs[, cost_upper_2019:= cost_upper*factor2019]
hrs_costs[, sample_var:= (cost_upper_2019 - cost_lower_2019)/(2*1.96)*sample_size]

# only keep the columns we need to merge with the other data sources
nhats_costs <- nhats_costs[, .(year,state, sample_size, cost_per_hour_2019, cost_lower_2019, cost_upper_2019, sample_var)]
hrs_costs <- hrs_costs[, .(year,state, sample_size, cost_per_hour_2019, cost_lower_2019, cost_upper_2019, sample_var)]

brfss_cost_dt <- rbind(brfss_cost_dt, nhats_costs)
brfss_cost_dt <- rbind(brfss_cost_dt, hrs_costs)

########### MEDIAN INCOME - data prep ######################
# read in the median income data
median_income <- fread(paste0(census_dir,"median_income_prepped.csv"))



###################################################################
#### start working on covariates
####################################################################

source("FILEPATH/get_location_metadata.R")
source("FILEPATH/get_population.R")
source("FILEPATH/get_covariate_estimates.R")
source("FILEPATH/get_outputs.R")
source("FILEPATH/get_age_weights.R")

release <- 6 # GBD 2019 iterative 
gbd_round <- 6 # GBD 2019

######## create caregiving hours per prevalent case

# pull dementia prevalence
dementia_cases <- get_outputs(topic = "cause",
                              measure_id = 5,  # prevalence 
                              metric_id = 1,   # number
                              location_id = c(102,523:573),   # states in the united states
                              cause_id = c(543),   # ADRD
                              release_id = release,   # GBD 2019
                              #compare_version_id = 7670,
                              age_group_id = 22,   
                              sex_id = 3, # both sex
                              year_id = seq(2010, 2019))

# keep only the variables we need and also rename them
dementia_cases <- dementia_cases[, .(state = location_name, dem_cases = val, year = year_id)]

brfss_cost_dt <- merge(brfss_cost_dt, dementia_cases, by = c("state","year"))



# load in location set (51 US states)
locs_full <- get_location_metadata(location_set_id = 35, gbd_round_id = gbd_round, release_id = release)
locs <- locs_full[(location_id==102) | (parent_id == 102), .(state = location_name, location_id)] 



#### merge MEDIAN INCOME data ####
median_income[, factor2019:=NULL]
test_df <- merge(brfss_cost_dt, median_income, by = c("state","year"))

# cut Florida ? wages don't have as many outliers as hours
# Modified z-score = 0.6745(xi – x̃) / MAD
median_cost <- median(test_df$cost_per_hour_2019 )
test_df[, abs_diff := abs(median_cost - cost_per_hour_2019)]
med_abs_diff <- median(test_df$abs_diff)
test_df[, mod_z := 0.6745*(cost_per_hour_2019 - median_cost)/med_abs_diff]

# save out a list of the state-years that get cut
if(outlier==1){
  outliers <- test_df[abs(mod_z) >= 2,]
  test_df <- test_df[abs(mod_z) < 2,]
  fwrite(outliers,paste0("FILEPATH/outliers_foregone_wage_",top_code,".csv"))
}




######### BEGIN: models ################################


# try log-log model
test_df[,log_cost := log(cost_per_hour_2019)]
test_df[,log_income := log(income_adj)]

# also log the upper and lower bounds
test_df[, cost_lower_2019 := log(cost_lower_2019)]
test_df[, cost_upper_2019 := log(cost_upper_2019)]


test_model_preferred <- lm(log_cost ~ log_income, test_df)
summary(test_model_preferred)

model_df <- test_df[, .(log_cost, log_income, sample_size)]

#  *** Preferred Model: log-log cost and adjusted median income, weighted by sample size ####
final_model <- lm(log_cost ~ log_income, weights = sample_size, model_df)
summary(final_model)
#  ***

# create readable label names for model_df
attr(model_df,"names") 
attr(model_df, "model.varnames") <- attr(model_df,"names")
attr(model_df,"model.varnames") <- c("Expected wage (log)", "Income (log)", "Sample size")

final_model_table <- paste0(output_dir,'final_model_OC_',top_code,'.html')
#stargazer(final_model, type='html', out=final_model_table)

stargazer(final_model, type = "html",         
          covariate.labels = attr(model_df, "model.varnames")[2], 
          dep.var.labels = attr(model_df, "model.varnames")[1],
          out = final_model_table)

#### look at the U.S. actual vs. fitted values
fitted_values <- data.table(predict(test_model_preferred))
names(fitted_values) <- c("fitted_values")
fitted_values <- cbind(test_df, fitted_values)
fitted_values[state=="United States of America", .(state, year, log_cost, fitted_values)]


# Leave code in for one plot just for reference
ggplot(test_df, aes(x=log_income,y=log_cost)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_cor(aes(label = ..p.label..), p.digits = 3,label.x.npc="left", label.y.npc="bottom") +
  ggtitle("BRFSS - log cost per hour (2019 $) \n vs log median income (2019 $)")


##################################################
# create and write data file for st-gpr - basically this is BRFSS caregiving file with some renaming, plus covariates
#################################################

#merge in locs so we have location_id
test_df <- merge(test_df, locs, by = "state")

data_for_stgpr <- test_df[, .(year_id = year, location_id, val = log_cost, sample_size, log_income, sex_id=3, age_group_id = 22, measure = "continuous", measure_id = 19, nid = 999999, variance = sample_var, is_outlier = 0, cost_lower_2019, cost_upper_2019)]
if(outlier==1){
  fwrite(data_for_stgpr, paste0(st_gpr_dir,"caregiving_data_opportunity_cost_",top_code,".csv"))
}else{
  fwrite(data_for_stgpr, paste0(st_gpr_dir,"caregiving_data_opportunity_cost_",top_code,"_keep_outliers.csv"))
}


##################################################
####### create and write out fitted values for use in ST-GPR
##################################################

source("FILEPATH/get_location_metadata.R")

########### pull income variable:
# Median income is not in shared functions; 
# just create a file with all 0s outside of the US

location_list <- unique(locs_full$location_id) 
years <- 2010:2019

global_covs <- expand.grid(location_list, years)
names(global_covs) <- c("location_id","year_id")

# --- get list of US locations and years
locs_us <- locs_full[(parent_id==102) | location_id==102, .(location_id, state = location_name)]
location_us_list <- unique(locs_us$location_id) 
us_covs <- expand.grid(location_us_list, years)
names(us_covs) <- c("location_id", "year")

# --- add median income
median_income <- merge(median_income, locs_us, by = "state")
median_income[,log_income := log(income_adj)]
us_covs <- merge(us_covs, median_income, by = c("location_id","year"))


pred_values <- data.table(predict(final_model, newdata = us_covs))
names(pred_values) <- c("cv_custom_stage_1")
pred_values <- cbind(us_covs, pred_values) # the predicted values are in the same order as test_df, just append the column

setnames(pred_values, "year", "year_id")
global_covs <- merge(global_covs, pred_values, by = c("location_id", "year_id"), all.x = TRUE)
global_covs <- data.table(global_covs)

# everywhere outside the US the cv_custom_stage_1 is NA, replace with 0s
global_covs[is.na(cv_custom_stage_1), cv_custom_stage_1:=0]

# add age_group_id and sex_id, I believe these are needed for ST-GPR
global_covs[, age_group_id := 22]
global_covs[, sex_id := 3]

# I have some empty columns here, delete these
global_covs[, state:=NULL]
global_covs[, income:=NULL]
global_covs[, income_adj:=NULL]
global_covs[, log_income:=NULL]
global_covs[, std_error:=NULL]

##################################################################
# write output file for use in ST-GPR
##################################################################
if(outlier==1){
  fwrite(global_covs,paste0(st_gpr_dir, "caregiving_predictions_opportunity_cost_",top_code,".csv"))
}else{
  fwrite(global_covs,paste0(st_gpr_dir, "caregiving_predictions_opportunity_cost_",top_code,"_keep_outliers.csv"))
}

