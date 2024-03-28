##########################################################################
### Author: Amy Lastuka
### Date: 08/30/2022
### Project: US dementia spending
### Purpose: run first-stage model for caregiving hours
##########################################################################

# This script is used to create the stage1 model for caregiving hours, 
# either top-coded at 112 hours/week for replacement cost or 
# top-coded at 40 hours/week for forgone wages. The stage1 model creates 
# predictions of  weekly caregiving hours per dementia case for all US states 
# and years. The stage1 model is used as input to ST=GPR along with the raw data. 

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
library(MASS) # for mvrnorm function

# setup - define directories, load shared functions, and specify arguments
out_dir <- "FILEPATH"
census_dir <- "FILEPATH"

source("FILEPATH/get_location_metadata.R")
source("FILEPATH/get_population.R")
source("FILEPATH/get_covariate_estimates.R")
source("FILEPATH/get_outputs.R")
source("FILEPATH/get_age_weights.R")


### constants/flags ###

release <- 6 # GBD 2019 iterative 
gbd_round <- 6 # GBD 2019
gbd_decomp <- 'iterative'

# Note: we are using 112 hours/week top code for the replacement cost model 
#       and 40 hours/week top code for the forgone wage model (previously called opportunity cost)
top_code = 40
top_code_string = as.character(top_code)
outlier <- 1
z_threshold <- 2.5

st_gpr_dir <- paste0("FILEPATH/care_hours_",top_code,"/")
output_dir <- "FILEPATH"

# ------------ Read in and process data from 3 sources - BRFSS, NHATS, and HRS

# 1) BRFSS - for this dataset, we use GBD prevalence estimates since we can't 
# create prevalence estimates directly within BRFSS
brfss_caregiving_dt <- fread(paste0(out_dir,"cg_hours_dementia_patients_by_state_year_envelope_",top_code_string,".csv"))
brfss_caregiving_dt$year <- as.numeric(brfss_caregiving_dt$year)
# there is a column with row numbers; delete this
brfss_caregiving_dt[, V1:=NULL]


# 2) NHATS
if(top_code == 168){
  nhats_data <- fread(paste0('FILEPATH/NHATS_OP_hours_2019_SP_weights_samp_var.csv'))
}else if(top_code==112){
  nhats_data <- fread(paste0('FILEPATH/NHATS_topcode_hours_2019.csv'))
}else if(top_code==40){
  nhats_data <- fread(paste0('FILEPATH/NHATS_OP_hours_2019_top_code_40.csv'))
}

nhats_data[, state:="United States of America"] #this matches the naming from get_location_metadata
nhats_data[, cg_per_case:=mean_per_case]
setnames(nhats_data, 'cg_mean', 'cg_sum')
nhats_data[, mean_per_case:=NULL]
nhats_data[, dementia_pop:=NULL]

nhats_data[, cg_sum:=NULL]
nhats_data[, cg_lower:=NULL]
nhats_data[, cg_upper:=NULL]
nhats_data[, cg_var:=NULL]

# switch to using sample variance versus the replicate weight variance
nhats_data[, cg_var_per_case :=samp_var_per_case]
nhats_data[, samp_var_per_case:=NULL]
#nhats_data[, upper_per_case := log(upper_per_case)]
#nhats_data[, lower_per_case := log(lower_per_case)]

brfss_caregiving_dt <- rbind(brfss_caregiving_dt, nhats_data)

# 3) HRS
hrs_data <- fread(paste0('FILEPATH/HRS_caregiving_hours_2010_2018_55up_',top_code_string,'.csv'))
hrs_data[, state:="United States of America"] #this matches the naming from get_location_metadata
hrs_data[, cg_var_per_case :=cg_var]
hrs_data[, cg_per_case:=cg_mean]
hrs_data[, cg_mean:=NULL]
hrs_data[, upper_per_case :=cg_upper]
hrs_data[, lower_per_case :=cg_lower]

hrs_data[, cg_sum:=NULL]
hrs_data[, cg_lower:=NULL]
hrs_data[, cg_upper:=NULL]
hrs_data[, cg_var:=NULL]

# create HRS dummy variable 
hrs_data$hrs_indicator <- 1
brfss_caregiving_dt$hrs_indicator <- 0
brfss_caregiving_dt <- rbind(brfss_caregiving_dt, hrs_data)

# ------------ Create other covariates needed for the model
# load in location set 
locs <- get_location_metadata(location_set_id = 35, gbd_round_id = gbd_round, release_id = release)
locs <- locs[(location_id==102) | (parent_id == 102), .(location_name, location_id)] 

########### pull most up to date population estimates, subsetting to US states 
# pops <- get_population(release_id = release, location_id = -1, year_id = -1, age_group_id = 22)[
#   location_id %in% unique(locs$location_id)] 

########### pull education variable (Education (years per capita) aggregated by age (15+) and sex)
edu <- get_covariate_estimates(covariate_id = 1975, gbd_round_id = gbd_round, decomp_step = "iterative", age_group_id = 22, sex_id = 3, status = 'best')[
  location_id %in% unique(locs$location_id)][year_id %in% seq(2010, 2019)]
setnames(edu, c("location_name","mean_value", "year_id"), c("state","mean_yrs_educ","year"))

# read in median income data
median_income <- fread(paste0(census_dir,"median_income_prepped.csv"))

#HAQI
haqi <- get_covariate_estimates(
  covariate_id = 1099,
  gbd_round_id = 6,
  decomp_step = 'step4',
  location_id = c(102,523:573),
  year_id = seq(2010,2019)
)
haqi <- haqi[, .(state = location_name, year = year_id, haqi = mean_value)]

#### merge EDUCATION data ######
# drop vars we don't need
edu <- data.table(edu)
edu[, age_group_id :=NULL]
#edu[, location_id :=NULL]
edu[, sex_id :=NULL]
test_df <- merge(brfss_caregiving_dt, edu, by = c("state","year"))


#### merge POPULATION data ######
pop_dt <- fread(paste0(out_dir,"age_group_fractions.csv"))
pop_dt[, age_group_id.x :=NULL]
pop_dt[, age_group_id.y :=NULL]
pop_dt[, age_group_id :=NULL]
pop_dt[, location_id :=NULL]
pop_dt[, sex_id :=NULL]
test_df <- merge(test_df, pop_dt, by = c("state","year"))

diabetes_std <- fread(paste0(out_dir,"diabetes_covariate.csv"))

setnames(diabetes_std,"year_id","year")
test_df <- merge(test_df, diabetes_std, by = c("location_id","year"))

# merge HAQI
test_df <- merge(test_df, haqi, by = c("state","year"))


# -------------- exclude outliers based on modified z-score

##### calculate modified z-score
# Modified z-score = 0.6745(xi – x̃) / MAD
median_cg <- median(test_df$cg_per_case)
test_df[, abs_diff := abs(median_cg - cg_per_case)]
med_abs_diff <- median(test_df$abs_diff)
test_df[, mod_z := 0.6745*(cg_per_case - median_cg)/med_abs_diff]

# save out a list of the state-years that get cut
if(outlier==1){
  outliers <- test_df[abs(mod_z) >= z_threshold,]
  test_df <- test_df[abs(mod_z) < z_threshold,]
  fwrite(outliers,paste0("FILEPATH/outliers_caregiving_hours_",top_code,".csv"))
}

# take log of caregiving hours - do this AFTER merging NHATS, HRS, and BRFSS data
test_df[, log_hours:= log(cg_per_case)]
test_df[, lower_per_case:= log(lower_per_case)]
test_df[, upper_per_case:=log(upper_per_case)]

##################################################
# create and write raw data file for st-gpr - this is the caregiving data (BRFSS+NHATS_HRS) with some renaming, plus covariates
#################################################
#cg_var <- var(brfss_caregiving_dt$cg_per_case)

# since we are using a dumy variable for HRS, we can't include those points as raw data point to feed into ST-GPR
var_scale_factor <- 1 # scale up variance to create larger error bounds in ST-GPR
data_for_stgpr <- test_df[hrs_indicator==0, .(year_id = year, location_id, val = log_hours, sample_size, diabetes_std, sex_id=3, age_group_id = 22, measure = "continuous", measure_id = 19, nid = 999999, variance = cg_var_per_case*var_scale_factor, is_outlier = 0, lower_per_case, upper_per_case)]
if(outlier==1){
  fwrite(data_for_stgpr, paste0(st_gpr_dir,"caregiving_data_",top_code_string,".csv"))
}else{
  fwrite(data_for_stgpr, paste0(st_gpr_dir,"caregiving_data_",top_code_string,"_keep_outliers.csv"))
}


######### BEGIN: models ################################


##################################################
####### create and write out fitted values for use in ST-GPR
##################################################


# ----- we don't actually need the covariates from every location - just fill in with zeros
locs_full <- get_location_metadata(location_set_id = 35, gbd_round_id = gbd_round, release_id = release)
#locs_full <- locs_full[, .(location_id)]
location_list <- unique(locs_full$location_id) 
years <- 2010:2019

global_covs <- expand.grid(location_list, years)
names(global_covs) <- c("location_id","year_id")

model_df <- test_df[, .(log_hours, hrs_indicator, mean_yrs_educ, diabetes_std, sample_size)]

# FIT FINAL MODEL
final_model <- lm( log_hours ~ hrs_indicator + mean_yrs_educ + diabetes_std,
                   weights = sample_size, model_df)
summary(final_model)

# create readable label names for model_df
attr(model_df,"names") 
attr(model_df, "model.varnames") <- attr(model_df,"names")
attr(model_df,"model.varnames") <- c("Hours (log)", "HRS indicator", "Mean years of education", "Diabetes prevalence","Sample size")

final_model_table <- paste0(output_dir,'final_model_RC_',top_code,'.html')
#stargazer(final_model, type='html', out=final_model_table)

stargazer(final_model, type = "html",         
          covariate.labels = attr(model_df, "model.varnames")[2:4], 
          dep.var.labels = attr(model_df, "model.varnames")[1],
          out = final_model_table)

# CREATE PREDICTED VALUES FOR US - need to include all years 2010-2019

# --- get list of US locations and years
locs_us <- locs_full[(parent_id==102) | location_id==102, .(location_id)]
location_us_list <- unique(locs_us$location_id) 
us_covs <- expand.grid(location_us_list, years)
names(us_covs) <- c("location_id", "year")

# --- add diabetes, education, and hrs_indicator
edu <- edu[, .(mean_yrs_educ, year, location_id)]
us_covs <- merge(us_covs, edu, by = c("year", "location_id"))
us_covs <- merge(us_covs, diabetes_std, by = c("year", "location_id"))
us_covs$hrs_indicator <- 0

# --- add frac_over_85
pop_dt <- fread(paste0(out_dir,"age_group_fractions.csv"))
pop_dt <- pop_dt[, .(year, location_id, frac_over_85)]
us_covs <- merge(us_covs, pop_dt, by = c("year", "location_id"))


pred_values <- data.table(predict(final_model, newdata = us_covs))
names(pred_values) <- c("cv_custom_stage_1")
pred_values <- cbind(us_covs, pred_values) # the predicted values are in the same order as test_df, just append the column

#MERGE US PREDICTED VALUES WITH THE FULL GLOBAL LOCATION LIST
setnames(pred_values, "year", "year_id")
global_covs <- merge(global_covs, pred_values, by = c("location_id", "year_id"), all.x = TRUE)
global_covs <- data.table(global_covs)

# everywhere outside the US the cv_custom_stage_1 is NA, replace with 0s
global_covs[is.na(cv_custom_stage_1), cv_custom_stage_1:=0]

# add age_group_id and sex_id, I believe these are needed for ST-GPR
global_covs[, age_group_id := 22]
global_covs[, sex_id := 3]


##################################################################
# write output file for use in ST-GPR
##################################################################
if(outlier==1){
  fwrite(global_covs,paste0(st_gpr_dir, "caregiving_predictions_",top_code_string,".csv"))
}else{
  fwrite(global_covs,paste0(st_gpr_dir, "caregiving_predictions_",top_code_string,"_keep_outliers.csv"))
}


# -------------------- take draws of betas so that I can get a confidence interval for the predicted values ---------------


# Pull 100 draws of beta per model
beta_list <- list()

vcov_mat <- vcov(final_model)
beta_means <- coefficients(summary(final_model))[, 1]
beta_draws <- mvrnorm(100, beta_means, vcov_mat) #TODO - set seed
beta_draws <- data.table(beta_draws)
setnames(beta_draws, "(Intercept)", "Intercept")
us_covs <- data.table(us_covs)

# create 100 predicted values
for(i in 1:100){
  intercept <- beta_draws[i]$Intercept
  hrs_beta <- beta_draws[i]$hrs_indicator
  educ_beta <- beta_draws[i]$mean_yrs_educ
  diabetes_beta <- beta_draws[i]$diabetes_std
  
  draw_name <- paste0("pred_value_",i)
  #print(draw_name)

  us_covs[, eval(draw_name):= intercept + hrs_beta*hrs_indicator + 
            educ_beta*mean_yrs_educ + diabetes_beta*diabetes_std]
  
}

col_list <- paste0("pred_value_",1:100)

us_covs$cg_mean <- apply(subset(us_covs, select = col_list), 1, mean)
us_covs$cg_lower <- apply(subset(us_covs, select = col_list), 1, quantile, probs=.025)
us_covs$cg_upper <- apply(subset(us_covs, select = col_list), 1, quantile, probs=.975)
