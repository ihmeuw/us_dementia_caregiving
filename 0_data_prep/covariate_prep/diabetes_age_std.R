##########################################################################
### Author: Amy Lastuka
### Date: 03/24/2023
### Project: US dementia spending
### Purpose: create diabetes prevalence estimates
#            that are age-standardized to the US population
#            This is used as a covariate for our stage 1 model
#            of caregiving hours
##########################################################################

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

source("FILEPATH/get_age_weights.R")
source("FILEPATH/get_outputs.R")
source("FILEPATH/get_population.R")

data_dir <- "FILEPATH"
release <- 6 # GBD 2019 iterative 
gbd_round <- 6 # GBD 2019
gbd_decomp <- 'iterative'

# define functions
age_sex_std <- function(df_as, value_col, as_weights, col_name) {
  
  # perform age standardization across any set of ages using US age proportions
  # perform sex standardization using USA sex proportions
  
  # rescale weights based off age groups in data
  #as_sum <- sum(as_weights[age_group_id %in% df_as$age_group_id & sex_id %in% df_as$sex_id, weight, by="year_id"])
  as_weights[age_group_id %in% df_as$age_group_id & sex_id %in% df_as$sex_id, as_sum := sum(weight), by="year_id"]
  #print(head(as_weights$as_sum))
  as_weights <- as_weights[age_group_id %in% df_as$age_group_id & sex_id %in% df_as$sex_id] %>%
    .[, as_weight := weight/as_sum]
  #print(head(as_weights))
  
  df_as <- merge(df_as, as_weights, by = c("age_group_id", "sex_id", "year_id"))
  df_as <- df_as[, .(age_std_val = sum(get(value_col)* as_weight)), by = c("location_id", "year_id"), ]
  
  # sex weights
  setnames(df_as, "age_std_val", col_name)
  
  return(df_as)
}

age_list <- get_age_weights(gbd_round_id = gbd_round) # get list of age groups
as_weights <- get_population(age_group_id = age_list$age_group_id,
                             location_id = 102,
                             year_id = 2010:2019,
                             sex_id = 3,
                             gbd_round_id = gbd_round,
                             decomp_step = gbd_decomp,
                             status = 'best')
as_weights <- as_weights[, weight:=population/sum(population), by="year_id"][, .(sex_id, age_group_id, year_id, weight)]



diabetes_rate <- get_outputs(topic = "cause",
                             measure_id = 5,  # prevalence
                             metric_id = 3,   # rate
                             location_id = c(102,523:573),   # united states
                             cause_id = c(975, 976),   # diabetes type1,2
                             release_id = release,   # GBD 2019
                             #compare_version_id = 7378,
                             age_group_id = unique(age_list$age_group_id),#seq(5,21),   #22 is all ages, 27 is age-standardized rates, 5-21 covers 0-80+
                             sex_id = 3, # both sex
                             year_id = seq(2010, 2019))

diabetes_rate <- data.table(diabetes_rate)
diabetes_rate[, diabetes := sum(val), by = c("age_group_id","location_id","year_id")] # sum values for diabetes type 1 and 2
diabetes_rate <- diabetes_rate[, .SD[1], by = c("age_group_id","location_id","year_id")]



########## END: age-standardize to US age distribution #########
diabetes_std <- age_sex_std(diabetes_rate, "diabetes", as_weights, "diabetes_std")

fwrite(diabetes_std, paste0(data_dir,"diabetes_covariate.csv"))

