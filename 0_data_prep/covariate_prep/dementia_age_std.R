##########################################################################
### Author: Amy Lastuka
### Date: 06/20/2023
### Project: US dementia spending
### Purpose: create dementia prevalence estimates
#            that are age-standardized to the US population
#            This is used to create a 'prevalence ratio' that feeds
#            into the Das Gupta decomposition
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


# get dementia rates for all age groups so that we can age-standardize
dementia_rates <- get_outputs(topic = "cause",
                              measure_id = 5,  # prevalence
                              metric_id = 3,   # rate
                              location_id = c(102,523:573), # national + states in the US
                              cause_id = c(543),   # ADRD
                              release_id = release,   # GBD 2019
                              #compare_version_id = 7670,
                              age_group_id = unique(age_list$age_group_id),
                              sex_id = 3, # both sex
                              year_id = 2019)

dementia_rates <- data.table(dementia_rates)
setnames(dementia_rates,"val","dementia")


########## END: age-standardize to US age distribution #########
dementia_std <- age_sex_std(dementia_rates, "dementia", as_weights, "dementia_std")

# get non-standardized dementia rates for all-ages so we can create a ratio of non-standardized to standardized
dementia_non_std <- get_outputs(topic = "cause",
                              measure_id = 5,  # prevalence
                              metric_id = 3,   # rate
                              location_id = c(102,523:573), # national + states in the US
                              cause_id = c(543),   # ADRD
                              release_id = release,   # GBD 2019
                              #compare_version_id = 7670,
                              age_group_id = 22,
                              sex_id = 3, # both sex
                              year_id = 2019)

dementia_non_std <- dementia_non_std[, .(location_id,year_id,dementia_nonstd = val)]

# merge std and non-std, then create the ratio
dementia_merged <- merge(dementia_std, dementia_non_std, by = c("location_id","year_id"))
dementia_merged[, dem_prev_ratio := dementia_nonstd/dementia_std]

# add state names before saving out
source(file.path("FILEPATH/get_location_metadata.R"))
hierarchy <- get_location_metadata(location_set_id = 35, release_id = 6)
name_dict <- hierarchy[(parent_id==102 | location_id==102), .(location_id, location_name)]
name_dict[location_id==102, location_name:= "USA"]
name_dict[location_id==531, location_name:= "Washington, DC"]

dementia_merged <- merge(dementia_merged, name_dict, by = "location_id")
setnames(dementia_merged,"location_name","State")

fwrite(dementia_merged, paste0(data_dir,"dementia_prev_ratio.csv"))

