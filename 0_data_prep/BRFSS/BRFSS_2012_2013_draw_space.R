##########################################################################
### Author: Amy Lastuka
### Date: 02/24/2023
### Project: US dementia spending
### Purpose: put BRFSS caregiving estimates into per-case units 
###          using prevalence estimates in draw space 
#            Note: originally, the BRFSS 2012-2013 caregiving hour
#                  estimates were not in draw space, but we decided
#                  to use draws of prevalence and the way BRFSS works, we 
#                  get total caregiving hours for the state. We don't have
#                  an estimate of the # of people with dementia from BRFSS
#                  (in contrast to HRS and NHATS, which do have info that 
#                  allows us to create prevalence estimates). Therefore, we 
#                  divide by each of the prevalence estimates (from GBD) to
#                  end up with 1000 different estimates of hours/case.
##########################################################################

rm(list=ls())

library(haven)
library(data.table)
library(readxl)
library("cdlTools", lib.loc = "/FILEPATH/rlibs")
library(usmap)
library(ggplot2)
library(foreign)
library(pbapply)
library(Hmisc)

source("FILEPATH/get_population.R")
source(file.path("FILEPATH/get_location_metadata.R"))
hierarchy <- get_location_metadata(location_set_id = 35, release_id = 6)
name_dict <- hierarchy[parent_id==102, .(location_id, location_name)]

draws = 1000

l_path <- "FILEPATH"
data_dir <- "FILEPATH"
out_dir <- "FILEPATH"

# specify parameters
top_code = 40 #112 #16*7
top_code_string = as.character(top_code)

# read in the total hours estimates for the 2012-2013 data
brfss_2012 <- fread(paste0("FILEPATH/BRFSS_caregiving_hours_2012_2013_",top_code_string,".csv"))



### ----- PREP the prevalence draws data --------------
prev_draws_all <- fread("FILEPATH/dementia_prevalence_interpolate_1990_2019.csv")

# age_group_ids: 
prev_draws_all <- prev_draws_all[(age_group_id = c(5:20, 30:32, 235)) & (year_id >= 2010),]
# age_list <- unique(prev_draws_all$age_group_id)

# pull population for age_list and the years that we need
pops <- get_population(release_id = 6, location_id = -1, year_id = seq(2010,2019), 
                       sex_id=3, age_group_id = 22)[location_id %in% c(102, 523:573)]

prev_draws_all <- merge(prev_draws_all, pops, by=c("age_group_id", "location_id", "year_id", "sex_id") )
col_list <- paste0("draw_",0:999)

# for each state-year, create 1000 per-case estimates
brfss_out <- data.table(cg_per_case = as.numeric(), 
                        cg_var_per_case = as.numeric(), 
                        lower_per_case = as.numeric(), 
                        upper_per_case = as.numeric(), 
                        state = as.character(), 
                        year = as.numeric(), 
                        sample_size = as.character())

for(i in 1:nrow(brfss_2012)){
  year <- brfss_2012$year[i]
  st <- brfss_2012$state[i]
  print(paste0("State = ",st," year = ",year))
   # melt prev_row -> prev_melt is 'long' so there's a column called 'variable' 
   # which is "draw_X" and a column called 'value' which is the 
   # prevalence rate for that draw
  prev_row <- prev_draws_all[(year_id==year) & (location_id==name_dict[location_name==st]$location_id),]
  col_list <- paste0("draw_",0:999)
  prev_melt = melt(prev_row , id.vars = c("location_id", "age_group_id","sex_id","year_id","population"),
                   measure.vars = col_list)
  prev_melt[, dem_cases := value*population]

  
  # now do the draw-space loop  here 
  hours_list <- list()
  for( draw_num in 0:999){
    draw_string <- paste0("draw_",draw_num)
    prevalence_draw <- prev_melt$dem_cases[prev_melt$variable == draw_string]
    cg_metric <- brfss_2012[i]$cg_sum/prevalence_draw
    print(paste0("cg_metric = ",cg_metric))
    hours_list <- c(hours_list,cg_metric)
  }
  
  hours_estimates <- unlist(hours_list)
  brfss_out_temp <- brfss_2012[i][,c("state","year", "sample_size")]
  brfss_out_temp$cg_per_case <- mean(hours_estimates)
  brfss_out_temp$lower_per_case <-  quantile(hours_estimates, probs = c(0.025))
  brfss_out_temp$upper_per_case <-  quantile(hours_estimates, probs = c(0.975))
  
  st_error <- (brfss_out_temp$upper_per_case - brfss_out_temp$lower_per_case)/(1.96*2)
  brfss_out_temp$cg_var_per_case <- draws*st_error^2
  
  if(nrow(brfss_out) == 0){
    
    brfss_out <- brfss_out_temp
    #print(paste0("number of rows in brfss_out ",nrow(brfss_out)))
  }else{
    brfss_out <- rbind(brfss_out, brfss_out_temp)
    #print(paste0("number of rows in brfss_out ",nrow(brfss_out)))
  }
}

fwrite(brfss_out,paste0('FILEPATH/BRFSS_caregiving_hours_2012_2013_draw_space_',top_code_string,'.csv'))

