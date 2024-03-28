##########################################################################
### Author: Amy Lastuka
### Date: 06/14/2023
### Project: US dementia spending
### Purpose: number plug methods section
##########################################################################

rm(list=ls())

library(data.table)

top_code <- 112 # this model has more rows than the 168 hour model

out_dir <- "FILEPATH"

#### Sentence 1: We collected data from XX state-years from 2012 through 2019 

brfss_caregiving_dt <- fread(paste0(out_dir,"cg_hours_dementia_patients_by_state_year_envelope_",top_code,".csv"))

print(paste0("We collected data from ",nrow(brfss_caregiving_dt),
             " state-years from 2012 through 2019"))

#### Sentence 2: On average, across all X states that administered the caregiver module in 2019

num2019 <- brfss_caregiving_dt[year==2019,]

print(paste0("On average, across all ",nrow(num2019),
             " states that administered the caregiver module in 2019"))

### ----- Sentence 3: using the more inclusive question led to an estimate of XX times more caregivers
scale_factor <- fread(paste0(FILEPATH,"BRFSS_dem_scale_factor.csv"))

print(paste0("using the more inclusive question led to an estimate of ", 
             scale_factor$reported_scale_factor," times more caregivers"))

