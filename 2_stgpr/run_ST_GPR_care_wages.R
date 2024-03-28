##########################################################################
### Author: Michael Breshock
### Date: 01/24/2023
### Project: US dementia spending
### Purpose: run ST-GPR for forgone wages due to caregiving
###          (used for the opportunity cost method)
##########################################################################
# Clean the environment 

rm(list=ls()) 


library(data.table)
st_gpr_dir <- paste0(FILEPATH,"care_wages/")

# Register and run a model 
source('FILEPATH/stgpr/api/public.R')


# Specify cluster arguments for launch
cluster_project = "proj_fgh" # "ihme_general"
nparallel = 50 #number of parallelizations! More parallelizations --> faster (if cluster is emtpy). I usually do 50-100.
slots = 5 #number of slots for each ST or GP job. You can profile by looking at a specific ST job through qacct.
logs <- 'FILEPATH'
holdouts = 0 # Keep at 0 unless you want to run cross-validation
draws = 100 #Either 0 or 1000 - run with 0 for test runs, 1000 for upload-ready runs
model_num = 15 # update this to be the model_index_id you want to use from the config file
keep_outliers = T # indicate whether or not this is a run with or without outliers 

# Register an ST-GPR model using a config for non-decom run. 

run_id <- register_stgpr_model(paste0(st_gpr_dir,'brfss_us_st_gpr_config_file.csv'), model_index_id = model_num)
run_id


# Submit ST-GPR model for your newly-created run_id!
stgpr_sendoff(run_id, cluster_project,
              nparallel = nparallel,
              log_path = logs)

# check model's status
get_model_status(version_id = run_id)

# continuously check ST-GPR status until it is done: 
stgpr_status = get_model_status(version_id = run_id)
while(stgpr_status == 2){
  stgpr_status = get_model_status(version_id = run_id)
  Sys.sleep(30) # wait 30 seconds in between checks 
}

# read_draws
## list all files in results directory
results_dir <- paste0("/ihme/covariates/ubcov/model/output/", run_id, "/draws_temp_0/")
files <- list.files(results_dir)

## function to read, melt, and select
read_stgpr_draws <- function(x){
  draw <- fread(x)
  draw <- melt(draw, id.vars = c("location_id", "year_id", "age_group_id", "sex_id"))
  draw <- draw[, .(location_id, year_id, variable, value)]
  return(draw)
}

## read, melt, select from, and bind all results
results <- rbindlist(lapply(paste0(results_dir, files), read_stgpr_draws))

fwrite(results,
       paste0('FILEPATH',
              model_num, 
              ifelse(keep_outliers, "_keep_outliers", ""),
              '.csv'), 
       row.names = FALSE)

# Reading outputs from models
dx_rates_stgpr <- get_estimates(run_id, 'gpr')

fwrite(dx_rates_stgpr,
       paste0('FILEPATH',
              model_num,
              ifelse(keep_outliers, "_keep_outliers", ""),
              '.csv'),
       row.names = FALSE)

