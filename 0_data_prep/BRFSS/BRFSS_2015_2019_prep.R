
##########################################################################
### Author: Amy Lastuka
### Date: 09/12/2022
### Project: US dementia spending
### Purpose: calculate total caregiving hours in BRFSS 2015-2019 by
###           state-year, stratified by dementia status
###           Note: prevalence draws are also used for these years. 
###           We bootstrap 1000 draws of BRFSS respondents so for each 
###           of those draws we also select one prevalence draw. That
###           way we are incorporating uncertainty about caregiving hours
###           and about number of prevalent cases.
##########################################################################

rm(list=ls())

library(haven)
library(data.table)
library("cdlTools", lib.loc = "/FILEPATH/rlibs")
library(usmap)
library(ggplot2)
library(foreign)
library(pbapply)
library(Hmisc)

source("FILEPATH/BRFSS_helper_functions.R")
source("FILEPATH/get_population.R")

out_dir <- "FILEPATH"

# step 1: set up NAC data - split into one distribution for each bin
nac_dir <- "FILEPATH"

# set all flags/paramaters
top_code_string = "40"
draws = 1000


collapse_educ=1 # if this flag is 1, collapse education variable to 3 groups
collapse_age=1 # if this flagis 1, collapse age to 7 year bins

# cost_flag ---if this == 0, create HOURS estimates: do not group by any demographics
#              if this == 1, create COST estimates: group by education, age, and sex
cost_flag=0

# if dementia flag == 1, subset to people where CRGVPRB2==5 (main health problem is dementia)
# if dementia flag == 2, subset to people where CRGVALZD==1 (yes, care recipient has Alz/dem)
#   NOTE: only use dementia_flag==2 if the year is 2019, the CRGVALZD question was not in place 
#         prior to 2019
dementia_flag=1

nac_flag=0 # for testing whether to use NAC or BRFSS as the hours distribution 
           # (if nac_flag is 0, then we're using BRFSS)

# NAC - National Alliance for Caregiving, they did a caregiving survey that doesn't have 
# geographic location information, but we use the caregiving hours distribution to help
# convert BRFSS from bins to a point estimate
nac_data <- read_sav(paste0(nac_dir,"Data_2015_CG_in_US_Public_Use_file_FINAL.sav"))
nac_data <- data.table(nac_data)
# filter to care recipients with dementia
nac_data <- nac_data[alzdem==1,]

nac_dist <- list()
nac_dist[[1]] <- nac_data[hours <=8, hours]
nac_dist[[2]] <- nac_data[(hours >=9 & hours <=19), hours]
nac_dist[[3]] <- nac_data[(hours >=20 & hours <=39), hours]
nac_dist[[4]] <- nac_data[(hours >=40), hours]


# early BRFSS data - switching to this instead of NAC data
early_brfss_caregiving_distribution <- fread(paste0("FILEPATH/early_BRFSS_caregiving_distribution_",top_code_string,".csv"))

early_brfss_dist <- list()
early_brfss_dist[[1]] <- early_brfss_caregiving_distribution[weekly_cg_hours <=8, weekly_cg_hours]
early_brfss_dist[[2]] <- early_brfss_caregiving_distribution[(weekly_cg_hours >=9 & weekly_cg_hours <=19), weekly_cg_hours]
early_brfss_dist[[3]] <- early_brfss_caregiving_distribution[(weekly_cg_hours >=20 & weekly_cg_hours <=39), weekly_cg_hours]
early_brfss_dist[[4]] <- early_brfss_caregiving_distribution[(weekly_cg_hours >=40), weekly_cg_hours]


# select NAC or BRFSS distribution based on nac_flag
if(nac_flag==1){
  outlist <-  nac_dist
}else{
  outlist <-  early_brfss_dist
}

### ----- PREP the prevalence draws data --------------
prev_draws_all <- fread("FILEPATH/dementia_prevalence_interpolate_1990_2019.csv")

# age_group_ids:
prev_draws_all <- prev_draws_all[(age_group_id = c(5:20, 30:32, 235)) & (year_id >= 2010),]
age_list <- unique(prev_draws_all$age_group_id)

# pull population for age_list and the years that we need
pops <- get_population(release_id = 6, location_id = -1, year_id = seq(2010,2019), 
                       sex_id=3, age_group_id = 22)[location_id %in% c(102, 523:573)] 

prev_draws_all <- merge(prev_draws_all, pops, by=c("age_group_id", "location_id", "year_id", "sex_id") )
col_list <- paste0("draw_",0:999)

# take weighted mean of each draw over age and sex groups; then create sum of population over age/sex groups
prev_draws_all[, (col_list) := lapply(.SD, function(x) wtd.mean(x, population)), by=c("location_id","year_id"), .SDcols = col_list]
# prev_draws_all[, pop_all := sum(population), by=c("location_id","year_id")]
# 
# prev_draws_all <- prev_draws_all[ , .SD[1], by=c("location_id","year_id")]




### ----- PREP WAGE DATA (for opportunity cost model) --------
# For the variable "pesex", male = 1; female = 2. This matches the coding in BRFSS
cps_data <- fread("FILEPATH/CPS_wages_LFP_2010_21.csv")

# rename to match the BRFSS data
setnames(cps_data, c("gestfips", "pesex", "age", "education"), c("FIPS", "SEX", "AGECAT", "EDUCA"))
#also, brfss_dt doesn't end up having FIPs in it, so convert this to state
cps_data[, state := cdlTools::fips(FIPS,to='Name')]
cps_data[, adj_wage := mean_wage*LFP] 


#### ------ now process BRFSS data - reset data_dir
data_dir <- "FILEPATH"

######### 2019 files
######### STEP 1: create caregiving datatable for each file 
#         note: it's not worth trying to rbind these files since the .dta and .xpt have different variable names
######### STEP 2: process (create mean/bounds)

year=2019

cg_dt <- create_brfss_cg(year, "USA_BRFSS_2019_LLCP_Y2021M03D24.DTA", "dta","_LLCPWT")
cg_dt_v1 <- create_brfss_cg(year,"USA_BRFSS_2019_LLCP_V1_Y2021M01D13.XPT",
                                                               "xpt","X_LCPWTV1") 
cg_dt_v2 <- create_brfss_cg(year,"USA_BRFSS_2019_LLCP_V2_Y2021M01D13.XPT",
                                                               "xpt","X_LCPWTV2")                      
cg_dt_v3 <- create_brfss_cg(year,"USA_BRFSS_2019_LLCP_V3_Y2021M01D13.XPT",
                                                               "xpt","X_LCPWTV3")


brfss_caregiving_dt <- process_cg(cg_dt, cost_flag = cost_flag, prev_draws = prev_draws_all)
brfss_caregiving_dt <- rbind(brfss_caregiving_dt,process_cg(cg_dt_v1, cost_flag = cost_flag, prev_draws = prev_draws_all))
brfss_caregiving_dt <- rbind(brfss_caregiving_dt,process_cg(cg_dt_v2, cost_flag = cost_flag, prev_draws = prev_draws_all))
brfss_caregiving_dt <- rbind(brfss_caregiving_dt,process_cg(cg_dt_v3, cost_flag = cost_flag, prev_draws = prev_draws_all))




### for creating scale-factor ##########
#### re-run 2019 with dementia_flag==2 
if(cost_flag==0){
  dementia_flag <- 2
  brfss_caregiving_alzdem <- process_cg(cg_dt, cost_flag = cost_flag, prev_draws = prev_draws_all)
  brfss_caregiving_alzdem <- rbind(brfss_caregiving_alzdem,process_cg(cg_dt_v1, cost_flag = cost_flag, prev_draws = prev_draws_all))
  brfss_caregiving_alzdem <- rbind(brfss_caregiving_alzdem,process_cg(cg_dt_v2, cost_flag = cost_flag, prev_draws = prev_draws_all))
  brfss_caregiving_alzdem <- rbind(brfss_caregiving_alzdem,process_cg(cg_dt_v3, cost_flag = cost_flag, prev_draws = prev_draws_all))

  scale_factor_sample_size <- brfss_caregiving_dt$sample_size/brfss_caregiving_alzdem$sample_size
  scale_factor_care_hours <- brfss_caregiving_dt$cg_per_case /brfss_caregiving_alzdem$cg_per_case 
  
  # create a table with the caregiving hours with both options for the dementia question
}


############
dementia_flag <- 1

# 2018 files
year=2018

cg_dt <- create_brfss_cg(year,"USA_BRFSS_2018_LLCP18_Y2019M10D30.DTA", "dta","_LLCPWT")
cg_dt_v1 <- create_brfss_cg(year,"USA_BRFSS_2018_LLCP18V1_Y2019M10D30.XPT", "xpt","X_LCPWTV1")                        
cg_dt_v2 <- create_brfss_cg(year,"USA_BRFSS_2018_LLCP18V2_Y2019M10D30.XPT", "xpt","X_LCPWTV2") 

brfss_caregiving_dt <- rbind(brfss_caregiving_dt,process_cg(cg_dt, cost_flag = cost_flag, prev_draws = prev_draws_all))
brfss_caregiving_dt <- rbind(brfss_caregiving_dt,process_cg(cg_dt_v1, cost_flag = cost_flag, prev_draws = prev_draws_all))
brfss_caregiving_dt <- rbind(brfss_caregiving_dt,process_cg(cg_dt_v2, cost_flag = cost_flag, prev_draws = prev_draws_all))


# 2017 files
year=2017

cg_dt <- create_brfss_cg(year,"USA_BRFSS_2017_LLCP2017_Y2018M10D23.DTA", "dta","_LLCPWT")

cg_dt_v1 <- create_brfss_cg(year,"USA_BRFSS_2017_LLCP17V1_Y2018M10D23.XPT", "xpt","X_LCPWTV1")                        
cg_dt_v2 <- create_brfss_cg(year,"USA_BRFSS_2017_LLCP17V2_Y2018M10D23.XPT", "xpt","X_LCPWTV2")                        
cg_dt_v3 <- create_brfss_cg(year,"USA_BRFSS_2017_LLCP17V3_Y2018M10D23.XPT", "xpt","X_LCPWTV3")  

brfss_caregiving_dt <- rbind(brfss_caregiving_dt,process_cg(cg_dt, cost_flag = cost_flag, prev_draws = prev_draws_all))
brfss_caregiving_dt <- rbind(brfss_caregiving_dt,process_cg(cg_dt_v1, cost_flag = cost_flag, prev_draws = prev_draws_all))
brfss_caregiving_dt <- rbind(brfss_caregiving_dt,process_cg(cg_dt_v2, cost_flag = cost_flag, prev_draws = prev_draws_all))
brfss_caregiving_dt <- rbind(brfss_caregiving_dt,process_cg(cg_dt_v3, cost_flag = cost_flag, prev_draws = prev_draws_all))

# 2016 files
year=2016

cg_dt <- create_brfss_cg(year,"USA_BRFSS_2016_Y2018M01D23.DTA", "dta","_LLCPWT")

cg_dt_v1 <- create_brfss_cg(year,"USA_BRFSS_2016_LLCP_V1_Y2022M06D01.XPT", "xpt","X_LCPWTV1")                        
cg_dt_v2 <- create_brfss_cg(year,"USA_BRFSS_2016_LLCP_V2_Y2022M05D24.XPT", "xpt","X_LCPWTV2")                        
cg_dt_v3 <- create_brfss_cg(year,"USA_BRFSS_2016_LLCP_V3_Y2022M05D24.XPT", "xpt","X_LCPWTV3")

brfss_caregiving_dt <- rbind(brfss_caregiving_dt,process_cg(cg_dt, cost_flag = cost_flag, prev_draws = prev_draws_all))
brfss_caregiving_dt <- rbind(brfss_caregiving_dt,process_cg(cg_dt_v1, cost_flag = cost_flag, prev_draws = prev_draws_all))
brfss_caregiving_dt <- rbind(brfss_caregiving_dt,process_cg(cg_dt_v2, cost_flag = cost_flag, prev_draws = prev_draws_all))
brfss_caregiving_dt <- rbind(brfss_caregiving_dt,process_cg(cg_dt_v3, cost_flag = cost_flag, prev_draws = prev_draws_all))

# 2015 files
year=2015

cg_dt <- create_brfss_cg(year,"USA_BRFSS_2015_LANDLINE_AND_CELL_Y2016M09D07.DTA", "dta","_LLCPWT")

cg_dt_v1 <- create_brfss_cg(year,"USA_BRFSS_2015_LANDLINE_AND_CELL_QUEST_V1_Y2016M09D07.XPT", "xpt","X_LCPWTV1")                        
cg_dt_v2 <- create_brfss_cg(year,"USA_BRFSS_2015_LANDLINE_AND_CELL_QUEST_V2_Y2016M09D07.XPT", "xpt","X_LCPWTV2")                       
cg_dt_v3 <- create_brfss_cg(year,"USA_BRFSS_2015_LANDLINE_AND_CELL_QUEST_V3_Y2016M09D07.XPT", "xpt","X_LCPWTV3")  

###### need to remove duplicates in V1 and V2 - V3 does not have any caregiving data
# cg_dt_v1 <- cg_dt_v1[! cg_dt_v1$SEQNO %in% cg_dt$SEQNO]
# cg_dt_v2 <- cg_dt_v2[! cg_dt_v2$SEQNO %in% cg_dt$SEQNO]

cg_dt_v1 <- cg_dt_v1[! cg_dt_v1$FIPS %in% cg_dt$FIPS]
cg_dt_v2 <- cg_dt_v2[! cg_dt_v2$FIPS %in% cg_dt$FIPS]

brfss_caregiving_dt <- rbind(brfss_caregiving_dt,process_cg(cg_dt, cost_flag = cost_flag, prev_draws = prev_draws_all))
brfss_caregiving_dt <- rbind(brfss_caregiving_dt,process_cg(cg_dt_v1, cost_flag = cost_flag, prev_draws = prev_draws_all))
brfss_caregiving_dt <- rbind(brfss_caregiving_dt,process_cg(cg_dt_v2, cost_flag = cost_flag, prev_draws = prev_draws_all))

# save out brfss_caregiving_dt here b/c the lines above take close to an hour to run
#fwrite(brfss_caregiving_dt, paste0(out_dir, "brfss_caregiving_dt_2015_2019_unscaled.csv"))

# read it back in to pick up where I left off
# brfss_caregiving_dt <- fread(paste0(out_dir, "brfss_caregiving_dt_2015_2019_unscaled.csv"))


# 2012/2013 files ################################################
#
# Note: the BRFSS files from 2012 and 2013 were using an older version of the caregiving module
#       in this module, respondents provide an exact number of caregiving hours per week, therefore
#       we don't have to sample from NAC. Because the processing is so different I process
#       these files in the script BRFSS_2012_prep.R

# merge in the 2012 and 2013 data
if(cost_flag==0){
  older_data <- fread(paste0('FILEPATH/BRFSS_caregiving_hours_2012_2013_draw_space_',top_code_string,'.csv'))
  
  
  # rename to match brfss_caregiving_dt
  #setnames(older_data, c("cg_sum","cg_lower","cg_upper","cg_var"),c("cg_per_case","lower_per_case","upper_per_case","cg_var_per_case") )
  brfss_caregiving_dt <- rbind(brfss_caregiving_dt, older_data)

  # use scale factor to adjust BRFSS data to include all dementia patients (Don't apply this to NHATS data!)
  # add scale factor to the dataframe
  scale_factor <- mean(scale_factor_care_hours)
  reported_scale_factor <- 1/scale_factor
  reported_scale_factor <- data.frame(reported_scale_factor)
  # write out this scale factor so we have it for number-plugging later
  #fwrite(reported_scale_factor,"FILEPATH/BRFSS_dem_scale_factor.csv")
  brfss_caregiving_dt[, cg_per_case := cg_per_case *(1/scale_factor)]
  brfss_caregiving_dt[, lower_per_case  := lower_per_case  *(1/scale_factor)]
  brfss_caregiving_dt[, upper_per_case := upper_per_case *(1/scale_factor)]
}else{
  # the caregiving hour scale factor isn't needed since I would be scaling up 
  # each demographic by the same percentage so, it would not affect the weighted average wage
  brfss_2012 <- fread(paste0("FILEPATH/BRFSS_caregiving_costs_2012_2013_",top_code_string,".csv"))
  
  brfss_caregiving_dt <- rbind(brfss_caregiving_dt, brfss_2012)
  fwrite(brfss_caregiving_dt, paste0(out_dir,"dementia_caregiving_total_cost_",top_code_string,".csv"))

}

# only write out this file if cost_flag is 0 (not splitting out by demographic bins)
if(cost_flag == 0){
  if(dementia_flag==1){
    #write.csv(brfss_caregiving_dt, "~/alzheimers/cg_hours_dementia_patients_by_state_year_SUM.csv")
    write.csv(brfss_caregiving_dt, paste0(out_dir,"cg_hours_dementia_patients_by_state_year_envelope_",top_code_string,".csv"))
  }else{
    #write.csv(brfss_caregiving_dt, "~/alzheimers/cg_hours_nondem_patients_by_state_year_SUM_",top_code_string,".csv")
  }
}

