##########################################################################
### Author: Amy Lastuka
### Date: 08/29/2022
### Project: US dementia spending
### Purpose: open/clean BRFSS 2012/2013 datasets
#   Description: these files need to be processed separately from the 
#                2015+ files because the caregiving module was not
#                included with the main BRFSS release prior to 2015
#                The variables names are not consistent between states,
#                and the caregiving hours are reported as any number 
#                from 0-168 instead of in buckets, therefore the 
#                sampling/bootstrapping process that is used for the 2015+
#                data is not needed here.
#   Outputs: 
#           - a csv file with all of the raw caregiving hours from these
#             files is saved. This is an INPUT to BRFSS_sum_hours.R (used as an
#             hours distribution to draw from). The dataframe for this is called
#             early_brfss_caregiving_distribution.
#           - TOTAL caregiving hours estimates for the 2012/2013 states. This file outputs
#             "cg_sum" which is a sum of hours, that data is then passed to
#             BRFSS_2012_draw_space.R where the sum is divided by 1000 different
#             prevalence estimates, to get 1000 hours/case estimates. 
#           - caregiving costs estimates for the 2012/2013 states. This is an
#             input to brfss_opportunity_cost_model.R
##########################################################################

rm(list=ls())

library(haven)
library(data.table)
library(readxl)
library("cdlTools", lib.loc = "/FILEPATH/rlibs")
library("tigris", lib.loc = "/FILEPATH/rlibs")
library(usmap)
library(ggplot2)
library(foreign)
library(pbapply)
library("Hmisc", lib.loc = "/FILEPATH/rlibs")


l_path <- "FILEPATH"
iowa_dir <- "FILEPATH"
data_dir <- "FILEPATH"
out_dir <- "FILEPATH"

# specify parameters
top_code = 40     # 168 (24*7); 112 (16*7); or 40 (used for the forgone wage model)
top_code_string = as.character(top_code)

########################################################
#### code that is common for all state-years
########################################################

# function that takes the brfss_cg dataframe and bootstraps to come up with upper/lower/mean for cost per hour
# cps_data needs to be loaded before calling this function
cost_bootstrap <- function(cg_datatable, ndraws=100){
  
  cost_estimate_list <- list()
  
  for(i in 1:ndraws){
    ids <- cg_datatable[, unique(SEQNO)]
    sample_list <- data.table(SEQNO = sample(ids, replace = T))
    cg_resampled <- merge(sample_list, cg_datatable, by = "SEQNO", all.x = T, sort = F, allow.cartesian = T)
    
    # need year and state to merge with CPS data
    cg_temp <- cg_resampled[, .(cg_sum = sum(wtd_hrs), year = mean(year), state = cg_resampled$state[1], draw = i), by=c("EDUCA","AGECAT","SEX")]
    
    # merge in the CPS data 
    cg_temp <- merge(cg_temp, cps_data, by = c("state", "SEX", "AGECAT", "EDUCA","year"))
    cg_temp[, cg_cost := cg_sum*adj_wage]
    
    # sum up cost over all demographic groups 
    cg_temp <- cg_temp[,.(total_cost=sum(cg_cost,na.rm=T),total_hours=sum(cg_sum,na.rm=T)), by=c("state","year")]
    cg_temp[, avg_cost_per_hour:= total_cost/total_hours]
    
    # append the avg_cost_per_hour to the end of cost estimate list
    cost_estimate_list <- c(cost_estimate_list, cg_temp$avg_cost_per_hour)
    #print(cg_temp)
  }
  
  cost_estimates <- unlist(cost_estimate_list)
  bootstrap_mean <- mean(cost_estimates)
  bootstrap_lower <- quantile(cost_estimates, probs = c(0.025))
  bootstrap_upper <- quantile(cost_estimates, probs = c(0.975))
  bootstrap_var <- var(cost_estimates)
  
  
  #put mean/upper/lower into a dataframe and then return it
  cost_output <- data.table(cost_mean = bootstrap_mean, cost_lower = bootstrap_lower, cost_upper = bootstrap_upper, cost_var = bootstrap_var, year = mean(cg_resampled$year), state = cg_resampled$state[1], sample_size = nrow(cg_datatable) )
  
}

# function that collapses age and education to the categories that we want to use
# inputs needed are age in 5 year buckets, education, and sex
brfss_demos <- function(brfss_data, age_var="AGE5YR", edu_var="EDUCA",sex_var="SEX"){
  #print(nrow(brfss_data[eval(age_var) >5,]))
  # COLLAPSE AGE INTO 10 YEAR BINS
  brfss_data[get(age_var) <=3, age_tmp := "18-34"]
  brfss_data[(get(age_var) >3) & (get(age_var) <=5), age_tmp := "35-44"]
  brfss_data[(get(age_var) >5) & (get(age_var) <=7), age_tmp := "45-54"]
  brfss_data[(get(age_var) >7) & (get(age_var) <=9), age_tmp := "55-64"]
  brfss_data[(get(age_var) >9) & (get(age_var) <=11), age_tmp := "65-74"]
  brfss_data[(get(age_var) >11) & (get(age_var) <=13), age_tmp := "75+"]
  brfss_data[get(age_var)==14, age_tmp := NA]
  brfss_data$AGE5YR <- brfss_data$age_tmp
  brfss_data <- brfss_data[!is.na(AGE5YR),]
  #create new name because it's not a 5 year increment anymore
  brfss_data[, AGECAT := AGE5YR]
  # remove AGE5YR so I don't use it by accident
  brfss_data[, AGE5YR:=NULL]
  print(nrow(brfss_data))
  
  # COLLAPSE EDUCATION INTO 3 CATEGORIES
  brfss_data[(get(edu_var) <=4), educ := "High school or less"]
  brfss_data[(get(edu_var) ==5), educ := "Some college"]
  brfss_data[(get(edu_var)==6), educ := "Bachelor's degree or more"]
  brfss_data[(get(edu_var)==9), educ := NA]
  brfss_data$EDUCA <- brfss_data$educ
  brfss_data <- brfss_data[!is.na(EDUCA),]
 
  # remove if missing sex variable
  brfss_data <- brfss_data[(get(sex_var)==1) | (get(sex_var)==2),]
  return(brfss_data)
}


# caregiver hours prep function, since these steps are the same for every state
cg_prep <- function(state_dt){
  care_dt <- state_dt[(CAREGIV1==1) | (CAREGIV1==7)]
  # also subset to dementia carers
  care_dt <- care_dt[CRGVPRB2==16,]
  care_dt[CRGVHRS1==777, CRGVHRS1 := NA]
  care_dt[CRGVHRS1==999, CRGVHRS1 := NA]
  
  # top code hours 
  care_dt[CRGVHRS1 > top_code, CRGVHRS1:=top_code]
  return(care_dt)
}

# first create FIPS dictionary 
fips_lookup <- tigris::fips_codes
setDT(fips_lookup)
fips_lookup <- fips_lookup[, .SD[1], by=state_name]
fips_lookup <- fips_lookup[, .(state = state_name, FIPS = state_code)]
fips_lookup$FIPS <- as.numeric(fips_lookup$FIPS)
# load CPS data
cps_data <- fread("FILEPATH/CPS_wages_LFP_2010_21.csv")
setnames(cps_data, c("gestfips", "pesex", "age", "education"), c("FIPS", "SEX", "AGECAT", "EDUCA"))
cps_data <- merge(cps_data, fips_lookup)
#cps_data[, state := cdlTools::fips(FIPS,to='Name')]
cps_data[, adj_wage := mean_wage*LFP]

# create an empty dataframe to store all of the caregiving hours. This will be the distribution I draw from for BRFSS 2015+.
early_brfss_caregiving_distribution <- data.table(weekly_cg_hours = as.numeric(), state = as.character(), year = as.numeric())


####################### 2012 #########################################
year = 2012

#######################################################
#####        INDIANA - 2012
#######################################################

cg_filename <- 'USA_IN_BRFSS_2012_CAREGIVER_Y2022M08D19.CSV' 
brfss_2012_filename <- 'USA_BRFSS_2012_LANDLINE_AND_CELL_Y2013M07D24.DTA'

brfss_cg_module <- fread(paste0(data_dir,year,"/",cg_filename))

brfss_data <- read_dta(paste0(data_dir,year,"/",brfss_2012_filename))
names(brfss_data) <- toupper(names(brfss_data))

# merge INDIANA caregiver module
# NOTE: first cut to only people in Indiana, because they re-use SEQNO across states
brfss_data <- data.table(brfss_data)
brfss_indiana <- brfss_data[A_STATE==18,]
brfss_indiana <- merge(brfss_indiana, brfss_cg_module, by="SEQNO")

# set names for caregiving module to match the 2015+ names
# Q35f = CAREGIV1
setnames(brfss_indiana,c("Q35f","Q36f","Q37f","Q38f","Q39f","Q40f","Q42f"), 
         c("CAREGIV1","AGE_CR","SEX_CR","CRGVREL3","CRGVLNG1","CRGVPRB2","CRGVHRS1"))

#histogram(brfss_indiana$CRGVHRS1[brfss_indiana$CRGVHRS1 < 200])

brfss_cg <- cg_prep(brfss_indiana)
# brfss_cg <- brfss_indiana[(CAREGIV1==1) | (CAREGIV1==7)]
# # also subset to dementia carers
# brfss_cg <- brfss_cg[CRGVPRB2==16,]
# brfss_cg[CRGVHRS1==777, CRGVHRS1 := NA]
# brfss_cg[CRGVHRS1==999, CRGVHRS1 := NA]

# for caregiving hours, replace 'don't know' with average value 
avg_temp <- weighted.mean(brfss_cg$CRGVHRS1,brfss_cg$A_LLCPWT, na.rm=TRUE)
brfss_cg$CRGVHRS1 <- as.double(brfss_cg$CRGVHRS1)
brfss_cg[is.na(CRGVHRS1), CRGVHRS1 := avg_temp]

# note: I know the state is Indiana so hard-coding to skip the FIPS step
brfss_cg$state <- "Indiana"
setnames(brfss_cg, c("A_AGEG5YR"), c("AGE5YR"))

###### calculate average and sum of caregiving hours for everyone, not segmented by demographics
avg_cg_hours <- weighted.mean(brfss_cg$CRGVHRS1, brfss_cg$A_LLCPWT, na.rm=TRUE)
brfss_cg[, wtd_hrs := A_LLCPWT*CRGVHRS1]
total_cg_hours <- sum(brfss_cg$wtd_hrs, na.rm=TRUE)

# create table of caregiving hours by race 1 = white; 5 = American indian
race_dt <- brfss_cg[, .(hours_by_race  = weighted.mean(CRGVHRS1,A_LLCPWT, na.rm=TRUE), sample_size = .N, state = "Indiana"), by = "RACE2"]

# Create data table for final output 
#This is the first state I process, so I create the datatable brfss_final_RC (replacement cost). Subsequent states use a temp DT and then rbind to this one.
brfss_final_RC <- data.table()
brfss_final_RC$cg_mean <- avg_cg_hours
brfss_final_RC$cg_sum <- total_cg_hours

num_obs <- sum(brfss_cg$A_LLCPWT)

# upper and lower bounds - for SUM not for MEAN, multiply bounds by number of observations
wt_var <- wtd.var(brfss_cg$CRGVHRS1, brfss_cg$A_LLCPWT, na.rm =TRUE)
st_error_IN <- sqrt(wt_var)/sqrt(length((brfss_cg$CRGVHRS1)))
brfss_final_RC$cg_lower <- num_obs*(brfss_final_RC$cg_mean - 1.96*st_error_IN)
brfss_final_RC$cg_upper <- num_obs*(brfss_final_RC$cg_mean + 1.96*st_error_IN)
brfss_final_RC$state <- "Indiana"
brfss_final_RC$year <- 2012
brfss_final_RC$sample_size <- nrow(brfss_cg)

# add variance, we will use this in ST-GPR
brfss_final_RC$cg_var <- wt_var

##### Opportunity cost method:
#####   After creating full estimates, do everything by demographic category 

###### clean up demographic categories ######
brfss_cg <- brfss_demos(brfss_cg)
brfss_cg$year <- as.numeric(brfss_cg$IYEAR)

# for each state, append info to this data.table
early_brfss_caregiving_distribution <- data.table(weekly_cg_hours = brfss_cg$CRGVHRS1, state = brfss_cg$state, year = brfss_cg$year)

# This function resamplse and then creates 100 avg_cost_per_hour estimates.
# It returns a dataframe that has the lower bound, upper bound, and mean for cost per hour.
#This is the first state I process, so I named the datatable brfss_final_OC (opportunity cost). Subsequent states use a temp DT and then rbind to this one.
brfss_final_OC <- cost_bootstrap(brfss_cg)


# END of Indiana 2012 processing

#######################################################
#####        OREGON - 2012
#######################################################

# NOTE: the weight names are different for Oregon (I rename them below for consistency). Documentation for the weights is here: 
# "L:\LIMITED_USE\PROJECT_FOLDERS\USA\BRFSS\OR\2012\BRFSS_OREGON_2012_RAKED_WEIGHTS_INFO_Y2021M07D08.DOCX"

rm(brfss_cg)
filename <- "BRFSS_OREGON_2012_Y2021M07D08.SAV"

brfss_oregon <- read_sav(paste0(l_path,year,"/",filename))
brfss_oregon <- data.table(brfss_oregon)

setnames(brfss_oregon,c("CAREMONTH","CAREAGE","CARESEX","CAREREL","CARELONG","CAREWHAT","CAREHRS","wtrk_C_c"), 
         c("CAREGIV1","AGE_CR","SEX_CR","CRGVREL3","CRGVLNG1","CRGVPRB2","CRGVHRS1","A_LLCPWT"))

brfss_oregon$CRGVHRS1 <- as.double(brfss_oregon$CRGVHRS1)
brfss_cg <- cg_prep(brfss_oregon)
# for caregiving hours, replace 'don't know' with average value 
# brfss_cg <- brfss_oregon[(CAREGIV1==1) | (CAREGIV1==7)]
# brfss_cg$CRGVHRS1 <- as.double(brfss_cg$CRGVHRS1)
# 
# 
# # also subset to dementia carers
# brfss_cg <- brfss_cg[CRGVPRB2==16,]
# brfss_cg[CRGVHRS1==777, CRGVHRS1 := NA]
# brfss_cg[CRGVHRS1==999, CRGVHRS1 := NA]

avg_temp <- weighted.mean(brfss_cg$CRGVHRS1, brfss_cg$A_LLCPWT, na.rm=TRUE)
brfss_cg$CRGVHRS1 <- as.double(brfss_cg$CRGVHRS1)
brfss_cg[is.na(CRGVHRS1), CRGVHRS1 := avg_temp]
avg_cg_hours <- weighted.mean(brfss_cg$CRGVHRS1, brfss_cg$A_LLCPWT)

brfss_cg[, wtd_hrs := A_LLCPWT*CRGVHRS1]
total_cg_hours <- sum(brfss_cg$wtd_hrs, na.rm=TRUE)
brfss_cg$cg_sum <- total_cg_hours


brfss_cg$state <- "Oregon"
# Note: there is a variable called AGEGRP in here but it is almost all NAs
#       need to use AGE and categorize it myself
# COLLAPSE AGE INTO 10 YEAR BINS
brfss_cg[(AGE >=18) & (AGE < 35), AGECAT := "18-34"]
brfss_cg[(AGE >=35) & (AGE < 45), AGECAT := "35-44"]
brfss_cg[(AGE >=45) & (AGE < 55), AGECAT := "45-54"]
brfss_cg[(AGE >=55) & (AGE < 65), AGECAT := "55-64"]
brfss_cg[(AGE >=65) & (AGE < 75), AGECAT := "65-74"]
brfss_cg[(AGE >=75), AGECAT := "75+"]
brfss_cg <- brfss_cg[!is.na(AGE),]

# COLLAPSE EDUCATION INTO 3 CATEGORIES
brfss_cg[EDUCA <=4, educ := "High school or less"]
brfss_cg[EDUCA ==5, educ := "Some college"]
brfss_cg[EDUCA==6, educ := "Bachelor's degree or more"]
brfss_cg[EDUCA==9, educ := NA]
brfss_cg$EDUCA <- brfss_cg$educ
brfss_cg <- brfss_cg[!is.na(EDUCA),]


brfss_dt <- data.table()
brfss_dt$cg_mean <- avg_cg_hours
brfss_dt$cg_sum <- total_cg_hours


num_obs <- sum(brfss_cg$A_LLCPWT)

# upper and lower bounds
wt_var <- wtd.var(brfss_cg$CRGVHRS1, brfss_cg$A_LLCPWT, na.rm =TRUE)
st_error_OR <- sqrt(wt_var)/sqrt(length((brfss_cg$CRGVHRS1)))
brfss_dt$cg_lower <- num_obs*(brfss_dt$cg_mean - 1.96*st_error_OR)
brfss_dt$cg_upper <- num_obs*(brfss_dt$cg_mean + 1.96*st_error_OR)
brfss_dt$state <- "Oregon"
brfss_dt$year <- 2012
brfss_dt$sample_size <- nrow(brfss_cg)

# add variance, we will use this in ST-GPR
brfss_dt$cg_var <- wt_var

brfss_final_RC <- rbind(brfss_final_RC, brfss_dt)

brfss_cg[, RACE2:=NULL]
setnames(brfss_cg, "racesum1", "RACE2")
brfss_cg$RACE2 <- as.numeric(brfss_cg$RACE2)
race_temp <- brfss_cg[, .(hours_by_race  = weighted.mean(CRGVHRS1,A_LLCPWT, na.rm=TRUE),sample_size = .N,  state = "Oregon"), by = "RACE2"]
race_dt <- rbind(race_dt, race_temp)


###### opportunity cost method - demographic variables are already binned
brfss_cg$year <- as.numeric(brfss_cg$IYEAR)

early_brfss_temp_dist <- data.table(weekly_cg_hours = brfss_cg$CRGVHRS1, state = brfss_cg$state, year = brfss_cg$year)
early_brfss_caregiving_distribution <- rbind(early_brfss_caregiving_distribution,early_brfss_temp_dist)


brfss_final_temp <- cost_bootstrap(brfss_cg)
brfss_final_OC <- rbind(brfss_final_OC, brfss_final_temp)

# END of Oregon 2012 processing

#######################################################
#####        MISSOURI - 2012
#######################################################
rm(brfss_dt, brfss_cg)

cg_filename <- 'USA_MO_BRFSS_2012_CAREGIVER_Y2022M11D02.CSV'
brfss_2012_filename <- 'USA_BRFSS_2012_LANDLINE_AND_CELL_Y2013M07D24.DTA'

brfss_missouri <- fread(paste0(data_dir,year,"/",cg_filename))
names(brfss_missouri) <- toupper(names(brfss_missouri))

# set names for caregiving module to match the 2015+ names
# Q35f = CAREGIV1
setnames(brfss_missouri,c("SACM1","SACM2","SACM3","SACM4","SACM5","SACM6","SACM8"), 
         c("CAREGIV1","AGE_CR","SEX_CR","CRGVREL3","CRGVLNG1","CRGVPRB2","CRGVHRS1"))
setnames(brfss_missouri, c("_AGEG5YR","_LLCPWT"), c("AGE5YR","A_LLCPWT"))

brfss_cg <- cg_prep(brfss_missouri)

# brfss_cg <- brfss_missouri[(CAREGIV1==1) | (CAREGIV1==7)]
# # also subset to dementia carers
# brfss_cg <- brfss_cg[CRGVPRB2==16,]
# brfss_cg[CRGVHRS1==777, CRGVHRS1 := NA]
# brfss_cg[CRGVHRS1==999, CRGVHRS1 := NA]

# for caregiving hours, replace 'don't know' with average value 
avg_temp <- weighted.mean(brfss_cg$CRGVHRS1,brfss_cg$A_LLCPWT, na.rm=TRUE)
brfss_cg$CRGVHRS1 <- as.double(brfss_cg$CRGVHRS1)
brfss_cg[is.na(CRGVHRS1), CRGVHRS1 := avg_temp]
avg_cg_hours <- weighted.mean(brfss_cg$CRGVHRS1,brfss_cg$A_LLCPWT)

brfss_cg$state <- "Missouri"

race_temp <- brfss_cg[, .(hours_by_race  = weighted.mean(CRGVHRS1,A_LLCPWT, na.rm=TRUE),sample_size = .N,  state = "Missouri"), by = "RACE2"]
race_dt <- rbind(race_dt, race_temp)


###### calculate average and sum of caregiving hours for everyone, not segmented by demographics
avg_cg_hours <- weighted.mean(brfss_cg$CRGVHRS1, brfss_cg$A_LLCPWT, na.rm=TRUE)
brfss_cg[, wtd_hrs := A_LLCPWT*CRGVHRS1]
total_cg_hours <- sum(brfss_cg$wtd_hrs, na.rm=TRUE)

# Create data table for final output 
brfss_dt <- data.table()
brfss_dt$cg_mean <- avg_cg_hours
brfss_dt$cg_sum <- total_cg_hours

num_obs <- sum(brfss_cg$A_LLCPWT)

# upper and lower bounds - for SUM not for MEAN, multiply bounds by number of observations
wt_var <- wtd.var(brfss_cg$CRGVHRS1, brfss_cg$A_LLCPWT, na.rm =TRUE)
st_error_IN <- sqrt(wt_var)/sqrt(length((brfss_cg$CRGVHRS1)))
brfss_dt$cg_lower <- num_obs*(brfss_dt$cg_mean - 1.96*st_error_IN)
brfss_dt$cg_upper <- num_obs*(brfss_dt$cg_mean + 1.96*st_error_IN)
brfss_dt$state <- "Missouri"
brfss_dt$year <- 2012
brfss_dt$sample_size <- nrow(brfss_cg)

# add variance, we will use this in ST-GPR
brfss_dt$cg_var <- wt_var

brfss_final_RC <- rbind(brfss_final_RC, brfss_dt)

##### Opportunity cost method:
#####   After creating full estimates, do everything by demographic category 

###### clean up demographic categories ######
brfss_cg <- brfss_demos(brfss_cg)
brfss_cg$year <- 2012

early_brfss_temp_dist <- data.table(weekly_cg_hours = brfss_cg$CRGVHRS1, state = brfss_cg$state, year = brfss_cg$year)
early_brfss_caregiving_distribution <- rbind(early_brfss_caregiving_distribution,early_brfss_temp_dist)

# Missouri didn't have the SEQNO variable so I will create a unique ID here (this is just the row number, I need something to sample on)
brfss_cg[, SEQNO:=.I]
brfss_final_temp <- cost_bootstrap(brfss_cg)
brfss_final_OC <- rbind(brfss_final_OC, brfss_final_temp)

#######################################################
#####        GEORGIA - 2012 
#######################################################
rm(brfss_dt,brfss_cg)
# 
# # read in caregiving module
filename <- "USA_GA_BRFSS_2012_CAREGIVER_SEQNO_Y2022M11D15.XLSX"
ga_data <- read_excel(paste0(data_dir,year,"/",filename))
ga_data <- data.table(ga_data)
# 
# # read in the rest of the BRFSS 2012 data
brfss_data <- read_dta(paste0(data_dir,year,"/",brfss_2012_filename))
names(brfss_data) <- toupper(names(brfss_data))
# 
# # merge Georgia caregiver module
# # NOTE: first cut to only people in Georgia and see if we can merge on weight, since they didn't provide SEQNO for GA
brfss_data <- data.table(brfss_data)
brfss_georgia <- brfss_data[A_STATE==13,]
# 
# merge w/ other BRFSS data to get demographics
# can remove sex from ga_data, otherwise we have two copies
ga_data[, SEX:=NULL]
ga_data <- merge(ga_data, brfss_georgia, by = "SEQNO")
ga_data[,"_LLCPWT":=NULL]
# 
# 
setnames(ga_data,c("CAREGIVE","CRGVAGE","CRGVGNDR","CRGVRELT","CRGVLONG","CRGVPROB","CRGVHRS"), 
          c("CAREGIV1","AGE_CR","SEX_CR","CRGVREL3","CRGVLNG1","CRGVPRB2","CRGVHRS1"))
ga_data$CRGVHRS1 <- as.numeric(ga_data$CRGVHRS1)
brfss_cg <- cg_prep(ga_data)


# # for caregiving hours, replace 'don't know' with average value (then re-calculate average)
avg_temp <- weighted.mean(as.numeric(brfss_cg$CRGVHRS1), brfss_cg$A_LLCPWT, na.rm=TRUE)
brfss_cg$CRGVHRS1 <- as.double(brfss_cg$CRGVHRS1)
brfss_cg[is.na(CRGVHRS1), CRGVHRS1 := avg_temp]
avg_cg_hours <- weighted.mean(as.numeric(brfss_cg$CRGVHRS1), brfss_cg$A_LLCPWT)

brfss_cg[, wtd_hrs := A_LLCPWT*CRGVHRS1]
total_cg_hours <- sum(brfss_cg$wtd_hrs, na.rm=TRUE)

brfss_dt <- data.table()
brfss_dt$cg_sum <- total_cg_hours
 

# # clean up other variable names (State, Age, Sex) to match 2015+ data
brfss_cg$state <- "Georgia"
setnames(brfss_cg, c("A_AGEG5YR"), c("AGE5YR"))

race_temp <- brfss_cg[, .(hours_by_race  = weighted.mean(CRGVHRS1,A_LLCPWT, na.rm=TRUE),sample_size = .N,  state = "Georgia"), by = "RACE2"]
race_dt <- rbind(race_dt, race_temp)


brfss_dt <- data.table()
brfss_dt$cg_mean <- avg_cg_hours
brfss_dt$cg_sum <- total_cg_hours
# 
# 
num_obs <- sum(brfss_cg$A_LLCPWT)
# 
# upper and lower bounds - for SUM not for MEAN, multiply bounds by number of observations
wt_var <- wtd.var(brfss_cg$CRGVHRS1, brfss_cg$A_LLCPWT, na.rm =TRUE)
st_error_GA <- sqrt(wt_var)/sqrt(length((brfss_cg$CRGVHRS1)))
brfss_dt$cg_lower <- num_obs*(brfss_dt$cg_mean - 1.96*st_error_GA)
brfss_dt$cg_upper <- num_obs*(brfss_dt$cg_mean + 1.96*st_error_GA)
brfss_dt$state <- "Georgia"
brfss_dt$year <- 2012
brfss_dt$sample_size <- nrow(brfss_cg)

# add variance, we will use this in ST-GPR
brfss_dt$cg_var <- wt_var

brfss_final_RC <- rbind(brfss_final_RC, brfss_dt)

##### Opportunity cost method:
#####   After creating full estimates, do everything by demographic category 

###### clean up demographic categories ######
brfss_cg <- brfss_demos(brfss_cg)
brfss_cg$year <- as.numeric(brfss_cg$IYEAR)

early_brfss_temp_dist <- data.table(weekly_cg_hours = brfss_cg$CRGVHRS1, state = brfss_cg$state, year = brfss_cg$year)
early_brfss_caregiving_distribution <- rbind(early_brfss_caregiving_distribution,early_brfss_temp_dist)

brfss_final_temp <- cost_bootstrap(brfss_cg)
brfss_final_OC <- rbind(brfss_final_OC, brfss_final_temp)

### ---------- END Georgia ----------- ######


#######################################################
#####        IOWA - 2012 
#######################################################
rm(brfss_dt,brfss_cg)
# 
# # read in caregiving module
filename <- "USA_IA_BRFSS_2012_CAREGIVER_Y2023M03D07.XLSX"
ia_data <- read_excel(paste0(iowa_dir,filename))
ia_data <- data.table(ia_data)
setnames(ia_data,c("SACGQ1","SACGQ2","SACGQ3","SACGQ4","SACGQ5","SACGQ6","SACGQ8"), 
         c("CAREGIV1","AGE_CR","SEX_CR","CRGVREL3","CRGVLNG1","CRGVPRB2","CRGVHRS1"))
ia_data$CRGVHRS1 <- as.numeric(ia_data$CRGVHRS1)

brfss_cg <- cg_prep(ia_data)

setnames(brfss_cg,"_LLCPWT","A_LLCPWT")

# # for caregiving hours, replace 'don't know' with average value (then re-calculate average)
avg_temp <- weighted.mean(as.numeric(brfss_cg$CRGVHRS1), brfss_cg$A_LLCPWT, na.rm=TRUE)
brfss_cg$CRGVHRS1 <- as.double(brfss_cg$CRGVHRS1)
brfss_cg[is.na(CRGVHRS1), CRGVHRS1 := avg_temp]
avg_cg_hours <- weighted.mean(as.numeric(brfss_cg$CRGVHRS1), brfss_cg$A_LLCPWT)

brfss_cg[, wtd_hrs := A_LLCPWT*CRGVHRS1]
total_cg_hours <- sum(brfss_cg$wtd_hrs, na.rm=TRUE)

brfss_dt <- data.table()
brfss_dt$cg_sum <- total_cg_hours


brfss_cg$state <- "Iowa"
# format age (same as Oregon, age is listed in years)
brfss_cg[(AGE >=18) & (AGE < 35), AGECAT := "18-34"]
brfss_cg[(AGE >=35) & (AGE < 45), AGECAT := "35-44"]
brfss_cg[(AGE >=45) & (AGE < 55), AGECAT := "45-54"]
brfss_cg[(AGE >=55) & (AGE < 65), AGECAT := "55-64"]
brfss_cg[(AGE >=65) & (AGE < 75), AGECAT := "65-74"]
brfss_cg[(AGE >=75), AGECAT := "75+"]
brfss_cg <- brfss_cg[!is.na(AGE),]

# COLLAPSE EDUCATION INTO 3 CATEGORIES
brfss_cg[EDUCA <=4, educ := "High school or less"]
brfss_cg[EDUCA ==5, educ := "Some college"]
brfss_cg[EDUCA==6, educ := "Bachelor's degree or more"]
brfss_cg[EDUCA==9, educ := NA]
brfss_cg$EDUCA <- brfss_cg$educ
brfss_cg <- brfss_cg[!is.na(EDUCA),]


# we did not request the race variable for Iowa, don't include in the race datatable
#race_temp <- brfss_cg[, .(hours_by_race  = weighted.mean(CRGVHRS1,A_LLCPWT, na.rm=TRUE),sample_size = .N,  state = "Iowa"), by = "RACE2"]
#race_dt <- rbind(race_dt, race_temp)


brfss_dt <- data.table()
brfss_dt$cg_mean <- avg_cg_hours
brfss_dt$cg_sum <- total_cg_hours
# 
# 
num_obs <- sum(brfss_cg$A_LLCPWT)
# 
# upper and lower bounds - for SUM not for MEAN, multiply bounds by number of observations
wt_var <- wtd.var(brfss_cg$CRGVHRS1, brfss_cg$A_LLCPWT, na.rm =TRUE)
st_error_IA <- sqrt(wt_var)/sqrt(length((brfss_cg$CRGVHRS1)))
brfss_dt$cg_lower <- num_obs*(brfss_dt$cg_mean - 1.96*st_error_IA)
brfss_dt$cg_upper <- num_obs*(brfss_dt$cg_mean + 1.96*st_error_IA)
brfss_dt$state <- "Iowa"
brfss_dt$year <- 2012
brfss_dt$sample_size <- nrow(brfss_cg)

# add variance, we will use this in ST-GPR
brfss_dt$cg_var <- wt_var

brfss_final_RC <- rbind(brfss_final_RC, brfss_dt)

##### Opportunity cost method:
#####   After creating full estimates, do everything by demographic category 


brfss_cg$year <- 2012

early_brfss_temp_dist <- data.table(weekly_cg_hours = brfss_cg$CRGVHRS1, state = brfss_cg$state, year = brfss_cg$year)
early_brfss_caregiving_distribution <- rbind(early_brfss_caregiving_distribution,early_brfss_temp_dist)

brfss_final_temp <- cost_bootstrap(brfss_cg)
brfss_final_OC <- rbind(brfss_final_OC, brfss_final_temp)

### ---------- END Iowa ----------- ######




#######################################################
#####        MAINE - 2012 
#######################################################
rm(brfss_dt,brfss_cg)
# read in caregiving module
filename <- "USA_ME_BRFSS_2001_2021_Y2023M04D06.DTA"
me_data <- read_dta(paste0(iowa_dir,"ME/",filename))
setDT(me_data)

#this is a huge file, we only need 2012 data
me_data <- me_data[year==2012,]

#remove CRGVHRS1 b/c it's in the data but it's full of NAs
me_data[, CRGVHRS1:=NULL]
me_data[, CRGVLNG1:=NULL]
setnames(me_data,c("caregive","CRE_AGE","CRE_GNDR","CRGVREL1","CRE_LONG","CRE_DSBL","CRE_HRS"),
         c("CAREGIV1","AGE_CR","SEX_CR","CRGVREL3","CRGVLNG1","CRGVPRB2","CRGVHRS1"))


brfss_cg <- cg_prep(me_data)
setnames(brfss_cg,"_LLCPWT","A_LLCPWT")
setnames(brfss_cg,"orgseqno","SEQNO")
setnames(brfss_cg,"sex","SEX")

# for caregiving hours, replace 'don't know' with average value (then re-calculate average)
avg_temp <- weighted.mean(as.numeric(brfss_cg$CRGVHRS1), brfss_cg$A_LLCPWT, na.rm=TRUE)
brfss_cg$CRGVHRS1 <- as.double(brfss_cg$CRGVHRS1)
brfss_cg[is.na(CRGVHRS1), CRGVHRS1 := avg_temp]
avg_cg_hours <- weighted.mean(as.numeric(brfss_cg$CRGVHRS1), brfss_cg$A_LLCPWT)

brfss_cg[, wtd_hrs := A_LLCPWT*CRGVHRS1]
total_cg_hours <- sum(brfss_cg$wtd_hrs, na.rm=TRUE)

brfss_dt <- data.table()
brfss_dt$cg_sum <- total_cg_hours


brfss_cg$state <- "Maine"
# format age (same as Oregon, age is listed in years)
brfss_cg[(age >=18) & (age < 35), AGECAT := "18-34"]
brfss_cg[(age >=35) & (age < 45), AGECAT := "35-44"]
brfss_cg[(age >=45) & (age < 55), AGECAT := "45-54"]
brfss_cg[(age >=55) & (age < 65), AGECAT := "55-64"]
brfss_cg[(age >=65) & (age < 75), AGECAT := "65-74"]
brfss_cg[(age >=75), AGECAT := "75+"]
brfss_cg <- brfss_cg[!is.na(age),]

# COLLAPSE EDUCATION INTO 3 CATEGORIES
brfss_cg[educa <=4, educ := "High school or less"]
brfss_cg[educa ==5, educ := "Some college"]
brfss_cg[educa==6, educ := "Bachelor's degree or more"]
brfss_cg[educa==9, educ := NA]
brfss_cg$EDUCA <- brfss_cg$educ
brfss_cg <- brfss_cg[!is.na(EDUCA),]

brfss_dt <- data.table()
brfss_dt$cg_mean <- avg_cg_hours
brfss_dt$cg_sum <- total_cg_hours

num_obs <- sum(brfss_cg$A_LLCPWT)

# upper and lower bounds - for SUM not for MEAN, multiply bounds by number of observations
wt_var <- wtd.var(brfss_cg$CRGVHRS1, brfss_cg$A_LLCPWT, na.rm =TRUE)
st_error_ME <- sqrt(wt_var)/sqrt(length((brfss_cg$CRGVHRS1)))
brfss_dt$cg_lower <- num_obs*(brfss_dt$cg_mean - 1.96*st_error_ME)
brfss_dt$cg_upper <- num_obs*(brfss_dt$cg_mean + 1.96*st_error_ME)
brfss_dt$state <- "Maine"
brfss_dt$year <- 2012
brfss_dt$sample_size <- nrow(brfss_cg)
# 
# add variance, we will use this in ST-GPR
brfss_dt$cg_var <- wt_var

brfss_final_RC <- rbind(brfss_final_RC, brfss_dt)

##### Opportunity cost method:
#####   After creating full estimates, do everything by demographic category 
brfss_cg$year <- 2012

early_brfss_temp_dist <- data.table(weekly_cg_hours = brfss_cg$CRGVHRS1, state = brfss_cg$state, year = brfss_cg$year)
early_brfss_caregiving_distribution <- rbind(early_brfss_caregiving_distribution,early_brfss_temp_dist)

brfss_final_temp <- cost_bootstrap(brfss_cg)
brfss_final_OC <- rbind(brfss_final_OC, brfss_final_temp)

### ---------- END Maine ----------- ######





####################### 2013 #########################################
year = 2013


#################################################
######           ILLINOIS - 2013          #######
#################################################
rm(brfss_dt)
cg_filename <- 'USA_IL_BRFSS_2013_SA_CAREGIVER_Y2022M08D12.xlsx' 

brfss_2013_filename <- 'USA_BRFSS_2013_LANDLINE_AND_CELL_Y2014M09D18.DTA'


brfss_cg_module <- read_excel(paste0(data_dir,year,"/",cg_filename))

brfss_data <- read_dta(paste0(data_dir,year,"/",brfss_2013_filename))
brfss_data <- data.table(brfss_data)
# Illinois is 17
brfss_data <- brfss_data[A_STATE==17,]
names(brfss_data) <- toupper(names(brfss_data))

# merge ILLINOIS caregiver module - note CAREGIV1 is 1 for everyone in the caregiver file, no need to filter to caregivers
brfss_cg <- merge(brfss_data, brfss_cg_module, by="SEQNO")

setnames(brfss_cg,c("CGvQ01CARE","CGvQ02AGE","CGvQ03SEX","CGvQ04RELATE","CGvQ05TIMECARE","CGvQ06MJDX","CGvQ08WEEKHRS"), 
         c("CAREGIV1","AGE_CR","SEX_CR","CRGVREL3","CRGVLNG1","CRGVPRB2","CRGVHRS1"))
brfss_cg <- cg_prep(brfss_cg)

# for caregiving hours, replace 'don't know' with average value 
avg_temp <- weighted.mean(brfss_cg$CRGVHRS1, brfss_cg$A_LLCPWT, na.rm=TRUE)
brfss_cg$CRGVHRS1 <- as.double(brfss_cg$CRGVHRS1)
brfss_cg[is.na(CRGVHRS1), CRGVHRS1 := avg_temp]
avg_cg_hours <- weighted.mean(brfss_cg$CRGVHRS1, brfss_cg$A_LLCPWT)

# calculate total caregiving hours
brfss_cg[, wtd_hrs := A_LLCPWT*CRGVHRS1]
total_cg_hours <- sum(brfss_cg$wtd_hrs, na.rm=TRUE)


# clean up other variable names (State, Age, Sex) to match 2015+ data
# note: I know the state is Indiana so hard-coding for now to skip the FIPS step
#       sex is already named "SEX", no need to recode
brfss_cg$state <- "Illinois"
setnames(brfss_cg, c("A_AGEG5YR"), c("AGE5YR"))

setnames(brfss_cg, "A_RACE","RACE2")
race_temp <- brfss_cg[, .(hours_by_race  = weighted.mean(CRGVHRS1,A_LLCPWT, na.rm=TRUE),sample_size = .N,  state = "Illinois"), by = "RACE2"]
race_dt <- rbind(race_dt, race_temp)

brfss_dt <- data.table()
brfss_dt$cg_mean <- avg_cg_hours
brfss_dt$cg_sum <- total_cg_hours

num_obs <- sum(brfss_cg$A_LLCPWT)

# upper and lower bounds
wt_var <- wtd.var(brfss_cg$CRGVHRS1, brfss_cg$A_LLCPWT, na.rm =TRUE)
st_error_IL <- sqrt(wt_var)/sqrt(length((brfss_cg$CRGVHRS1)))
brfss_dt$cg_lower <- num_obs*(brfss_dt$cg_mean - 1.96*st_error_IL)
brfss_dt$cg_upper <- num_obs*(brfss_dt$cg_mean + 1.96*st_error_IL)
brfss_dt$state <- "Illinois"
brfss_dt$year <- 2013
brfss_dt$sample_size <- nrow(brfss_cg)

# add variance, we will use this in ST-GPR
brfss_dt$cg_var <- wt_var

brfss_final_RC <- rbind(brfss_final_RC, brfss_dt)

###### BEGIN: opportunity cost data prep ######

brfss_cg <- brfss_demos(brfss_cg)
brfss_cg$year <- as.numeric(brfss_cg$IYEAR)

early_brfss_temp_dist <- data.table(weekly_cg_hours = brfss_cg$CRGVHRS1, state = brfss_cg$state, year = brfss_cg$year)
early_brfss_caregiving_distribution <- rbind(early_brfss_caregiving_distribution,early_brfss_temp_dist)

brfss_final_temp <- cost_bootstrap(brfss_cg)
brfss_final_OC <- rbind(brfss_final_OC, brfss_final_temp)

###### END: opportunity cost data prep ######



###############################################
######           OREGON - 2013          #######
###############################################
rm(brfss_dt, brfss_cg,brfss_final_temp)
cg_filename <- "BRFSS_OREGON_2013_Y2021M07D08.SAV"

brfss_cg_module <- read_sav(paste0(l_path,year,"/",cg_filename))
brfss_cg_module <- data.table(brfss_cg_module)

setnames(brfss_cg_module,c("CAREMONTH","CAREAGE","CARESEX","CAREREL","CARELONG","CAREWHAT","CAREHRS","wtrk_C_c"), 
         c("CAREGIV1","AGE_CR","SEX_CR","CRGVREL3","CRGVLNG1","CRGVPRB2","CRGVHRS1","A_LLCPWT"))
brfss_cg <- cg_prep(brfss_cg_module)

brfss_cg$CRGVHRS1 <- as.double(brfss_cg$CRGVHRS1)
avg_temp <- weighted.mean(brfss_cg$CRGVHRS1, brfss_cg$A_LLCPWT, na.rm=TRUE)
brfss_cg[is.na(CRGVHRS1), CRGVHRS1 := avg_temp]
brfss_cg[, wtd_hrs := A_LLCPWT*CRGVHRS1]
total_cg_hours <- sum(brfss_cg$wtd_hrs, na.rm=TRUE)
#brfss_dt$cg_sum <- total_cg_hours
avg_cg_hours <- weighted.mean(brfss_cg$CRGVHRS1, brfss_cg$A_LLCPWT)


brfss_cg$state <- "Oregon"


brfss_dt <- data.table()
brfss_dt$cg_mean <- avg_cg_hours
brfss_dt$cg_sum <- total_cg_hours

# Note: there is a variable called AGEGRP in here but it is almost all NAs
#       need to use AGE and categorize it myself
# COLLAPSE AGE INTO 10 YEAR BINS
brfss_cg[(AGE >=18) & (AGE < 35), AGECAT := "18-34"]
brfss_cg[(AGE >=35) & (AGE < 45), AGECAT := "35-44"]
brfss_cg[(AGE >=45) & (AGE < 55), AGECAT := "45-54"]
brfss_cg[(AGE >=55) & (AGE < 65), AGECAT := "55-64"]
brfss_cg[(AGE >=65) & (AGE < 75), AGECAT := "65-74"]
brfss_cg[(AGE >=75), AGECAT := "75+"]
brfss_cg <- brfss_cg[!is.na(AGE),]

# COLLAPSE EDUCATION INTO 3 CATEGORIES
# for some reason the EDUCA variable is empty, but EDUCAX looks good. 
#    Use EDUCAX and then overwrite EDUCA for naming consistency
brfss_cg[EDUCAX <=4, educ := "High school or less"]
brfss_cg[EDUCAX ==5, educ := "Some college"]
brfss_cg[EDUCAX==6, educ := "Bachelor's degree or more"]
brfss_cg[EDUCAX==9, educ := NA]
brfss_cg$EDUCAX <- brfss_cg$educ
brfss_cg <- brfss_cg[!is.na(EDUCAX),]
brfss_cg[, EDUCA := educ]


num_obs <- sum(brfss_cg$A_LLCPWT)

# upper and lower bounds
wt_var <- wtd.var(brfss_cg$CRGVHRS1, brfss_cg$A_LLCPWT, na.rm =TRUE)
st_error_OR <- sqrt(wt_var)/sqrt(length((brfss_cg$CRGVHRS1)))
brfss_dt$cg_lower <- num_obs*(brfss_dt$cg_mean - 1.96*st_error_OR)
brfss_dt$cg_upper <- num_obs*(brfss_dt$cg_mean + 1.96*st_error_OR)
brfss_dt$state <- "Oregon"
brfss_dt$year <- 2013
brfss_dt$sample_size <- nrow(brfss_cg)

# add variance, we will use this in ST-GPR
brfss_dt$cg_var <- wt_var

brfss_final_RC <- rbind(brfss_final_RC, brfss_dt)

brfss_cg[, RACE2:=NULL]
setnames(brfss_cg, "racesum2", "RACE2")
brfss_cg$RACE2 <- as.numeric(brfss_cg$RACE2)
race_temp <- brfss_cg[, .(hours_by_race  = weighted.mean(CRGVHRS1,A_LLCPWT, na.rm=TRUE),sample_size = .N,  state = "Oregon"), by = "RACE2"]
race_dt <- rbind(race_dt, race_temp)


###### opportunity cost method - demographic variables are already binned
brfss_cg$year <- as.numeric(brfss_cg$IYEAR)

early_brfss_temp_dist <- data.table(weekly_cg_hours = brfss_cg$CRGVHRS1, state = brfss_cg$state, year = brfss_cg$year)
early_brfss_caregiving_distribution <- rbind(early_brfss_caregiving_distribution,early_brfss_temp_dist)

brfss_final_temp <- cost_bootstrap(brfss_cg)
brfss_final_OC <- rbind(brfss_final_OC, brfss_final_temp)

#################################################
######           ARKANSAS - 2013          #######
#################################################


rm(brfss_dt)
cg_filename <- 'USA_AR_BRFSS_2013_LANDLINE_CELL_PHONE_Y2023M01D06.sas7bdat' 


brfss_arkansas <- read_sas(paste0(data_dir,year,"/",cg_filename))

# --- Age, sex, and education are in this file, no need to merge
#     Rename caregiver variables to match other modules
setnames(brfss_arkansas,c("CAREGIVE","CAREAGE","CAREGEN","CAREREL","CARELONG","CAREDOC","CAREHRS","_LLCPWT"), 
         c("CAREGIV1","AGE_CR","SEX_CR","CRGVREL3","CRGVLNG1","CRGVPRB2","CRGVHRS1","A_LLCPWT"))

brfss_arkansas <- data.table(brfss_arkansas)
brfss_cg <- cg_prep(brfss_arkansas)


# for caregiving hours, replace 'don't know' with average value 
avg_temp <- weighted.mean(brfss_cg$CRGVHRS1, brfss_cg$A_LLCPWT, na.rm=TRUE)
brfss_cg$CRGVHRS1 <- as.double(brfss_cg$CRGVHRS1)
brfss_cg[is.na(CRGVHRS1), CRGVHRS1 := avg_temp]
avg_cg_hours <- weighted.mean(brfss_cg$CRGVHRS1, brfss_cg$A_LLCPWT)

# calculate total caregiving hours
brfss_cg[, wtd_hrs := A_LLCPWT*CRGVHRS1]
total_cg_hours <- sum(brfss_cg$wtd_hrs, na.rm=TRUE)


# clean up other variable names (State, Age, Sex) to match 2015+ data as needed
brfss_cg$state <- "Arkansas"
setnames(brfss_cg, c("_AGEG5YR"), c("AGE5YR"))

brfss_dt <- data.table()
brfss_dt$cg_mean <- avg_cg_hours
brfss_dt$cg_sum <- total_cg_hours

num_obs <- sum(brfss_cg$A_LLCPWT)

# upper and lower bounds
wt_var <- wtd.var(brfss_cg$CRGVHRS1, brfss_cg$A_LLCPWT, na.rm =TRUE)
st_error_AR <- sqrt(wt_var)/sqrt(length((brfss_cg$CRGVHRS1)))
brfss_dt$cg_lower <- num_obs*(brfss_dt$cg_mean - 1.96*st_error_AR)
brfss_dt$cg_upper <- num_obs*(brfss_dt$cg_mean + 1.96*st_error_AR)
brfss_dt$state <- "Arkansas"
brfss_dt$year <- 2013
brfss_dt$sample_size <- nrow(brfss_cg)

# add variance, we will use this in ST-GPR
brfss_dt$cg_var <- wt_var

brfss_final_RC <- rbind(brfss_final_RC, brfss_dt)

###### BEGIN: opportunity cost data prep ######

brfss_cg <- brfss_demos(brfss_cg)
brfss_cg$year <- as.numeric(brfss_cg$IYEAR)

setnames(brfss_cg, "_RACE", "RACE2")
race_temp <- brfss_cg[, .(hours_by_race  = weighted.mean(CRGVHRS1,A_LLCPWT, na.rm=TRUE),sample_size = .N,  state = "Arkansas"), by = "RACE2"]
race_dt <- rbind(race_dt, race_temp)

# try cutting AR b/c it's an outlier in terms of ppl reporting 168 hrs of care
early_brfss_temp_dist <- data.table(weekly_cg_hours = brfss_cg$CRGVHRS1, state = brfss_cg$state, year = brfss_cg$year)
early_brfss_caregiving_distribution <- rbind(early_brfss_caregiving_distribution,early_brfss_temp_dist)

brfss_final_temp <- cost_bootstrap(brfss_cg)
brfss_final_OC <- rbind(brfss_final_OC, brfss_final_temp)

###### END: opportunity cost data prep ######


##### END: state data prep ######

fwrite(brfss_final_RC, paste0("FILEPATH/BRFSS_caregiving_hours_2012_2013_",top_code_string,".csv"))
fwrite(brfss_final_OC, paste0("FILEPATH/BRFSS_caregiving_costs_2012_2013_",top_code_string,".csv"))
fwrite(early_brfss_caregiving_distribution, paste0("FILEPATH/early_BRFSS_caregiving_distribution_",top_code_string,".csv"))
