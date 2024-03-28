## NHATS data preprocessing for dementia attribution model 
## 11/01/2022
## Author: Michael Breshock

setwd("FILEPATH")
rm(list = ls())

library(ggplot2)
library(dplyr)
library(data.table)
library(tidyr)
library(haven)
library(Amelia)
library(purrr)
source("functions/classify_dementia_NHATS.R")
source("functions/backfill_hc.R")

# load all data

# NHATS SP Data
sp1 = data.table(read_sas(paste0(FILEPATH,"NHATS_Round_1_SP_File.sas7bdat")))
sp2 = data.table(read_sas(paste0(FILEPATH,"NHATS_Round_2_SP_File_v2.sas7bdat")))
sp3 = data.table(read_sas(paste0(FILEPATH,"NHATS_Round_3_SP_File.sas7bdat")))
sp4 = data.table(read_sas(paste0(FILEPATH,"NHATS_Round_4_SP_File.sas7bdat")))
sp5 = data.table(read_sas(paste0(FILEPATH,"NHATS_Round_5_SP_File_v2.sas7bdat")))
sp6 = data.table(read_sas(paste0(FILEPATH,"NHATS_Round_6_SP_File_V2.sas7bdat")))
sp7 = data.table(read_sas(paste0(FILEPATH,"NHATS_Round_7_SP_File.sas7bdat")))
sp8 = data.table(read_sas(paste0(FILEPATH,"NHATS_Round_8_SP_File.sas7bdat")))
sp9 = data.table(read_sas(paste0(FILEPATH,"NHATS_Round_9_SP_File.sas7bdat")))
# add year column
sp1[,year := 2011]
sp2[,year := 2012]
sp3[,year := 2013]
sp4[,year := 2014]
sp5[,year := 2015]
sp6[,year := 2016]
sp7[,year := 2017]
sp8[,year := 2018]
sp9[,year := 2019]

# NHATS Sensitive data (for age)
sens1 = data.table(read_dta(paste0(FILEPATH,"USA_NHATS_2011_ROUND_1_SP_SEN_DEM_FILE_Y2022M08D03.DTA")))
sens2 = data.table(read_dta(paste0(FILEPATH,"USA_NHATS_2012_ROUND_2_SP_SEN_DEM_FILE_Y2022M08D03.DTA")))
sens3 = data.table(read_dta(paste0(FILEPATH,"USA_NHATS_2013_ROUND_3_SP_SEN_DEM_FILE_Y2022M08D03.DTA")))
sens4 = data.table(read_dta(paste0(FILEPATH,"USA_NHATS_2014_ROUND_4_SP_SEN_DEM_FILE_Y2022M08D03.DTA")))
sens5 = data.table(read_dta(paste0(FILEPATH,"USA_NHATS_2015_ROUND_5_SP_SEN_DEM_FILE_Y2022M08D03.DTA")))
sens6 = data.table(read_dta(paste0(FILEPATH,"USA_NHATS_2016_ROUND_6_SP_SEN_DEM_FILE_Y2022M08D03.DTA")))
sens7 = data.table(read_dta(paste0(FILEPATH,"USA_NHATS_2017_ROUND_7_SP_SEN_DEM_FILE_Y2022M08D03.DTA")))
sens8 = data.table(read_dta(paste0(FILEPATH,"USA_NHATS_2018_ROUND_8_SP_SEN_DEM_FILE_Y2022M08D03.DTA")))
sens9 = data.table(read_dta(paste0(FILEPATH,"USA_NHATS_2019_ROUND_9_SP_SEN_DEM_FILE_Y2022M08D03.DTA")))

# ---------------------------------------------------------------------------- #
# initial merging and preprocessing

# classifying persons in NHATS has having dementia or not
sp1dem = classify_dementia_NHATS(sp1, round_num = 1)
sp2dem = classify_dementia_NHATS(sp2, round_num = 2)
sp3dem = classify_dementia_NHATS(sp3, round_num = 3)
sp4dem = classify_dementia_NHATS(sp4, round_num = 4)
sp5dem = classify_dementia_NHATS(sp5, round_num = 5)
sp6dem = classify_dementia_NHATS(sp6, round_num = 6)
sp7dem = classify_dementia_NHATS(sp7, round_num = 7)
sp8dem = classify_dementia_NHATS(sp8, round_num = 8)
sp9dem = classify_dementia_NHATS(sp9, round_num = 9)

# get age from sensitive data
age1 = sens1[, .(spid, r1dintvwrage)]
age2 = sens2[, .(spid, r2dintvwrage)]
age3 = sens3[, .(spid, r3dintvwrage)]
age4 = sens4[, .(spid, r4dintvwrage)]
age5 = sens5[, .(spid, r5dintvwrage)]
age6 = sens6[, .(spid, r6dintvwrage)]
age7 = sens7[, .(spid, r7dintvwrage)]
age8 = sens8[, .(spid, r8dintvwrage)]
age9 = sens9[, .(spid, r9dintvwrage)]

# merging sensitive age data with public nhats data 
# and backfilling health condition data
sp1age = data.table(merge(sp1dem, age1, by = "spid", all.x = TRUE))
# dont need to backfill this round since it is the first one

sp2age = data.table(merge(sp2dem, age2, by = "spid", all.x = TRUE))
sp2age = backfill_hc(sp1age, sp2age, 2)

sp3age = data.table(merge(sp3dem, age3, by = "spid", all.x = TRUE))
sp3age = backfill_hc(sp2age, sp3age, 3)

sp4age = data.table(merge(sp4dem, age4, by = "spid", all.x = TRUE))
sp4age = backfill_hc(sp3age, sp4age, 4)

sp5age = data.table(merge(sp5dem, age5, by = "spid", all.x = TRUE))
sp5age = backfill_hc(sp4age, sp5age, 5)

sp6age = data.table(merge(sp6dem, age6, by = "spid", all.x = TRUE))
sp6age = backfill_hc(sp5age, sp6age, 6)

sp7age = data.table(merge(sp7dem, age7, by = "spid", all.x = TRUE))
sp7age = backfill_hc(sp6age, sp7age, 7)

sp8age = data.table(merge(sp8dem, age8, by = "spid", all.x = TRUE))
sp8age = backfill_hc(sp7age, sp8age, 8)

sp9age = data.table(merge(sp9dem, age9, by = "spid", all.x = TRUE))
sp9age = backfill_hc(sp8age, sp9age, 9)

# ---------------------------------------------------------------------------- #
# aggregate NHATS data

# filling in missing gender and race data for round 2
# these questions were only asked in rounds 1 and 5 -> when sample was updated
r1sex_race = sp1age[,.(spid, r1dgender, rl1dracehisp)]

# gender and race data from round 1 data was not included in round 2 data
# but previous gender and race data is included in rounds 3 and following
sp2age = merge(sp2age, r1sex_race, by = "spid", all.x = T)



# function to process nhats round column names
NHATS_demographics <- function(df, r){
  # get appropriate column name for corresponding round
  if(r == 1){
    marriage = "hh1martlstat"
  }else if(r < 1 | r > 9) {
    return("You Sure?")
  }else{
    marriage = paste0("hh",r,"dmarstat")
  }
  
  if(r %in% c(1:4)){
    ethnicity = "rl1dracehisp"
    gender = "r1dgender"
  }else{
    ethnicity = "rl5dracehisp"
    gender = "r5dgender"
  }

  children = paste0("hh",r,"dhshldchd")
  age = paste0("r",r,"dintvwrage")
  health_conditions = paste0("hc",r,"disescn",c(2:4,6:8,10))
  psych_conditions = paste0("hc",r,"depresan",c(1:4))
  
  # combine all names into list
  old_names = c(age, gender, ethnicity, marriage, children, 
                health_conditions, psych_conditions)
  
  # create list of new names they should be changed to 
  new_names = c("age","gender","ethnicity","marriage","num_child",
                paste0("disescn", c(2:4,6:8,10)),
                paste0("depresan", c(1:4)))
  
  setnames(df, old = old_names, new = new_names)
  return(df)
}

new_names = c("age","gender","ethnicity","marriage","num_child",
              paste0("disescn", c(2:4,6:8,10)),
              paste0("depresan", c(1:4)))

# changing column names of interest to unify names across rounds
sp1demo = NHATS_demographics(sp1age, 1)
sp2demo = NHATS_demographics(sp2age, 2)
sp3demo = NHATS_demographics(sp3age, 3)
sp4demo = NHATS_demographics(sp4age, 4)
sp5demo = NHATS_demographics(sp5age, 5)
sp6demo = NHATS_demographics(sp6age, 6)
sp7demo = NHATS_demographics(sp7age, 7)
sp8demo = NHATS_demographics(sp8age, 8)
sp9demo = NHATS_demographics(sp9age, 9)

# grabbing of columns of interest to combine each round of data
vars = c("year", "spid", "dementia", new_names)

sp1vars = sp1demo[, .SD, .SDcols = vars]
sp2vars = sp2demo[, .SD, .SDcols = vars]
sp3vars = sp3demo[, .SD, .SDcols = vars]
sp4vars = sp4demo[, .SD, .SDcols = vars]
sp5vars = sp5demo[, .SD, .SDcols = vars]
sp6vars = sp6demo[, .SD, .SDcols = vars]
sp7vars = sp7demo[, .SD, .SDcols = vars]
sp8vars = sp8demo[, .SD, .SDcols = vars]
sp9vars = sp9demo[, .SD, .SDcols = vars]

# bind all rounds together
SP = rbind(sp1vars, sp2vars, sp3vars, sp4vars, sp5vars, 
           sp6vars, sp7vars, sp8vars, sp9vars)

# set missing values to NA
SP[SP < 0] <- NA

# ---------------------------------------------------------------------------- #
# # multiple imputation 
# # Amelia imputations can take a while to run so I'm commenting this section out - 
# # uncomment this section only if imputations need to be run again. 

# # taking only the variables that will be used in the model and a few extra demographics
# # that have no missing values to help  amelia run better
# SPcovars = SP[, .(year, spid, age, dementia, ethnicity, gender, disescn2,
#                   disescn3, disescn4, disescn6, disescn7, disescn8,
#                   disescn10, depresan1, depresan2, depresan3, depresan4)]
# # saving out raw NHATS demographic data to do BMS testing later
# fwrite(SPcovars, file = paste0(j_root, "Project/IRH/Informal_Care_AD/US_dementia_spending_2022/output/NHATS_NSOC/NHATS_AF_input_raw.csv"))
# 
# # declaring these variables as nominals since they are categorical
# nominal_vars = c("dementia", "ethnicity", "gender", "disescn2", "disescn3", "disescn4",
#                  "disescn6", "disescn7", "disescn8", "disescn10", "depresan1",
#                  "depresan2", "depresan3", "depresan4")
# # saving the column numbers of the columns we want lags and leads for (all of them)
# col_nums = c(1:ncol(SPcovars))
# 
# # create boundaries for age variable
# min_age = as.double(min(SPcovars$age, na.rm = T))
# max_age = as.double(max(SPcovars$age, na.rm = T))
# age_bounds = matrix(c(3, min_age, max_age), nrow = 1, ncol = 3)
# # first col = column number in input data, age is the 3rd column
# # second col = lower bound; third col = upper bound
# # run amelia imputation to fill missing covariate data
# ameliaNHATS = amelia(SPcovars, m = 10, idvars = "spid", ts = "year", splinetime = 2,
#                      noms = nominal_vars, lags = col_nums, leads = col_nums,
#                      bounds = age_bounds, parallel = "snow")
# 
# # save amelia output
# save(ameliaNHATS, file = paste0(FILEPATH,"imputedNHATS_Mar24.rda"))

# ---------------------------------------------------------------------------- #
# renaming and recoding final variables - exporting output

# # if you need to load the data again V
load(paste0(FILEPATH,"imputedNHATS_Mar24.rda"))

# recoding variables: 
# health conditions
for(i in c(1:10)){
  df = ameliaNHATS$imputations[[i]]
  df[, ':=' (
    heart_disease = disescn2 == 1, # has heart disease
    hbp = disescn3 == 1, # has high blood pressure
    arthritis = disescn4 == 1, # has arthritis
    diabetes = disescn6 == 1, # has diabetes
    lung_disease = disescn7 == 1, # has lung disease
    stroke = disescn8 == 1, # had stroke
    cancer = disescn10 == 1 # has cancer
  )]
  
  # using cut off points from Kroenke et al 2009: An Ultra-Brief Screening Scale for Anxiety and Depression: The PHQ–4
  # recoding PHQ-4 questions to appropriate scores 
  # 1 not at all = 0
  # 2 several days = 1
  # 3 more than half the days = 2
  # 4 nearly every day = 3
  df[, ':=' (dep1 = depresan1 - 1, # little interest or pleasure
             dep2 = depresan2 - 1, # down depressed hopeless
             anx1 = depresan3 - 1, # nervous anxious
             anx2 = depresan4 - 1  # unable to stop worry
  )]
  
  # calculating PHQ-2 and GAD-2 scores
  df[, ':=' (PHQ = dep1 + dep2, # PHQ-2 score = sum of depression scores
             GAD = anx1 + anx2)] # GAD-2 score = sum of anxiety scores
  
  # classifying depression and anxiety
  df[, ':=' (depression = PHQ >= 3, # PHQ cut-point = 3,
             anxiety = GAD >= 3)] # GAD cut-point = 3, both from Kroenke 2009
  
  # fix age variable output by Amelia so it is discrete (no fractions)
  df[, age := round(age)]
  
  # create age dummy variables in addition to continuous age
  df[, ":=" (age_65_69 = age %in% c(65:69),
             age_70_79 = age %in% c(70:79),
             age_80_89 = age %in% c(80:89),
             age_90plus = age >= 90)]
  
  ameliaNHATS$imputations[[i]] = df
}

# rename variable and save to RDA file
cleanNHATS = ameliaNHATS
save(cleanNHATS, file = paste0(FILEPATH,"cleanNHATS_Mar24.rda"))






