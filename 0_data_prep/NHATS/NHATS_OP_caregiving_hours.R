# Script to create .csv files of NHATS caregiving hours by SP and OP 
# 01/12/2023
# Author: Michael Breshock

setwd("FILEPATH")
rm(list = ls())

# load in libraries
library(dplyr)
library(data.table)
library(haven)
source("~/alzheimers/Informal_Care_2022/functions/process_NHATS_OP.R")
source("~/alzheimers/Informal_Care_2022/functions/classify_dementia_NHATS.R")


# load all data

# load NHATS OP (other person) data
op1 = data.table(read_sas(paste0(FILEPATH,"NHATS_Round_1_OP_File_v2.sas7bdat")))
op2 = data.table(read_sas(paste0(FILEPATH,"NHATS_Round_2_OP_File_v2.sas7bdat")))
op3 = data.table(read_sas(paste0(FILEPATH,"NHATS_Round_3_OP_File.sas7bdat")))
op4 = data.table(read_sas(paste0(FILEPATH,"NHATS_Round_4_OP_File.sas7bdat")))
op5 = data.table(read_sas(paste0(FILEPATH,"NHATS_Round_5_OP_File_V2.sas7bdat")))
op6 = data.table(read_sas(paste0(FILEPATH,"NHATS_Round_6_OP_File_V2.sas7bdat")))
op7 = data.table(read_sas(paste0(FILEPATH,"NHATS_Round_7_OP_File.sas7bdat")))
op8 = data.table(read_sas(paste0(FILEPATH,"NHATS_Round_8_OP_File.sas7bdat")))
op9 = data.table(read_sas(paste0(FILEPATH,"NHATS_Round_9_OP_File.sas7bdat")))

# NHATS sensitive data (need this for raw age value)
sens1 = data.table(read_dta(
  paste0(FILEPATH,"2011_R1_SP/USA_NHATS_2011_ROUND_1_SP_SEN_DEM_FILE_Y2022M08D03.DTA")))
sens2 = data.table(read_dta(
  paste0(FILEPATH,"USA_NHATS_2012_ROUND_2_SP_SEN_DEM_FILE_Y2022M08D03.DTA")))
sens3 = data.table(read_dta(
  paste0(FILEPATH,"USA_NHATS_2013_ROUND_3_SP_SEN_DEM_FILE_Y2022M08D03.DTA")))
sens4 = data.table(read_dta(
  paste0(FILEPATH,"USA_NHATS_2014_ROUND_4_SP_SEN_DEM_FILE_Y2022M08D03.DTA")))
sens5 = data.table(read_dta(
  paste0(FILEPATH,"USA_NHATS_2015_ROUND_5_SP_SEN_DEM_FILE_Y2022M08D03.DTA")))
sens6 = data.table(read_dta(
  paste0(FILEPATH,"USA_NHATS_2016_ROUND_6_SP_SEN_DEM_FILE_Y2022M08D03.DTA")))
sens7 = data.table(read_dta(
  paste0(FILEPATH,"USA_NHATS_2017_ROUND_7_SP_SEN_DEM_FILE_Y2022M08D03.DTA")))
sens8 = data.table(read_dta(
  paste0(FILEPATH,"USA_NHATS_2018_ROUND_8_SP_SEN_DEM_FILE_Y2022M08D03.DTA")))
sens9 = data.table(read_dta(
  paste0(FILEPATH,"USA_NHATS_2019_ROUND_9_SP_SEN_DEM_FILE_Y2022M08D03.DTA")))

# add year column to sens data
sens1[, year := 2011]
sens2[, year := 2012]
sens3[, year := 2013]
sens4[, year := 2014]
sens5[, year := 2015]
sens6[, year := 2016]
sens7[, year := 2017]
sens8[, year := 2018]
sens9[, year := 2019]

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


###################### GET AGES FROM SENSITIVE DATA FILES ######################
# define function to extract age column and rename
get_sens_age <- function(df){
  old_age = grep("dintvwrage", names(df), value = T)
  vars = c("year", "spid", old_age)
  age = df[, .SD, .SDcols = vars]
  setnames(age, old = old_age, new = "age")
  return(age)
}

sens_list = list(R1 = sens1, R2 = sens2, R3 = sens3, R4 = sens4, R5 = sens5, 
                 R6 = sens6, R7 = sens7, R8 = sens8, R9 = sens9)

# get SP ages from sensitive data
ages = lapply(names(sens_list), function(x) get_sens_age(sens_list[[x]]))

# combine age data into single DF
SP_age = rbindlist(ages)
# save out SP ages: 
fwrite(SP_age, file = paste0(FILEPATH,"SP_ages_R1_R9.csv"))

############ GET WEIGHTS/RESIDENCE STATUS FROM SENSITIVE DATA FILES ############

# change column names for SP data round 2
setnames(sp2, grep("ANFINWGT", names(sp2), value = T), paste0("w2anfinwgt",0:56))

# variables needed from NHATS to calculate estimates
# getting repeated weight variable names
old_weights = grep("anfinwgt", names(sp1), value = T) # getting all weight variables
new_weights = sub(".{2}","", old_weights) # removing prefix
vars = c("year", "spid", "dementia", "residence", new_weights)

# classifying dementia in SPs from NHATS and extracting columns of interest
sp1dem = classify_dementia_NHATS(sp1, round_num = 1)[, .SD, .SDcols = vars]
sp2dem = classify_dementia_NHATS(sp2, round_num = 2)[, .SD, .SDcols = vars]
sp3dem = classify_dementia_NHATS(sp3, round_num = 3)[, .SD, .SDcols = vars]
sp4dem = classify_dementia_NHATS(sp4, round_num = 4)[, .SD, .SDcols = vars]
sp5dem = classify_dementia_NHATS(sp5, round_num = 5)[, .SD, .SDcols = vars]
sp6dem = classify_dementia_NHATS(sp6, round_num = 6)[, .SD, .SDcols = vars]
sp7dem = classify_dementia_NHATS(sp7, round_num = 7)[, .SD, .SDcols = vars]
sp8dem = classify_dementia_NHATS(sp8, round_num = 8)[, .SD, .SDcols = vars]
sp9dem = classify_dementia_NHATS(sp9, round_num = 9)[, .SD, .SDcols = vars]

# binding NHATS rounds 1, 5, 7 SP data together 
SP = rbind(sp1dem, sp2dem, sp3dem, sp4dem, sp5dem, 
           sp6dem, sp7dem, sp8dem, sp9dem)

# filter to only alive persons with dementia and merge in age data
# residence = 6 -> deceased
SP_dem = merge(SP[dementia == 1 & residence != 6], SP_age, by = c("year", "spid"))

# filter to SPs age 70 and up
# this is to account for the aging sample population 
SP_dem70 = SP_dem[age >= 70]

###################### GET CARE HOURS FROM OP DATA FILES #######################


# process each round of data [filtering to unpaid helpers and extracting care hours per week]
op1_help = process_NHATS_OP(op1, 1)
op2_help = process_NHATS_OP(op2, 2)
op3_help = process_NHATS_OP(op3, 3)
op4_help = process_NHATS_OP(op4, 4)
op5_help = process_NHATS_OP(op5, 5)
op6_help = process_NHATS_OP(op6, 6)
op7_help = process_NHATS_OP(op7, 7)
op8_help = process_NHATS_OP(op8, 8)
op9_help = process_NHATS_OP(op9, 9)

op_hours = rbind(op1_help, op2_help, op3_help, op4_help, op5_help, 
                 op6_help, op7_help, op8_help, op9_help)
# filter op hours to non-deceased, age 70+, dementia SPs
op_hours_dem = merge(op_hours, SP_dem70, by = c("year", "spid"))

fwrite(op_hours_dem, paste0(FILEPATH,"OP_caregiving_hours.csv"))

sp_hours = data.table(op_hours %>% group_by(year, spid) %>% 
  summarise(care_hours_week = sum(care_hours_week)))

# merge caregiving hours from OP data with dementia status and weights from SP data
# by sample person unique ID and year
sp_hours_dem = merge(sp_hours, SP_dem70, by = c("year","spid"))

fwrite(sp_hours_dem, paste0(FILEPATH,"SP_caregiving_hours.csv"))

