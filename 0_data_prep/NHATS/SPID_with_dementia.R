# Create list of NHATS SPIDs with dementia
# 01/11/2023
# Author: Michael Breshock

setwd("FILEPATH")
rm(list = ls())

# load in libraries
library(dplyr)
library(data.table)
library(haven)
source("NHATS_model_inputs/classify_dementia_NHATS.R")



# NHATS Public Data
nhats2011 = data.table(read_sas(paste0(FILEPATH,"NHATS_Round_1_SP_File.sas7bdat")))
nhats2012 = data.table(read_sas(paste0(FILEPATH,"NHATS_Round_2_SP_File_v2.sas7bdat")))
nhats2013 = data.table(read_sas(paste0(FILEPATH,"NHATS_Round_3_SP_File.sas7bdat")))
nhats2014 = data.table(read_sas(paste0(FILEPATH,"NHATS_Round_4_SP_File.sas7bdat")))
nhats2015 = data.table(read_sas(paste0(FILEPATH,"NHATS_Round_5_SP_File_v2.sas7bdat")))
nhats2016 = data.table(read_sas(paste0(FILEPATH,"NHATS_Round_6_SP_File_V2.sas7bdat")))
nhats2017 = data.table(read_sas(paste0(FILEPATH,"NHATS_Round_7_SP_File.sas7bdat")))
nhats2018 = data.table(read_sas(paste0(FILEPATH,"NHATS_Round_8_SP_File.sas7bdat")))
nhats2019 = data.table(read_sas(paste0(FILEPATH,"NHATS_Round_9_SP_File.sas7bdat")))

# aggregate NHATS data
# add year column
nhats2011[,year := 2011]
nhats2012[,year := 2012]
nhats2013[,year := 2013]
nhats2014[,year := 2014]
nhats2015[,year := 2015]
nhats2016[,year := 2016]
nhats2017[,year := 2017]
nhats2018[,year := 2018]
nhats2019[,year := 2019]

vars = c("year", "spid", "dementia")
# classifying dementia in SPs from NHATS and extracting columns of interest
nhats1dem = classify_dementia_NHATS(nhats2011, round_num = 1)[, .SD, .SDcols = vars]
nhats2dem = classify_dementia_NHATS(nhats2012, round_num = 2)[, .SD, .SDcols = vars]
nhats3dem = classify_dementia_NHATS(nhats2013, round_num = 3)[, .SD, .SDcols = vars]
nhats4dem = classify_dementia_NHATS(nhats2014, round_num = 4)[, .SD, .SDcols = vars]
nhats5dem = classify_dementia_NHATS(nhats2015, round_num = 5)[, .SD, .SDcols = vars]
nhats6dem = classify_dementia_NHATS(nhats2016, round_num = 6)[, .SD, .SDcols = vars]
nhats7dem = classify_dementia_NHATS(nhats2017, round_num = 7)[, .SD, .SDcols = vars]
nhats8dem = classify_dementia_NHATS(nhats2018, round_num = 8)[, .SD, .SDcols = vars]
nhats9dem = classify_dementia_NHATS(nhats2019, round_num = 9)[, .SD, .SDcols = vars]

# binding NHATS rounds 1, 5, 7 together 
NHATS = rbind(nhats1dem, nhats2dem, nhats3dem, nhats4dem, nhats5dem, 
              nhats6dem, nhats7dem, nhats8dem, nhats9dem)

# creating list of unique SPIDs that have dementia from 2011-2019
sp_w_dem = NHATS[dementia == 1]

fwrite(sp_w_dem, paste0(FILEPATH,"SPID_with_dementia.csv"))
