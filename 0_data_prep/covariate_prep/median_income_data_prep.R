##########################################################################
### Author: Amy Lastuka
### Date: 11/2/2022
### Project: US dementia spending
### Purpose: clean/prep median income data from census
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

census_dir <- "FILEPATH"
moving_average <- 0 #if moving_average == 1, use the 3-year moving average 

# read in the GDP deflator data
gdp_deflator <- fread("FILEPATH/gdp_deflator.csv")
gdp_deflator <- gdp_deflator[, .(year, factor2019)]

########### MEDIAN INCOME - data prep ######################
# read in the median income data

if(moving_average == 0){
  median_income <- fread(paste0(census_dir,"median_income.csv"), select = c(1:29))
  # remove columns for duplicate years - remove the 2013 with the (39) footnote, that one is a clear outlier 
  median_income <- median_income[,-c(12:13,20:21) ] 
  # cut the head plus first 50 rows, which are in "current dollars" b/c it's not clear when current
  #    dollars were measured. Using 2021 dollars which started on line 65
  median_income <- median_income[-c(1:64), ]
  year_list <- c(2021:2010)
} else{

  median_income <- fread(paste0(census_dir,"h08b.csv"), select = c(1:19))
  median_income <- median_income[-c(1:6), ]

  year_list <- c(2019:2013,2011:2010)
}

col_list <- c("income","st_err")
name_list <- as.vector(outer(col_list, year_list, paste, sep=" - "))
setnames(median_income,c("state", name_list))

# reshape so year is a column - first split into 'income' and 'st error' data tables
income_list <- c("state", colnames(median_income)[colnames(median_income) %like% "income"])
median_income_1 <- median_income[, ..income_list]

sterr_list <- c("state", colnames(median_income)[colnames(median_income) %like% "st_err"])
median_income_2 <- median_income[, ..sterr_list]

melt_data_1 <- melt(median_income_1, id = c("state")) 
melt_data_1$variable <- sub("income - ", "", melt_data_1$variable)
setnames(melt_data_1,c("variable","value"),c("year","income"))

melt_data_2 <- melt(median_income_2, id = c("state")) 
melt_data_2$variable <- sub("st_err - ", "", melt_data_2$variable)
setnames(melt_data_2,c("variable","value"),c("year","std_error"))

median_income_final <- merge(melt_data_1, melt_data_2, by = c("state","year"))

# the income and st error have commas in them, re-format and convert to numeric
median_income_final[, income := sub(",","",income)]
median_income_final[, income := as.numeric(income)]
median_income_final[, std_error := sub(",","",std_error)]
median_income_final[, std_error := as.numeric(std_error)]

# price-adjust median income - the census data are in 2021 dollars so adjust to 2019 dollars
adjustment_2021_2019 <- gdp_deflator[year==2021, ]$factor2019
median_income_final[, year:= as.numeric(year)]
#median_income_final <- merge(median_income_final, gdp_deflator, by = "year")

median_income_final[, income_adj:= income*adjustment_2021_2019]

# change name of United States to match the shared functions output
median_income_final[state=="United States", state:= "United States of America"]


if(moving_average==1){
  output_file <- paste0(census_dir,"median_income_prepped_3yr_avg.csv")
}else{
  output_file <- paste0(census_dir,"median_income_prepped.csv")
}
fwrite(median_income_final,output_file)

