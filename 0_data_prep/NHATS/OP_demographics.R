## Script to process and extract demographic variables from NHATS OP data
## 01/17/2023
## Author: Michael Breshock

setwd("FILEPATH")
rm(list = ls())

# load in libraries
library(dplyr)
library(data.table)
library(tidyr)
library(haven)
library(zoo)


# load round 9 OP data
op9 = data.table(read_sas(paste0(FILEPATH,"NHATS_Round_9_OP_File.sas7bdat")))

# wrangle sex data 

# getting all sex variables and SP + OP unique identifiers 
sex_names = grep("gender$", names(op9), value = T)
sex_vars = c("spid", "opid", sex_names)
sex = op9[, .SD, .SDcols = sex_vars] 

# create new sex variable that is the maximum value of all rounds of sex data
# will assume sex won't change, and with Male = 1, Female = 2, and NA = -1 or < 0 
# so if OP answered sex question in any round, that answer will always be greater than NA response
# and if answered in multiple rounds, those answers should all be the same (either 1 or 2)
sex[, sex := pmax(eval(parse(text = sex_names)))]

# create new df with just spid, opid, and final sex variable 
sex_df = sex[, .(spid, opid, sex)]
sex_df[sex < 0]$sex <- NA # assign NA to missing values 

# wrangle age data

# getting all age variables 
age_names = grep("dage$", names(op9), value = T)
age_vars = c("spid", "opid", age_names)
age = op9[, .SD, .SDcols = age_vars]

# pivot longer to have each round as a different row instead of column 
age_long = pivot_longer(age, cols = age_names, values_to = "age", names_to = "round")
setDT(age_long) # convert long format to data.table

# create year variable based on round name/number
age_long[, year := 2010 + as.numeric(substr(round,3,3))] # year = 2010 + round number

# fill NA values forwards and backwards by spid and opid 
age_long[age < 0]$age <- NA # set missing values to NA
age_long[,age_fill:=na.locf(age,na.rm=FALSE), # fill NAs forward
         by=list(spid, opid)] 
age_long[,age_fill:=na.locf(age_fill,na.rm=FALSE,fromLast=TRUE), # then fill NAs backward
         by=list(spid, opid)]

# recode age values
age_long[, age_bins := case_when(age_fill %in% c(1,2,3,4) ~ "18-34", # 1 = 18-20, 2 = 20-24, 3 = 25-29, 4 = 30-34
                                age_fill %in% c(5,6) ~ "35-44", # 5 = 35-39, 6 = 40-44
                                age_fill %in% c(7,8) ~ "45-54", # 7 = 45-49, 8 = 50-54
                                age_fill %in% c(9,10) ~ "55-64", # 9 = 55-59, 10 = 60-64
                                age_fill %in% c(11,12) ~ "65-74", # 11 = 65-69, 12 = 70-74
                                age_fill >= 13 ~ "75+", # 13 = 75-79, 14 = 80-84, 15 = 85-89, 16 = 90+
                                TRUE ~ NA_character_)]

# create new df with year, spid, opid, and final age variable 
age_df = age_long[, .(year, spid, opid, age_bins)]

# wrangle education data

# getting all education variables 
edu_names = grep("leveledu$", names(op9), value = T)
edu_vars = c("spid", "opid", edu_names)
edu = op9[, .SD, .SDcols = edu_vars]

# pivot longer to have each round as a different row instead of column 
edu_long = pivot_longer(edu, cols = edu_names, values_to = "education_raw", names_to = "round")
setDT(edu_long) # convert long format to data.table

# create year variable based on round name/number
edu_long[, year := 2010 + as.numeric(substr(round,3,3))] # year = 2010 + round number

# fill NA values forwards and backwards by spid and opid 
edu_long[education_raw < 0]$education_raw <- NA # set missing values to NA
edu_long[,educ:=na.locf(education_raw,na.rm=FALSE), # fill NAs forward
         by=list(spid, opid)] 
edu_long[,educ:=na.locf(educ,na.rm=FALSE,fromLast=TRUE), # then fill NAs backward
         by=list(spid, opid)]

# recode education values
edu_long[, education := case_when(educ >= 1 & educ <= 4 ~ "High school or less",
                                  educ >= 5 & educ <= 7 ~ "Some college",
                                  educ >= 8 ~ "Bachelor's degree or more",
                                  TRUE ~ NA_character_)]
# 1 = No school, 2 = 1st-8th grade, 3 = 9th-12th (No diploma),
# 4 = High school graduate, 5 = Trade school certificate or equivalent,
# 6 = Some college (no degree), 7 = Associate's degree, 
# 8 = Bachelor's degree, 9 = Master's, Professional, or Doctoral degree 

# create new df with year, spid, opid, and final education variable 
edu_df = edu_long[, .(year, spid, opid, education)]


# merge all demographic variables together
# merge age and sex
age_sex = merge(age_df, sex_df, by = c("spid","opid"))
# merge age, sex, and education
demographics = merge(age_sex, edu_df, by = c("year","spid","opid"))

# save demographic data to csv file
fwrite(demographics, file = paste0(FILEPATH,"OP_age_sex_education.csv"))


