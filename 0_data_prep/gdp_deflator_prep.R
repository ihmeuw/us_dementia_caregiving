


##########################################################################
### Author: Amy Lastuka
### Date: 10/25/2022
### Project: US dementia spending
### Purpose: create multiplier to convert every year to 2019 dollars
##########################################################################


rm(list=ls())


if (Sys.info()["sysname"] == "Linux") {
  j_root <- "/home/j/" 
  h_root <- "~/"
  l_root <- "/ihme/limited_use/"
  functions_dir <- "/ihme/cc_resources/libraries/current/r/"
} else { 
  j_root <- "J:/"
  h_root <- "H:/"
  l_root <- "L:/"
  functions_dir <- "K:/libraries/current/r/"
}


library(data.table)

gdp_deflator <- fread(paste0(j_root,"Project/IRH/Informal_Care_AD/US_dementia_spending_2022/data/BEA/BEA GDP deflator.csv"))

setnames(gdp_deflator,"A191RD3A086NBEA","index")

base_index <- gdp_deflator[DATE=="2019-01-01", index]

gdp_deflator[, factor2019:= base_index/index]

gdp_deflator[, year:=year(DATE)]

fwrite(gdp_deflator,paste0(j_root,"Project/IRH/Informal_Care_AD/US_dementia_spending_2022/data/BEA/gdp_deflator.csv"))
