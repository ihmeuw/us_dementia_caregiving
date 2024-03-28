### Calculating Dementia Prevalance from NHATS
### Author: Michael Breshock
### Date: 11/11/2022
library(data.table)
library("cpsR", lib.loc = "FILEPATH")



# Must register with CPS to get an API key to make calls to the API
APIKEY = "KEY" # used in cpsR function

# variables of interest
variables = c("PEERNRT", "PEERNHRY", "PTERNHLY", "PTERNWA", "GESTFIPS",
              "PEERNPER", "HUFINAL", "PEERNUOT", "PRHRUSL", "PRFTLF", "PRWKSCH", "PRWKSTAT",
              "PEERNHRO", "PUNLFCK1", "PWSSWGT", "PRTAGE", "PESEX", "PEEDUCA")
# the get_basic() function from cpsR requires you to specify which variables
# you want to download, it will not download every variable for you by default. 

# months to iterate through
months = c(1,2,3,4,5,6,7,8,9,10,11,12) # Jan - December
# years to iterate through
years = c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019,
          2020, 2021) # 2010 - 2021

cps = data.table() # initiate empty data table to store data in

# for each year and month, call the API for basic monthly data
for (year in years) {
  for (month in months) {
    monthlydata = data.table(get_basic(year, month, variables, key = APIKEY))
    monthlydata[, `:=` (year = year, month = month)]
    cps = rbind(cps, monthlydata) # save monthly data to combined dataset 
  }
}
# note: Sometimes the get_basic() function will fail because the variable 
# you want is not available with this method. 

# Sometimes the function will fail completely on its own due to no user error,
# possibly due to CPS server issues. If the code fails and you are confident 
# the variables you are requesting are compatible with the function, simply try again. 

# saving combined data
fwrite(cps, paste0(FILEPATH,"monthly_cps_update_CURRENT_DATE.csv"))



