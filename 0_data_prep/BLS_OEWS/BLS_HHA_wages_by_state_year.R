## BLS Home Health Aides Mean Wage by State
## Date: 07/14/2022
## Author: Michael Breshock 

# clear environment
rm(list = ls())

# load libraries
library(ggplot2)
library(readxl)
library(dplyr)
library(data.table)
library(plotly)
library(tidyr)
library(zoo)
library(usmap)


# initialize path to BLS data 
path_to_data = "FILEPATH"

# load data: 
# each year is in a separate file
oesm10 = data.table(read_xls(paste0(path_to_data, "/2010/USA_OEWS_2010_STATE_Y2021M10D21.xls")))
oesm11 = data.table(read_xls(paste0(path_to_data, "/2011/USA_OEWS_2011_STATE_Y2021M10D21.xls")))
oesm12 = data.table(read_xls(paste0(path_to_data, "/2012/USA_OEWS_2012_STATE_Y2021M10D21.xls")))
oesm13 = data.table(read_xls(paste0(path_to_data, "/2013/USA_OEWS_2013_STATE_Y2021M10D21.xls")))
oesm14 = data.table(read_xlsx(paste0(path_to_data, "/2014/USA_OEWS_2014_STATE_Y2021M10D21.xlsx")))
oesm15 = data.table(read_xlsx(paste0(path_to_data, "/2015/USA_OEWS_2015_STATE_Y2021M10D21.xlsx")))
oesm16 = data.table(read_xlsx(paste0(path_to_data, "/2016/USA_OEWS_2016_STATE_Y2021M10D21.xlsx")))
oesm17 = data.table(read_xlsx(paste0(path_to_data, "/2017/USA_OEWS_2017_STATE_Y2021M10D21.xlsx")))
oesm18 = data.table(read_xlsx(paste0(path_to_data, "/2018/USA_OEWS_2018_STATE_Y2021M10D21.xlsx")))
# changing column names to match previous years of data
oesm19 = data.table(read_xlsx(paste0(path_to_data, "/2019/USA_OEWS_2019_STATE_Y2021M10D21.xlsx"))) %>%
  rename(STATE = area_title) %>% rename_with(toupper)
oesm20 = data.table(read_xlsx(paste0(path_to_data, "/2020/USA_OEWS_2020_STATE_Y2021M10D21.xlsx"))) %>%
  rename(STATE = AREA_TITLE)
oesm21 = data.table(read_xlsx(paste0(path_to_data, "/2021/USA_OEWS_2021_STATE_Y2022M06D29.xlsx"))) %>%
  rename(STATE = AREA_TITLE)

#-------------------------------------------------------------------------------
# fill in missing values for each year of data: 

# 2010
# removing US territories, filtering for occupations of interest,
# selecting variables of interest
oesm10 = oesm10[OCC_TITLE %in% c("Home Health Aides", "Personal Care Aides") & 
                  !STATE %in% c("Guam", "Virgin Islands", "Puerto Rico"), 
                .(STATE, OCC_TITLE, H_MEAN, MEAN_PRSE, TOT_EMP, JOBS_1000)]
# counting the total number of rows for each state
total_rows = oesm10[, .N, by = STATE]
# identifying any states with missing rows/observations
total_rows[N < 2]

# identified that Personal Care Aides data for North Dakota was missing
oesm10[STATE == "North Dakota"]
# creating a new row for this missing observation
new_row = data.table(STATE = "North Dakota", 
                     OCC_TITLE = "Personal Care Aides", 
                     H_MEAN = NA,
                     MEAN_PRSE = NA,
                     TOT_EMP = NA, 
                     JOBS_1000 = NA)
# adding the new row to the data
fill10 = rbind(oesm10, new_row)
# adding a year column
fill10[, YEAR := 2010]

# 2011
# removing US territories, filtering for occupations of interest,
# selecting variables of interest
oesm11 = oesm11[OCC_TITLE %in% c("Home Health Aides", "Personal Care Aides") & 
                  !STATE %in% c("Guam", "Virgin Islands", "Puerto Rico"), 
                .(STATE, OCC_TITLE, H_MEAN, MEAN_PRSE, TOT_EMP, JOBS_1000)]
# counting the total number of rows for each state
total_rows = oesm11[, .N, by = STATE]
# identifying any states with missing rows/observations
total_rows[N < 2]

# no missing rows identified, adding year column to data
fill11 = oesm11[, YEAR := 2011]

# 2012
# removing US territories, filtering for occupations of interest,
# selecting variables of interest
oesm12 = oesm12[OCC_TITLE %in% c("Home Health Aides", "Personal Care Aides") & 
                  !STATE %in% c("Guam", "Virgin Islands", "Puerto Rico"), 
                .(STATE, OCC_TITLE, H_MEAN, MEAN_PRSE, TOT_EMP, JOBS_1000)]
# counting the total number of rows for each state
total_rows = oesm12[, .N, by = STATE]
# identifying any states with missing rows/observations
total_rows[N < 2]

# no missing rows identified, adding year column to data
fill12 = oesm12[, YEAR:= 2012]

# 2013
# removing US territories, filtering for occupations of interest,
# selecting variables of interest
oesm13 = oesm13[OCC_TITLE %in% c("Home Health Aides", "Personal Care Aides") & 
                  !STATE %in% c("Guam", "Virgin Islands", "Puerto Rico"), 
                .(STATE, OCC_TITLE, H_MEAN, MEAN_PRSE, TOT_EMP, JOBS_1000)]
# counting the total number of rows for each state
total_rows = oesm13[, .N, by = STATE]
# identifying any states with missing rows/observations
total_rows[N < 2]

# no missing rows identified, adding year column to data
fill13 = oesm13[, YEAR := 2013]

# 2014
# removing US territories, filtering for occupations of interest,
# selecting variables of interest
oesm14 = oesm14[OCC_TITLE %in% c("Home Health Aides", "Personal Care Aides") & 
                  !STATE %in% c("Guam", "Virgin Islands", "Puerto Rico"), 
                .(STATE, OCC_TITLE, H_MEAN, MEAN_PRSE, TOT_EMP, JOBS_1000)]
# counting the total number of rows for each state
total_rows = oesm14[, .N, by = STATE]
# identifying any states with missing rows/observations
total_rows[N < 2]

# identified that Personal Care Aides data for Vermont was missing
oesm14[STATE == "Vermont"]
# creating a new row for this missing observation
new_row = data.table(STATE = "Vermont", 
                     OCC_TITLE = "Personal Care Aides", 
                     H_MEAN = NA,
                     MEAN_PRSE = NA,
                     TOT_EMP = NA, 
                     JOBS_1000 = NA)
# adding the new row to the data
fill14 = rbind(oesm14, new_row)
# adding a year column
fill14[, YEAR := 2014]

# 2015
# removing US territories, filtering for occupations of interest,
# selecting variables of interest
oesm15 = oesm15[OCC_TITLE %in% c("Home Health Aides", "Personal Care Aides") & 
                  !STATE %in% c("Guam", "Virgin Islands", "Puerto Rico"), 
                .(STATE, OCC_TITLE, H_MEAN, MEAN_PRSE, TOT_EMP, JOBS_1000)]
# counting the total number of rows for each state
total_rows = oesm15[, .N, by = STATE]
# identifying any states with missing rows/observations
total_rows[N < 2]

# identified that Personal Care Aides data for Vermont was missing
oesm15[STATE == "Vermont"]
# creating a new row for this missing observation
new_row = data.table(STATE = "Vermont", 
                     OCC_TITLE = "Personal Care Aides", 
                     H_MEAN = NA,
                     MEAN_PRSE = NA,
                     TOT_EMP = NA, 
                     JOBS_1000 = NA)
# adding the new row to the data
fill15 = rbind(oesm15, new_row)
# adding a year column
fill15[, YEAR := 2015]

# 2016
# removing US territories, filtering for occupations of interest,
# selecting variables of interest
oesm16 = oesm16[OCC_TITLE %in% c("Home Health Aides", "Personal Care Aides") & 
                  !STATE %in% c("Guam", "Virgin Islands", "Puerto Rico"), 
                .(STATE, OCC_TITLE, H_MEAN, MEAN_PRSE, TOT_EMP, JOBS_1000)]
# counting the total number of rows for each state
total_rows = oesm16[, .N, by = STATE]
# identifying any states with missing rows/observations
total_rows[N < 2]

# no missing rows identified, adding year column to data
fill16 = oesm16[, YEAR := 2016]

# 2017
# removing US territories, filtering for occupations of interest,
# selecting variables of interest
oesm17 = oesm17[OCC_TITLE %in% c("Home Health Aides", "Personal Care Aides") & 
                  !STATE %in% c("Guam", "Virgin Islands", "Puerto Rico"), 
                .(STATE, OCC_TITLE, H_MEAN, MEAN_PRSE, TOT_EMP, JOBS_1000)]
# counting the total number of rows for each state
total_rows = oesm17[, .N, by = STATE]
# identifying any states with missing rows/observations
total_rows[N < 2]

# no missing rows identified, adding year column to data
fill17 = oesm17[, YEAR := 2017]

# 2018
# removing US territories, filtering for occupations of interest,
# selecting variables of interest
oesm18 = oesm18[OCC_TITLE %in% c("Home Health Aides", "Personal Care Aides") & 
                  !STATE %in% c("Guam", "Virgin Islands", "Puerto Rico"), 
                .(STATE, OCC_TITLE, H_MEAN, MEAN_PRSE, TOT_EMP, JOBS_1000)]
# counting the total number of rows for each state
total_rows = oesm18[, .N, by = STATE]
# identifying any states with missing rows/observations
total_rows[N < 2]

# no missing rows identified, adding year column to data
fill18 = oesm18[, YEAR := 2018]

# 2019
# removing US territories, filtering for occupations of interest,
# selecting variables of interest
oesm19 = oesm19[OCC_TITLE %in% c("Home Health and Personal Care Aides") & 
                  !STATE %in% c("Guam", "Virgin Islands", "Puerto Rico"), 
                .(STATE, OCC_TITLE, H_MEAN, MEAN_PRSE, TOT_EMP, JOBS_1000)]
# counting the total number of rows for each state
total_rows = oesm19[, .N, by = STATE]
# identifying any states with missing rows/observations
total_rows[N < 1]

# no missing rows identified, adding year column to data
fill19 = oesm19[, YEAR := 2019]

# 2020 
# removing US territories, filtering for occupations of interest,
# selecting variables of interest
oesm20 = oesm20[OCC_TITLE %in% c("Home Health and Personal Care Aides") & 
                  !STATE %in% c("Guam", "Virgin Islands", "Puerto Rico"), 
                .(STATE, OCC_TITLE, H_MEAN, MEAN_PRSE, TOT_EMP, JOBS_1000)]
# counting the total number of rows for each state
total_rows = oesm20[, .N, by = STATE]
# identifying any states with missing rows/observations
total_rows[N < 1]

# no missing rows identified, adding year column to data
fill20 = oesm20[, YEAR := 2020]

# 2021
# removing US territories, filtering for occupations of interest,
# selecting variables of interest
oesm21 = oesm21[OCC_TITLE %in% c("Home Health and Personal Care Aides") & 
                  !STATE %in% c("Guam", "Virgin Islands", "Puerto Rico"), 
                .(STATE, OCC_TITLE, H_MEAN, MEAN_PRSE, TOT_EMP, JOBS_1000)]
# counting the total number of rows for each state
total_rows = oesm21[, .N, by = STATE]
# identifying any states with missing rows/observations
total_rows[N < 1]

# no missing rows identified, adding year column to data
fill21 = oesm21[, YEAR := 2021]

#-------------------------------------------------------------------------------
# combining 2010-2018 data, Filling NA values, and combining home health aides
# and personal care categories

# combining filled 2010-2018 data
wages18 = rbind(fill10, fill11, fill12, fill13, fill14, fill15, fill16,
                fill17, fill18)
# ordering by state and occupation title
wages18 = wages18[order(STATE, OCC_TITLE)]
# converting character columns to numeric
wages18[, `:=` (H_MEAN = as.numeric(H_MEAN), 
                MEAN_PRSE = as.numeric(MEAN_PRSE),
                TOT_EMP = as.numeric(TOT_EMP),
                JOBS_1000 = as.numeric(JOBS_1000))]
# filling NA values using an approximate interpolation
filledwages18 = wages18 %>% group_by(STATE) %>%
  mutate(H_MEAN = na.approx(H_MEAN),
         MEAN_PRSE = na.approx(MEAN_PRSE),
         TOT_EMP = na.approx(TOT_EMP),
         JOBS_1000 = na.approx(JOBS_1000))

# combining home health aides and personal care aides 
# into a single occupation category
groupedwages18 = filledwages18 %>%
  group_by(STATE, YEAR) %>% # grouping by state and year
  summarise(OCC_TITLE = "Home Health and Personal Care Aides", # changing to new grouped title name
            pop = sum(TOT_EMP), # summing the population sample sizes 
            hourly_mean = weighted.mean(H_MEAN, TOT_EMP), # taking weighted average of the hourly mean,
            prse = weighted.mean(MEAN_PRSE, TOT_EMP), # percent relative standard error
            j1000 = weighted.mean(JOBS_1000, TOT_EMP)) %>%  # Jobs per 1000
  rename(H_MEAN = hourly_mean, MEAN_PRSE = prse, TOT_EMP = pop, JOBS_1000 = j1000) # renaming to original col names

# combining grouped 2010-2018 data with the 2019-2021 data
# combining 19-21 data into single data.table
wages21 = rbind(fill19, fill20, fill21)
# converting columns to numeric type
wages21[, `:=` (H_MEAN = as.numeric(H_MEAN), 
                MEAN_PRSE = as.numeric(MEAN_PRSE),
                TOT_EMP = as.numeric(TOT_EMP),
                JOBS_1000 = as.numeric(JOBS_1000))]

# creating new data.table of all years combined
wage = data.table(rbind(groupedwages18, wages21))
# ordering data.table by state and year
setorder(wage, STATE, YEAR)

# verify that data is complete now: 
# counting total number of NA values
wage[, sum(is.na(.SD)),  .SDcols = c("TOT_EMP", "H_MEAN", "MEAN_PRSE", "JOBS_1000")]
# sum = 0

# counting number of observations per year
wage[, .N, by = YEAR]
# 51 observations for every year -> 50 states + DC 
# NO missing data

#-------------------------------------------------------------------------------
# get the average wage markup for the years we have from Genworth Cost of Care 

# loading in Genworth HHA cost data for 2012, 2015, 2018, 2021
# the `wage` dataframe used here is the data without rpp-adjustment
hha_costs = fread(paste0(FILEPATH,"genworth_homehealthaide_costs.csv"))
hha_costs[, V4 := NULL] # deleting empty column
hha_costs[, hourly_rate := na.approx(hourly_rate)] #  replacing NAs by interpolation
wage_chunks = wage[YEAR %in% c(2012,2015,2018,2021), .(STATE, H_MEAN, YEAR)] # grabbing data from the years we have from Genworth
wage_cost = merge(wage_chunks, hha_costs, by = c("STATE", "YEAR")) # merging wage and cost data
avg_markup = wage_cost %>% group_by(STATE, YEAR) %>% summarise(markup = hourly_rate / H_MEAN) # calculating avg markup by state and year
fitted_markup_data = data.table(wage %>% group_by(STATE, YEAR) %>% select(STATE,YEAR)) # creating new datatable to run predictions on 
avg_markup %>% group_by(YEAR) %>% summarise(national_markup = mean(markup)) # checking national averages by year

# saving these objects to RDA file to use later. 
save(avg_markup, fitted_markup_data, file = paste0(FILEPATH,"HHA_markup.rda"))


#-------------------------------------------------------------------------------
## create draws of HHA wages using standard error
# BLS provides PRSE values along with their estimates
# PRSE: Percent relative standard error 
# PRSE = SE/estimate * 100
# SE = estimate*PRSE/100

# calculate standard error from PRSE: 
wage[, std_err := H_MEAN*MEAN_PRSE/100]

wage_draws = data.table() # initialize empty variable to store draws in
# create 1000 draws for each year/state combo:
for (state in unique(wage$STATE)){
  for (yr in unique(wage$YEAR)){
    # filter wage data to the state & year in question
    state_yr = wage[STATE == state & YEAR == yr]
    # create 1000 draws of the hourly wage mean:
    h_mean_draws = rnorm(1000, mean = state_yr$H_MEAN, sd = state_yr$std_err)
    # save draws to data.table and preserve other metrics: 
    state_yr_draws = data.table(STATE = state, YEAR = yr,
                               OCC_TITLE = "Home Health and Personal Care Aides",
                               H_MEAN = h_mean_draws,
                               draw = paste0("draw_", c(0:999)),
                               TOT_EMP = state_yr$TOT_EMP, 
                               MEAN_PRSE = state_yr$MEAN_PRSE, 
                               JOBS_1000 = state_yr$JOBS_1000, 
                               std_err = state_yr$std_err)
    wage_draws = rbind(wage_draws, state_yr_draws)
  }
}

#-------------------------------------------------------------------------------
# price parity adjustments of wage data and saving .csv

# scale federal_adders by RPP
load(FILEPATH,"states.RData")
state_vec <- states$location_id; names(state_vec) <- states$state_name
rpp = fread(paste0(FILEPATH,"BEA Regional Price Parity by State - All Goods - 2008-2019.csv"))
rpp <- melt(rpp, id.vars = c("GeoFips", "GeoName"), variable.name = "year_id", value.name = "rpp")
rpp <- rpp[GeoName != "United States", .(state_name = GeoName, year_id = as.integer(as.character(year_id)), rpp)]
rpp[,location_id := as.integer(plyr::revalue(state_name, state_vec))]
rpp[,rpp := as.numeric(rpp)/100]
rpp = rename(rpp, STATE = state_name, YEAR = year_id)

wage_rpp = merge(wage_draws, rpp, by = c("STATE", "YEAR"))
wage_rpp[, H_MEAN_PPA := H_MEAN / rpp]

fwrite(wage_rpp, file = paste0(FILEPATH,"BLS_HHA_wages_2010_2019_draws.csv"))
