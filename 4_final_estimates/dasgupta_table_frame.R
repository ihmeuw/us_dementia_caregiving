## Create Dasgupta table framework 
## 02/21/2023
## Author: Michael Breshock
### Purpose: create table with everything needed to perform the 
##       Das Gupta decomposition. 
##          - care hours (scaled by attributable fraction)
##          - poplution (both total pop and 65+)
##          - cost per hour (either HHA wage or OC model wage)
##          - dementia prevalence
##########################################################################

rm(list = ls())

# load in libraries
library(dplyr)
library(data.table)
source("FILEPATH/get_location_metadata.R")
source("FILEPATH/get_population.R")


run_num_168 <- "0049"
run_num_112 <- "004"
run_num_40 <- "04"
run_num_wage <- "0014" #this run number corresponds to a top-code of 40 hours
#output_fname <- "dasgupta_input_table.csv"
# changing to new version for the RTR since we are now using different hours top-coding for the RC and OC models
output_fname <- "dasgupta_input_table_rtr.csv"

## ------ read in data: hours, attributable fraction, wages, and dem prevalence --------------
# RC stgpr output means (caregiving hours, average of all draws)
cg_hours_168 = fread(paste0(FILEPATH,"US_caregiving_hours_run_",run_num_168,".csv"))
cg_hours_112 = fread(paste0(FILEPATH,"US_caregiving_hours_run_",run_num_112,".csv"))
cg_hours_40 = fread(paste0(FILEPATH,"US_caregiving_hours_run_",run_num_40,".csv"))

# attributable fraction
AF = fread(paste0(FILEPATH,"attributable_fraction_dep_hbp_stroke.csv"))
# Home Health Aide BLS Wages W/ Genworth Cost Markup
hha = fread(paste0(FILEPATH,"HHA_wages_cost_PPA_2019.csv"))
# Wages from opportunity cost ST-GPR output
OC_wages = fread(paste0(FILEPATH,"US_wages_run_",run_num_wage,".csv"))
# Dementia Prevalence + Cases
dementia_prevalence = fread(FILEPATH,"rate_and_total_cases_1990_2019.csv"))
## ----------------------------------

# get 65+ population
pop65 <- get_population(release_id = 6, location_id = c(102, 523:573),
                         year_id = 2019, sex_id = 3,
                         age_group_id = 154)
setnames(pop65,"population","pop65plus")
pop65 <- pop65[, .(location_id, year_id,pop65plus)]

# calculate attributable fraction of dementia (1 - all other attributable fractions)
AF_dementia = 1 - sum(AF$AF_mean)

# multiply care giving hours by attributable fraction (hours attributed to dementia)
cg_hours_168[, ":=" (
  caregiving_AF_lower_168 = care_hours_lower*AF_dementia,
  caregiving_AF_168 = care_hours*AF_dementia,
  caregiving_AF_upper_168 = care_hours_upper*AF_dementia
)]

cg_hours_112[, ":=" (
  caregiving_AF_lower_112 = care_hours_lower*AF_dementia,
  caregiving_AF_112 = care_hours*AF_dementia,
  caregiving_AF_upper_112 = care_hours_upper*AF_dementia
)]

cg_hours_40[, ":=" (
  caregiving_AF_lower_40 = care_hours_lower*AF_dementia,
  caregiving_AF_40 = care_hours*AF_dementia,
  caregiving_AF_upper_40 = care_hours_upper*AF_dementia
)]

# filter dementia prevalence data to year 2019
dem2019 = dementia_prevalence[year_id == 2019]

# get weekly per-case caregiving hours by state and in 2019
US_hours_168 = cg_hours_168[location_id %in% c(102,523:573) & year_id == 2019]
US_hours_112 = cg_hours_112[location_id %in% c(102,523:573) & year_id == 2019]
US_hours_40 = cg_hours_40[location_id %in% c(102,523:573) & year_id == 2019]


# merge in all sets of caregiving hours with population + dementia prevalence
df_table = merge(dem2019[,.(year_id, location_id, prevalence, population)], 
                 US_hours_168[,.(year_id, location_id, caregiving_AF_168, caregiving_AF_lower_168, caregiving_AF_upper_168)],
                 by = c("year_id", "location_id"))

df_table = merge(df_table,
                 US_hours_112[,.(year_id, location_id, caregiving_AF_112, caregiving_AF_lower_112, caregiving_AF_upper_112)],
                 by = c("year_id", "location_id"))

df_table = merge(df_table,
                 US_hours_40[,.(year_id, location_id, caregiving_AF_40, caregiving_AF_lower_40, caregiving_AF_upper_40)],
                 by = c("year_id", "location_id"))

# merge in the 65+ population
df_table <- merge(df_table, pop65, by = c("year_id", "location_id"))

# add state names to table to merge in with HHA data
# get location meta data
loc_meta = get_location_metadata(location_set_id = 22, release_id = 6)
locs = loc_meta[, .(location_id, lancet_label)]
# change lancet_label column name to "state"
setnames(locs, old = "lancet_label", "State")
# merge state names in with current data table
df_table = merge(df_table, locs, by = "location_id")

# get US replacement cost (home health aide wages) for 2019
# change name of DC in HHA data to match with GBD 
hha[, State := case_when(
  STATE == "District of Columbia" ~ "Washington, DC",
  TRUE ~ STATE
)]
# change name of year column to match with other data
setnames(hha, old = "YEAR", new = "year_id")
# merge hha wages in with current data table
df_table = merge(df_table, hha[,.(year_id, State, H_COST_PPA2019)], 
                 by = c("year_id", "State"))
# change name of replacement cost column
setnames(df_table, old = "H_COST_PPA2019", new = "Replacement_Cost_Per_Hour")

# merge in opportunity cost data
df_table = merge(df_table, OC_wages[,.(year_id, location_id, wages)], 
                 by = c("year_id", "location_id"))
# change name of opportunity cost column
setnames(df_table, old = "wages", new = "Opportunity_Cost_Per_Hour")

# add columns for total cost = population*prevalence*caregiving hours per week*cost per hour*52 weeks per year
# updated this to use 112 for RC and 40 for OC, these are our new defaults for the RTR
df_table[, ":=" (
  Total_Replacement_Cost = population*prevalence*caregiving_AF_112*Replacement_Cost_Per_Hour*52, # multiply by 52 to convert from weekly to annual
  Total_Replacement_Cost_lower = population*prevalence*caregiving_AF_lower_112*Replacement_Cost_Per_Hour*52,
  Total_Replacement_Cost_upper = population*prevalence*caregiving_AF_upper_112*Replacement_Cost_Per_Hour*52,
  Total_Opportunity_Cost = population*prevalence*caregiving_AF_40*Opportunity_Cost_Per_Hour*52,
  Total_Opportunity_Cost_lower = population*prevalence*caregiving_AF_lower_40*Opportunity_Cost_Per_Hour*52,
  Total_Opportunity_Cost_upper = population*prevalence*caregiving_AF_upper_40*Opportunity_Cost_Per_Hour*52
)]

# remove unneccesary columns now
final_table = df_table[,":=" (
  year_id = NULL,
  location_id = NULL
)]
# change caregiving hours column name <- why didn't I just name them this in the first place?! 
setnames(final_table, old = c("caregiving_AF_168", "caregiving_AF_lower_168", "caregiving_AF_upper_168","caregiving_AF_112", "caregiving_AF_lower_112", "caregiving_AF_upper_112"),
         new = c("care_hours_week_168", "care_hours_week_lower_168", "care_hours_week_upper_168","care_hours_week_112", "care_hours_week_lower_112", "care_hours_week_upper_112"))
setnames(final_table, old = c("caregiving_AF_40", "caregiving_AF_lower_40", "caregiving_AF_upper_40"),
         new = c("care_hours_week_40", "care_hours_week_lower_40", "care_hours_week_upper_40"))

# save out table for amy to add dasgupta results later
fwrite(final_table, file = paste0(FILEPATH,output_fname))




