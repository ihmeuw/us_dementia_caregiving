##########################################################################
### Author: Amy Lastuka
### Date: 03/24/2023
### Project: US dementia spending
### Purpose: create age groups for 65+ and 85+ by state/year
#            These are tested as covariates for our stage 1 model
#            of caregiving hours
##########################################################################


rm(list=ls())

library(haven)
library(data.table)
library("cdlTools", lib.loc = "FILEPATH/rlibs")
library(usmap)
library(ggplot2)
library(foreign)
library(pbapply)
library(ggpubr)
library(stargazer)

source("FILEPATH/get_location_metadata.R")
source("FILEPATH/get_population.R")

data_dir <- "FILEPATH"
release <- 6 # GBD 2019 iterative 
gbd_round <- 6 # GBD 2019
gbd_decomp <- 'iterative'

# load in location set 
locs <- get_location_metadata(location_set_id = 35, gbd_round_id = gbd_round, release_id = release)
locs <- locs[(location_id==102) | (parent_id == 102), .(location_name, location_id)] 

########### pull specific age groups
all_ages <- get_population(release_id = release, location_id = -1, year_id = -1, age_group_id = 22)[
  location_id %in% unique(locs$location_id)][year_id %in% seq(2010, 2019)]

over_65 <- get_population(release_id = release, location_id = -1, year_id = -1, age_group_id = 154)[
  location_id %in% unique(locs$location_id)][year_id %in% seq(2010, 2019)]

setnames(over_65,"population","pop65")

pop_dt <- merge(over_65,all_ages, by = c("location_id","year_id","sex_id","run_id"))
pop_dt <- data.table(pop_dt)
pop_dt[, frac_over_65 := pop65/population]

########### 85+ is not a single group in shared functions, need to sum a few groups 
pop_85_plus <- get_population(release_id = release, location_id = -1, year_id = -1, age_group_id = c(31,32,235))[
  location_id %in% unique(locs$location_id)][year_id %in% seq(2010, 2019)]

pop85 <- pop_85_plus[, pop85 := sum(population), by = c("location_id","year_id")]
pop85 <- pop85[ , .SD[1], by = c("location_id","year_id")]
pop85[, population:=NULL]

pop_dt <- merge(pop85,pop_dt, by = c("location_id","year_id","sex_id","run_id"))

pop_dt[, frac_over_85 := pop85/population]
pop_dt[, pop65_84 := (pop65 - pop85)/population]

pop_dt <- merge(pop_dt, locs, by="location_id")
setnames(pop_dt, c("location_name","year_id"), c("state","year"))

fwrite(pop_dt, paste0(data_dir,"age_group_fractions.csv"))
