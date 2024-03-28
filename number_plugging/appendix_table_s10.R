## Number plugging - Table S10 in appendix
## Date: 10/24/23
## Author: Amy Lastuka

# clear environment
rm(list = ls())

# load libraries 
library(ggplot2)
library(dplyr)
library(data.table)

# # # # # replacement cost - means and CIs # # # # # 
rc_draws <- fread(paste0(FILEPATH, "annual_cost_of_care_draws_top_code_112.csv"))


rc = rc_draws[,.(cost_mean = mean(cost_attr),
                 cost_lower = quantile(cost_attr, 0.025), 
                 cost_upper = quantile(cost_attr, 0.975)),
              by = c("year_id", "location_id", "location_name")]

# only need 2019 for this table
rc <- rc[year_id==2019,]

# This is the values for the "Replacement Cost" column of table S10
paste0(rc$location_name, " | ", round(rc$cost_mean), " (", 
       round(rc$cost_lower), " - ", round(rc$cost_upper), ")")

# # # # # forgone wage cost - means and CIs # # # # # 
oc_draws <- fread(paste0(FILEPATH, "annual_cost_of_care_draws_top_code_40.csv"))

oc = oc_draws[,.(cost_mean = mean(cost_attr),
                 cost_lower = quantile(cost_attr, 0.025), 
                 cost_upper = quantile(cost_attr, 0.975)),
              by = c("year_id", "location_id", "location_name")]

# only need 2019 for this table
oc <- oc[year_id==2019,]

# This is the values for the "Forgone Wage" column of table S10
paste0(oc$location_name, " | ", round(oc$cost_mean), " (", 
       round(oc$cost_lower), " - ", round(oc$cost_upper), ")")

