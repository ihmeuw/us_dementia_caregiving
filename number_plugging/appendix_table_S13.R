# Number plugging - appendix table S13
# Forgone Wage Sensitivity Analysis
# Author: Michael Breshock

rm(list = ls())

# load in libraries
library(dplyr)
library(data.table)


oc_draws = fread(paste0(FILEPATH,"/annual_cost_of_care_draws_top_code_112_sensitive.csv"))

oc = oc_draws[,.(cost_mean = mean(cost_attr),
                 cost_lower = quantile(cost_attr, 0.025), 
                 cost_upper = quantile(cost_attr, 0.975)),
              by = c("year_id", "location_id", "location_name")]

# only need 2019 for this table
oc19 <- oc[year_id==2019]

# This is the values for the "Forgone Wages (112 Hours)" column of table S13
paste0(oc19$location_name, " | ", round(oc19$cost_mean), " (", 
       round(oc19$cost_lower), " - ", round(oc19$cost_upper), ")")
