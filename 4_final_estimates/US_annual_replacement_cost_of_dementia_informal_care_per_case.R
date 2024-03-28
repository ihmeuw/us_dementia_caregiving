## Estimate annual replacement cost of care per case from ST-GPR output by State
## Date: 01/23/2023
## Author: Michael Breshock 

# clear environment
rm(list = ls())

# load libraries 
library(ggplot2)
library(dplyr)
library(data.table)
library(tidyr)
library(sf)
source("FILEPATH/get_location_metadata.R")


# initialize the care hours top-code
top_code = 112
# initialize the Care Hours ST-GPR model number you want to make estimates from
model_num = 6
# initialize if this is a run with or without outliers
keep_outliers = TRUE
outlier = ifelse(keep_outliers, "_keep_outliers", "")

# initialize file paths to data: 
# st_gpr estimates
st_gpr_path = "FILEPATH"
# attributable fraction
AF_path = "FILEPATH"
# home health aide wages
hha_path = "FILEPATH"

# load data:
# stgpr output means (average of all draws)
stgpr_hours = fread(paste0(st_gpr_path,"US_caregiving_hours_run_00",
                           model_num,outlier,"_draws.csv"))
# attributable fraction
AF = fread(paste0(AF_path, "attributable_fraction_draws.csv"))
# BLS Wages W/ Genworth Cost Markup
hha = fread(paste0(hha_path, "HHA_wages_cost_PPA_2019_draws.csv"))

# merge in attributable fraction to hours: 
setnames(AF, old = "dementia", new = "dem_AF")
care_hours = merge(stgpr_hours, AF[,.(draw, dem_AF)], by = "draw")



# calculate care hours attributable to dementia
care_hours[, dem_hours := care_hours*dem_AF]

# get location meta data
loc_meta = get_location_metadata(location_set_id = 22, release_id = 6)
locs = loc_meta[, .(location_id, parent_id, location_name)]
# merge in location names and parent ids with STGPR output
US_hours = merge(care_hours,locs, by = "location_id")

# change name of USA in HHA data to match with GBD 
hha[location_id == 102, STATE := "United States of America"]
setnames(hha, c("YEAR", "STATE"), c("year_id", "location_name"))

# merge wages in with care hours
US_cost = merge(US_hours[,.(year_id, location_id, location_name,
                            draw, care_hours, dem_hours)], 
                   hha[,.(year_id, location_id, location_name, 
                          draw, H_COST_PPA2019)], 
                by = c("year_id", "location_id", "location_name", "draw"))

# estimate annual cost (52 weeks/year * $$$ cost/week = $$$ cost/year)
US_cost[, ":=" (cost_total = care_hours*H_COST_PPA2019*52, 
                cost_attr = dem_hours*H_COST_PPA2019*52)]

# save out annual replacement cost
out_path = "FILEPATH"
fwrite(US_cost, paste0(out_path, "annual_cost_of_care_draws_top_code_",
                          top_code, outlier,".csv"))

# get means from draws for plotting: 
US_means = US_cost[,.(cost_mean = mean(cost_attr), 
                      cost_lower = quantile(cost_attr, 0.025), 
                      cost_upper = quantile(cost_attr, 0.975)), 
                   by = c("year_id", "location_id", "location_name")]


# US Cost of Dementia Care Map 2019: 
smap <- readRDS("FILEPATH/state_DCinset_sf_shapefile.rds")
# change fips code variable name to allow for merging
smap = rename(smap, fips = state)

cost2019 = US_means[year_id == 2019]

# use this for number plugging: 
paste0(cost2019$location_name, " | ", round(cost2019$cost_mean), " (", 
       round(cost2019$cost_lower), " - ", round(cost2019$cost_upper), ")")

cost_map = merge(cost2019, smap, by = "location_id")

RC_map = ggplot(data = cost_map, aes(fill = cost_mean, geometry = geometry)) + 
  geom_sf() + 
  scale_fill_continuous(low = "lightblue", high = "darkblue", name = "Cost", 
                        label = scales::comma) + 
  theme(legend.position = "right") + 
  theme(panel.background = element_rect(colour = "black"), 
        text = element_text(size = 14)) + # set figure font size 
  labs(title = "Annual Replacement Cost per Case (2019 USD)") + 
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank(), 
        panel.border = element_rect(colour = "black", fill=NA)) 
  # geom_sf_text(data = cost_map, 
  #           aes(label = scales::number(care_cost_mean_AF, scale = 1e-3, accuracy = 1)),
  #           color = "black")
  # ^ use this to add numbers ^ 
# export map object: 
save(RC_map, file = paste0(FILEPATH,"rc_map112",
                           outlier,".rda"))


