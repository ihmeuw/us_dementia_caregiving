## Estimate annual opportunity cost of care per case from ST-GPR output by State
## Date: 01/25/2023
## Author: Michael Breshock 

# clear environment
rm(list = ls())

# load libraries 
library(ggplot2)
library(dplyr)
library(data.table)
library(tidyr)
library(usmap)
source("FILEPATH/get_location_metadata.R")


# load data:
# initialize the care hours top-code
top_code = 40
# initialize the Care Hours ST-GPR model number you want to make estimates from
hours_model_num = 4
# initialize the Care Wages ST-GPR model number you want to make estimates from
wages_model_num = 14
# initialize if this is a run with or without outliers
keep_outliers = FALSE
outlier = ifelse(keep_outliers, "_keep_outliers", "")

# initialize file paths to data: 
# st_gpr hours estimates
care_hours_path = "FILEPATH"
# attributable fraction
AF_path = "FILEPATH"
# st_gpr wages estimates
care_wages_path = "FILEPATH"

# load data:
# stgpr hours draws
stgpr_hours = fread(paste0(care_hours_path,"US_caregiving_hours_run_0",
                           hours_model_num,outlier,"_draws.csv"))
# attributable fraction
AF = fread(paste0(AF_path, "attributable_fraction_draws.csv"))
# Wages from opportunity cost ST-GPR output
stgpr_wages = fread(paste0(care_wages_path, "US_wages_run_00",
                           wages_model_num,outlier,"_draws.csv"))

# merge in attributable fraction with hours: 
setnames(AF, old = "dementia", new = "dem_AF")
care_hours = merge(stgpr_hours, AF[,.(draw, dem_AF)], by = "draw")

# calculate attributable fraction of dementia by subtracting all other AFs from 1
# dementia_AF = 1 - sum(AF$AF_mean)

# calculate care hours attributable to dementia
care_hours[, dem_hours := care_hours*dem_AF]
# stgpr_hours[, ":=" (dem_lower = care_hours_lower*dementia_AF, 
#                     dem_mean = care_hours*dementia_AF,
#                     dem_upper = care_hours_upper*dementia_AF)]

# get location meta data
loc_meta = get_location_metadata(location_set_id = 22, release_id = 6)
locs = loc_meta[, .(location_id, location_name)]
# merge in location names and parent ids with STGPR output
care_hours_named = merge(care_hours,locs, by = "location_id")
stgpr_wages_named = merge(stgpr_wages,locs, by = "location_id")

# merge wages and hours
US_cost = merge(care_hours_named, stgpr_wages_named, 
                by = c("year_id", "location_id", "draw", "location_name"))

# estimate annual cost (52 weeks/year * $$$ cost/week = $$$ cost/year)
US_cost[, ":=" (cost_total = care_hours*wage*52,
                cost_attr = dem_hours*wage*52)] 

# save out annual opportunity cost
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

OC_map = ggplot(data = cost_map, aes(fill = cost_mean, geometry = geometry)) + 
  geom_sf() + 
  scale_fill_continuous(low = "lightgreen", high = "darkgreen", name = "Cost", 
                        label = scales::comma) + 
  theme(legend.position = "right") + 
  theme(panel.background = element_rect(colour = "black"), 
        text = element_text(size = 14)) + # set figure font size 
  labs(title = "Annual Forgone Wage Cost per Case (2019 USD)") + 
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank(), 
        panel.border = element_rect(colour = "black", fill=NA))

# cowplot RC and OC maps together: 
# load RC map
load(file = paste0(FILEPATH,"rc_map112",outlier,".rda"))

# save to pdf: 
pdf_name = paste0(FILEPATH,"Figure1_cost_map.pdf")

pdf(width=12, height=6, file = pdf_name)
cowplot::plot_grid(RC_map, OC_map, nrow = 1, ncol = 2)
dev.off()

# save to EPS: 
eps_name = paste0(FILEPATH,"Figure1_cost_map.eps")

setEPS()
postscript(eps_name, width=12, height=6)
cowplot::plot_grid(RC_map, OC_map, nrow = 1, ncol = 2)
dev.off()

