# AAIC abstract diagrams: 
# RC -> OC Arrow Diagram | US Timescale Plots 
# Author: Michael Breshock
# Date: 01/26/2023

# clear environment
rm(list = ls())

# load libraries
library(ggplot2)
library(dplyr)
library(data.table)
library(tidyr)
library(viridis)
source("/ihme/cc_resources/libraries/current/r/get_location_metadata.R")

# initialize if this is a run with or without outliers
keep_outliers = TRUE
outlier = ifelse(keep_outliers, "_keep_outliers", "")

# load data
# RC file created in: US_annual_replacement_cost_of_dementia_informal_care_per_case.R
rc_draws = fread(paste0(FILEPATH,"annual_cost_of_care_draws_top_code_112",
                        outlier,".csv"))
# OC file created in: US_annual_opportunity_cost_of_dementia_informal_care_per_case.R
oc_draws = fread(paste0(FILEPATH,"annual_cost_of_care_draws_top_code_40",
                        outlier,".csv"))

# summarize RC and OC draws: 
rc = rc_draws[,.(rc = mean(cost_attr),
                 rc_lower = quantile(cost_attr, 0.025), 
                 rc_upper = quantile(cost_attr, 0.975)),
              by = c("year_id", "location_id", "location_name")]

oc = oc_draws[,.(oc = mean(cost_attr),
                 oc_lower = quantile(cost_attr, 0.025), 
                 oc_upper = quantile(cost_attr, 0.975)),
              by = c("year_id", "location_id", "location_name")]


rc_oc = merge(rc, oc, by = c("year_id", "location_name", "location_id"))

#-------------------------------------------------------------------------------
# Timescale Plots (per case costs): 


# get values for setting y axis limits
min_y = 0 # round(min(rc_oc$oc_lower), -3)
max_y = round(max(rc_oc$rc_upper), -4)

# Per-case Replacement Cost: 
timeRC = rc_oc %>% ggplot(aes(x = year_id, y = rc, ymin = rc_lower, 
                              ymax = rc_upper, color = location_name)) + 
  geom_line(data = rc_oc[location_id == 102]) + 
  geom_point(data = rc_oc[location_id == 102]) + 
  geom_ribbon(data = rc_oc[location_id == 102], alpha = 0.15) +
  geom_line(data = rc_oc[location_id != 102], alpha = 0.3) +
  labs(x = "Year", y = "Cost (2019 $USD)", 
       title = "Annual Per-Case Replacement Cost") + 
  scale_x_continuous(limits = c(2010, 2019), breaks = seq(2010, 2019, 1)) +
  theme(panel.background = element_rect(fill="white"),
        #aspect.ratio = ((1 + sqrt(5))/2)^(-1), 
        ## All axes changes
        axis.ticks.length = unit(0.5, "char"), #longer ticks
        text = element_text(size = 14), # set figure font size 
        ## Horizontal axis changes
        axis.line.x = element_line(size = 0.2), # thinner axis lines
        axis.ticks.x = element_line(size = 0.2), # thinner ticks
        axis.text.x = element_text(color = "black", size=12),
        axis.title.x = element_text(size = 12,
                                    margin = margin(t = 7.5, r = 0, b = 0, l = 0)),
        ## Vertical axis changes
        axis.line.y = element_line(size = 0.2), # thinner axis lines
        axis.ticks.y = element_blank(), # no y axis ticks (gridlines suffice)
        axis.text.y = element_text(color = "black", size=12),
        axis.title.y = element_text(size = 12,
                                    margin = margin(t = 0, r = 7.5, b = 0, l = 0)),
        ## Legend
        legend.position = 'none',
        legend.key = element_rect(fill = NA, color = NA),
        ## Gridlines
        panel.grid.major.x = element_line(color = "gray45", size = 0.1),
        panel.grid.major.y = element_line(color = "gray45", size = 0.1)
  ) + scale_color_manual(values = mako(52, begin = 0, end = 0.5)) + 
  scale_y_continuous(labels = scales::label_comma(scale_cut = c(0, K = 10^3)), 
                     limits = c(min_y, max_y)) # set y-axis limits

# Per-case Opportunity Cost
timeOC = rc_oc %>% ggplot(aes(x = year_id, y = oc, ymin = oc_lower, 
                              ymax = oc_upper, color = location_name)) + 
  geom_line(data = rc_oc[location_id == 102]) + 
  geom_point(data = rc_oc[location_id == 102]) + 
  geom_ribbon(data = rc_oc[location_id == 102], alpha = 0.15) +
  geom_line(data = rc_oc[location_id != 102], alpha = 0.3) +
  labs(x = "Year", y = "Cost (2019 $USD)", 
       title = "Annual Per-Case Forgone Wage Cost") + 
  scale_x_continuous(limits = c(2010, 2019), breaks = seq(2010, 2019, 1)) +
  theme(panel.background = element_rect(fill="white"),
        #aspect.ratio = ((1 + sqrt(5))/2)^(-1), 
        ## All axes changes
        axis.ticks.length = unit(0.5, "char"), #longer ticks
        text = element_text(size = 14), # set figure font size 
        ## Horizontal axis changes
        axis.line.x = element_line(size = 0.2), # thinner axis lines
        axis.ticks.x = element_line(size = 0.2), # thinner ticks
        axis.text.x = element_text(color = "black", size=12),
        axis.title.x = element_text(size = 12,
                                    margin = margin(t = 7.5, r = 0, b = 0, l = 0)),
        ## Vertical axis changes
        axis.line.y = element_line(size = 0.2), # thinner axis lines
        axis.ticks.y = element_blank(), # no y axis ticks (gridlines suffice)
        axis.text.y = element_text(color = "black", size=12),
        axis.title.y = element_text(size = 12,
                                    margin = margin(t = 0, r = 7.5, b = 0, l = 0)),
        ## Legend
        legend.position = 'none',
        legend.key = element_rect(fill = NA, color = NA),
        ## Gridlines
        panel.grid.major.x = element_line(color = "gray45", size = 0.1),
        panel.grid.major.y = element_line(color = "gray45", size = 0.1)
  ) + scale_color_manual(values = viridis(52, begin = 0.25, end = 0.75)) + 
  scale_y_continuous(labels = scales::label_comma(scale_cut = c(0, K = 10^3)), limits = c(min_y, max_y)) # set y-axis limits

# combine per-case plots and save to pdf/eps: 
# save to pdf: 
pdf_name = paste0(FILEPATH,"Figure2_cost_percase_timescale.pdf")

pdf(width=10, height=6, file = pdf_name)
cowplot::plot_grid(timeRC, timeOC, nrow = 1, ncol = 2)
dev.off()

# save to EPS: 
eps_name = paste0(FILEPATH,"Figure2_cost_percase_timescale.eps")

setEPS()
postscript(eps_name, width=10, height=6)
cowplot::plot_grid(timeRC, timeOC, nrow = 1, ncol = 2)
dev.off()

## Timescale Plots (total costs): 
#----------------------DIDN'T USE THIS FOR FINAL MANUSCRIPT---------------------
# demenita prevalence and total cases 
dem_cases = fread(file = paste0(FILEPATH,"rate_and_total_cases_1990_2019.csv"))

# merge in dementia cases data
setnames(dem_cases, old = "year_id", new = "year")
rc_oc_cases = merge(rc_oc, dem_cases[,.(year, location_id, cases)], 
                    by = c("year", "location_id"))
# calculate total annual costs
rc_oc_cases[,":=" (
  total_rc = rc_mean*cases, 
  total_rc_lower = rc_lower*cases, 
  total_rc_upper = rc_upper*cases,
  total_oc = oc_mean*cases,
  total_oc_lower = oc_lower*cases, 
  total_oc_upper = oc_upper*cases
)]

# get values for setting y axis limits
total_min_y = round(min(rc_oc_cases[state == "USA"]$total_oc_lower), -8)
total_max_y = round(max(rc_oc_cases[state == "USA"]$total_rc_upper), -11)

# Total Replacement Cost
total_timeRC = rc_oc_cases %>% ggplot(aes(x = year, y = total_rc, ymin = total_rc_lower, 
                              ymax = total_rc_upper, color = state)) + 
  geom_line(data = rc_oc_cases[state == "USA"]) + 
  geom_point(data = rc_oc_cases[state == "USA"]) + 
  geom_ribbon(data = rc_oc_cases[state == "USA"], alpha = 0.15) +
  labs(x = "Year", y = "Replacement Cost (2019 $USD)", 
       title = "Total Annual Replacement Cost of Informal Care") + 
  scale_x_continuous(limits = c(2010, 2019), breaks = seq(2010, 2019, 1)) +
  theme(panel.background = element_rect(fill="white"),
        #aspect.ratio = ((1 + sqrt(5))/2)^(-1), 
        ## All axes changes
        axis.ticks.length = unit(0.5, "char"), #longer ticks
        text = element_text(size = 14), # set figure font size 
        ## Horizontal axis changes
        axis.line.x = element_line(size = 0.2), # thinner axis lines
        axis.ticks.x = element_line(size = 0.2), # thinner ticks
        axis.text.x = element_text(color = "black", size=12),
        axis.title.x = element_text(size = 12,
                                    margin = margin(t = 7.5, r = 0, b = 0, l = 0)),
        ## Vertical axis changes
        axis.line.y = element_line(size = 0.2), # thinner axis lines
        axis.ticks.y = element_blank(), # no y axis ticks (gridlines suffice)
        axis.text.y = element_text(color = "black", size=12),
        axis.title.y = element_text(size = 12,
                                    margin = margin(t = 0, r = 7.5, b = 0, l = 0)),
        ## Legend
        legend.position = 'none',
        legend.key = element_rect(fill = NA, color = NA),
        ## Gridlines
        panel.grid.major.x = element_line(color = "gray45", size = 0.1),
        panel.grid.major.y = element_line(color = "gray45", size = 0.1)
  ) + scale_color_manual(values = mako(52, begin = 0.4, end = 0.5)) + 
  scale_y_continuous(labels = scales::label_comma(scale_cut = c(0, B = 10^9)),
                     limits = c(total_min_y, total_max_y)) # set y-axis limits

# Total Opportunity Cost
total_timeOC = rc_oc_cases %>% ggplot(aes(x = year, y = total_oc, ymin = total_oc_lower, 
                              ymax = total_oc_upper, color = state)) + 
  geom_line(data = rc_oc_cases[state == "USA"]) + 
  geom_point(data = rc_oc_cases[state == "USA"]) + 
  geom_ribbon(data = rc_oc_cases[state == "USA"], alpha = 0.15) +
  #geom_line(data = rc_oc[state != "USA"], alpha = 0.1) +
  labs(x = "Year", y = "Opportunity Cost (2019 $USD)", 
       title = "Total Annual Opportunity Cost of Informal Care") + 
  scale_x_continuous(limits = c(2010, 2019), breaks = seq(2010, 2019, 1)) +
  theme(panel.background = element_rect(fill="white"),
        #aspect.ratio = ((1 + sqrt(5))/2)^(-1), 
        ## All axes changes
        axis.ticks.length = unit(0.5, "char"), #longer ticks
        text = element_text(size = 14), # set figure font size 
        ## Horizontal axis changes
        axis.line.x = element_line(size = 0.2), # thinner axis lines
        axis.ticks.x = element_line(size = 0.2), # thinner ticks
        axis.text.x = element_text(color = "black", size=12),
        axis.title.x = element_text(size = 12,
                                    margin = margin(t = 7.5, r = 0, b = 0, l = 0)),
        ## Vertical axis changes
        axis.line.y = element_line(size = 0.2), # thinner axis lines
        axis.ticks.y = element_blank(), # no y axis ticks (gridlines suffice)
        axis.text.y = element_text(color = "black", size=12),
        axis.title.y = element_text(size = 12,
                                    margin = margin(t = 0, r = 7.5, b = 0, l = 0)),
        ## Legend
        legend.position = 'none',
        legend.key = element_rect(fill = NA, color = NA),
        ## Gridlines
        panel.grid.major.x = element_line(color = "gray45", size = 0.1),
        panel.grid.major.y = element_line(color = "gray45", size = 0.1)
  ) + scale_color_manual(values = viridis(52, begin = 0.6, end = 0.75)) + 
  scale_y_continuous(labels = scales::label_comma(scale_cut = c(0, B = 10^9)),
                     limits = c(total_min_y, total_max_y)) # set y-axis limits


# combine both total cost plots:
cowplot::plot_grid(total_timeRC, total_timeOC, nrow = 1, ncol = 2)

# plot both on same axis

rc_long = rc[, type := "Replacement Cost"]
setnames(rc_long, c("rc_mean", "rc_lower", "rc_upper"), c("mean", "lower", "upper"))

oc_long = oc[, type := "Opportunity Cost"]
setnames(oc_long, c("oc_mean", "oc_lower", "oc_upper"), c("mean", "lower", "upper"))

rc_oc_long = rbind(rc_long, oc_long)

rc_oc_long %>% ggplot(aes(x = year, y = mean, ymin = lower, 
                          ymax = upper, color = type)) + 
  geom_line(data = rc_oc_long[state == "USA"]) + 
  geom_point(data = rc_oc_long[state == "USA"]) + 
  geom_ribbon(data = rc_oc_long[state == "USA"], alpha = 0.15) +
  labs(x = "Year", y = "Annual Cost (per case)", 
       title = "Mean Annual Cost of Informal Care per Case (2010-2019)") + 
  scale_x_continuous(limits = c(2010, 2019), breaks = seq(2010, 2019, 1)) +
  theme(panel.background = element_rect(fill="white"),
        #aspect.ratio = ((1 + sqrt(5))/2)^(-1), 
        ## All axes changes
        axis.ticks.length = unit(0.5, "char"), #longer ticks
        ## Horizontal axis changes
        axis.line.x = element_line(size = 0.2), # thinner axis lines
        axis.ticks.x = element_line(size = 0.2), # thinner ticks
        axis.text.x = element_text(color = "black", size=12),
        axis.title.x = element_text(size = 12,
                                    margin = margin(t = 7.5, r = 0, b = 0, l = 0)),
        ## Vertical axis changes
        axis.line.y = element_line(size = 0.2), # thinner axis lines
        axis.ticks.y = element_blank(), # no y axis ticks (gridlines suffice)
        axis.text.y = element_text(color = "black", size=12),
        axis.title.y = element_text(size = 12,
                                    margin = margin(t = 0, r = 7.5, b = 0, l = 0)),
        ## Legend
        # legend.position = 'none',
        # legend.key = element_rect(fill = NA, color = NA),
        ## Gridlines
        panel.grid.major.x = element_line(color = "gray45", size = 0.1),
        panel.grid.major.y = element_line(color = "gray45", size = 0.1)
  ) + scale_color_manual(values = c("darkgreen", "blue"))


