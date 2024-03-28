# Number plugging - appendix table S14
# Forgone Wage Sensitivity Analysis - Forecasts
# Author: Michael Breshock

rm(list = ls())

# load in libraries
library(dplyr)
library(data.table)


source("FILEPATH/helper_functions.R")

# load forecasts
low_growth = fread(paste0(FILEPATH,"sensitivity_low_growth_cost_draws.csv"))
high_growth = fread(paste0(FILEPATH,"sensitivity_high_growth_cost_draws.csv"))

# summarize low and high growth draws: 
low_sum = low_growth[,.(total_oc_mean = mean(total_oc), 
                        total_oc_lower = quantile(total_oc, 0.025),
                        total_oc_upper = quantile(total_oc, 0.975),
                        pc_oc_mean = mean(per_cap_oc),
                        pc_oc_lower = quantile(per_cap_oc, 0.025),
                        pc_oc_upper = quantile(per_cap_oc, 0.975)), 
                     by = "year_id"]

high_sum = high_growth[,.(total_oc_mean = mean(total_oc), 
                          total_oc_lower = quantile(total_oc, 0.025),
                          total_oc_upper = quantile(total_oc, 0.975),
                          pc_oc_mean = mean(per_cap_oc),
                          pc_oc_lower = quantile(per_cap_oc, 0.025),
                          pc_oc_upper = quantile(per_cap_oc, 0.975)), 
                       by = "year_id"]

### create tables: 
# view total cost estimates in billions of $:
cat("Total Forgone Wage Cost (billions of $): \n", 
    "[Low-growth scenario] \n",
    "2019 | ", round_num(low_sum[year_id==2019]$total_oc_mean, 
                         type = "bill", lancet = F), " (", 
    round_num(low_sum[year_id==2019]$total_oc_lower, 
              type = "bill", lancet = F), "-", 
    round_num(low_sum[year_id==2019]$total_oc_upper, 
              type = "bill", lancet = F), ")\n", 
    "2030 | ", round_num(low_sum[year_id==2030]$total_oc_mean, 
                         type = "bill", lancet = F), " (", 
    round_num(low_sum[year_id==2030]$total_oc_lower, 
              type = "bill", lancet = F), "-", 
    round_num(low_sum[year_id==2030]$total_oc_upper, 
              type = "bill", lancet = F), ")\n",
    "2040 | ", round_num(low_sum[year_id==2040]$total_oc_mean, 
                         type = "bill", lancet = F), " (", 
    round_num(low_sum[year_id==2040]$total_oc_lower, 
              type = "bill", lancet = F), "-", 
    round_num(low_sum[year_id==2040]$total_oc_upper, 
              type = "bill", lancet = F), ")\n",
    "2050 | ", round_num(low_sum[year_id==2050]$total_oc_mean, 
                         type = "bill", lancet = F), " (", 
    round_num(low_sum[year_id==2050]$total_oc_lower, 
              type = "bill", lancet = F), "-", 
    round_num(low_sum[year_id==2050]$total_oc_upper, 
              type = "bill", lancet = F), ")\n",
    "[High-growth scenario] \n", 
    "2019 | ", round_num(high_sum[year_id==2019]$total_oc_mean, 
                         type = "bill", lancet = F), " (", 
    round_num(high_sum[year_id==2019]$total_oc_lower, 
              type = "bill", lancet = F), "-", 
    round_num(high_sum[year_id==2019]$total_oc_upper, 
              type = "bill", lancet = F), ")\n", 
    "2030 | ", round_num(high_sum[year_id==2030]$total_oc_mean, 
                         type = "bill", lancet = F), " (", 
    round_num(high_sum[year_id==2030]$total_oc_lower, 
              type = "bill", lancet = F), "-", 
    round_num(high_sum[year_id==2030]$total_oc_upper, 
              type = "bill", lancet = F), ")\n",
    "2040 | ", round_num(high_sum[year_id==2040]$total_oc_mean, 
                         type = "bill", lancet = F), " (", 
    round_num(high_sum[year_id==2040]$total_oc_lower, 
              type = "bill", lancet = F), "-", 
    round_num(high_sum[year_id==2040]$total_oc_upper, 
              type = "bill", lancet = F), ")\n",
    "2050 | ", round_num(high_sum[year_id==2050]$total_oc_mean, 
                         type = "bill", lancet = F), " (", 
    round_num(high_sum[year_id==2050]$total_oc_lower, 
              type = "bill", lancet = F), "-", 
    round_num(high_sum[year_id==2050]$total_oc_upper, 
              type = "bill", lancet = F), ")\n",
    sep = "")

## view per-capita cost estimates:
cat("Per-capita Forgone Wage Cost ($): \n", 
    "[Low-growth scenario] \n",
    "2019 | ", round_num(low_sum[year_id==2019]$pc_oc_mean, 
                         type = "dollar", lancet = F), " (", 
    round_num(low_sum[year_id==2019]$pc_oc_lower, 
              type = "dollar", lancet = F), "-", 
    round_num(low_sum[year_id==2019]$pc_oc_upper, 
              type = "dollar", lancet = F), ")\n", 
    "2030 | ", round_num(low_sum[year_id==2030]$pc_oc_mean, 
                         type = "dollar", lancet = F), " (", 
    round_num(low_sum[year_id==2030]$pc_oc_lower, 
              type = "dollar", lancet = F), "-", 
    round_num(low_sum[year_id==2030]$pc_oc_upper, 
              type = "dollar", lancet = F), ")\n",
    "2040 | ", round_num(low_sum[year_id==2040]$pc_oc_mean, 
                         type = "dollar", lancet = F), " (", 
    round_num(low_sum[year_id==2040]$pc_oc_lower, 
              type = "dollar", lancet = F), "-", 
    round_num(low_sum[year_id==2040]$pc_oc_upper, 
              type = "dollar", lancet = F), ")\n",
    "2050 | ", round_num(low_sum[year_id==2050]$pc_oc_mean, 
                         type = "dollar", lancet = F), " (", 
    round_num(low_sum[year_id==2050]$pc_oc_lower, 
              type = "dollar", lancet = F), "-", 
    round_num(low_sum[year_id==2050]$pc_oc_upper, 
              type = "dollar", lancet = F), ")\n",
    "[High-growth scenario] \n", 
    "2019 | ", round_num(high_sum[year_id==2019]$pc_oc_mean, 
                         type = "dollar", lancet = F), " (", 
    round_num(high_sum[year_id==2019]$pc_oc_lower, 
              type = "dollar", lancet = F), "-", 
    round_num(high_sum[year_id==2019]$pc_oc_upper, 
              type = "dollar", lancet = F), ")\n", 
    "2030 | ", round_num(high_sum[year_id==2030]$pc_oc_mean, 
                         type = "dollar", lancet = F), " (", 
    round_num(high_sum[year_id==2030]$pc_oc_lower, 
              type = "dollar", lancet = F), "-", 
    round_num(high_sum[year_id==2030]$pc_oc_upper, 
              type = "dollar", lancet = F), ")\n",
    "2040 | ", round_num(high_sum[year_id==2040]$pc_oc_mean, 
                         type = "dollar", lancet = F), " (", 
    round_num(high_sum[year_id==2040]$pc_oc_lower, 
              type = "dollar", lancet = F), "-", 
    round_num(high_sum[year_id==2040]$pc_oc_upper, 
              type = "dollar", lancet = F), ")\n",
    "2050 | ", round_num(high_sum[year_id==2050]$pc_oc_mean, 
                         type = "dollar", lancet = F), " (", 
    round_num(high_sum[year_id==2050]$pc_oc_lower, 
              type = "dollar", lancet = F), "-", 
    round_num(high_sum[year_id==2050]$pc_oc_upper, 
              type = "dollar", lancet = F), ")\n",
    sep = "")


