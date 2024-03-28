## Create Home Health Aide cost data file from BLS wages + Genworth cost markup ratio
## Date: 12/22/2022
## Author: Michael Breshock 

# clear environment
rm(list = ls())

# load libraries 
library(dplyr)
library(data.table)
library(tidyr)
library(stringr)



# load the markup data created in BLS_HHA_wages_by_state_year.R
# the dataframes avg_markup and fitted_markup_data come from here
load(paste0(FILEPATH,"HHA_markup.rda"))
# HHA wage draws
hha = fread(paste0(FILEPATH,"BLS_HHA_wages_2010_2019_draws.csv"))

# create linear model of HHA cost/wage markup by state and year
mod = lm(markup ~ YEAR + STATE, avg_markup)

### Create draws of markup ratio: 
## first get draws of beta coefficients and intercepts from linear model
vcov_mat = vcov(mod)
beta_means = coefficients(mod)
set.seed(123)
beta_draws = data.table(MASS::mvrnorm(1000, beta_means, vcov_mat))
setnames(beta_draws, old = "(Intercept)", new = "Intercept")

# Alabama was the hold out for the STATE variable 
# adding this into the beta data frame with coefficient = 0 
# so that it can be used to create predictions for Alabama later
beta_draws[, STATEAlabama := 0]

# add draw variable: 
beta_draws[, draw := paste0("draw_",c(0:999))]
# pivot draws longer so that we have one column for each STATE coefficient:
beta_long = beta_draws %>% 
  pivot_longer(cols = -c("draw", "Intercept", "YEAR"), 
               names_to = "STATE", values_to = "STATE_coef")
setDT(beta_long)

# recode STATE variable
beta_long[, STATE := str_remove(STATE, "STATE")]

# create 1000 markup ratio predictions for each year/state combo
# warning: this takes a while
markup_draws = data.table() # initialize empty DT to store draws in 
for (d in unique(beta_long$draw)){ # for each draw
  for (yr in unique(fitted_markup_data$YEAR)){ # for each year
    for (st in unique(fitted_markup_data$STATE)){ # for each state
      # extract the coefficients for the state/draw in question
      coefs = beta_long[draw == d & STATE == st] 
      # predict markup using coefs from regression model
      pred = yr*coefs$YEAR + coefs$STATE_coef + coefs$Intercept 
      # save markup prediction draw to DT
      markup = data.table(YEAR = yr, STATE = st, draw = d, markup = pred)
      markup_draws = rbind(markup_draws, markup) 
    }
  }
  print(d) # status check
}
head(markup_draws)
fwrite(markup_draws, file = paste0(FILEPATH,"HHA_wages_markup_draws.csv"))

hha_markup = merge(hha, markup_draws, by = c("YEAR", "STATE", "draw"))

# read in the GDP deflator data
gdp_deflator <- fread(paste0(FILEPATH,"gdp_deflator.csv"))
gdp_deflator <- gdp_deflator[, .(year, factor2019)]
names(gdp_deflator) <- c("YEAR", "factor2019")

# adjust caregiving costs to all be in 2019 dollars
hha2019 <- merge(hha_markup, gdp_deflator, by = "YEAR")

hha2019[, ":=" (H_MEAN2019 = H_MEAN*factor2019,
                H_MEAN_PPA2019 = H_MEAN_PPA*factor2019)]

# calculate HHA cost using markup ratio calculated with Genworth cost data
hha2019[, ":=" (H_COST2019 = H_MEAN2019*markup,
                H_COST_PPA2019 = H_MEAN_PPA2019*markup)]

# get national US estimates
US_hha = data.table(
  hha2019 %>% group_by(YEAR, draw) %>% 
    summarise(STATE = "USA",
              H_MEAN = weighted.mean(H_MEAN, TOT_EMP),
              H_MEAN_PPA = weighted.mean(H_MEAN_PPA, TOT_EMP),
              OCC_TITLE = "Home Health and Personal Care Aides",
              MEAN_PRSE = weighted.mean(MEAN_PRSE, TOT_EMP),
              std_err = weighted.mean(std_err, TOT_EMP),
              JOBS_1000 = weighted.mean(JOBS_1000, TOT_EMP),
              rpp = weighted.mean(rpp, TOT_EMP),
              location_id = "102",
              markup = weighted.mean(markup, TOT_EMP),
              factor2019 = weighted.mean(factor2019, TOT_EMP),
              H_MEAN2019 = weighted.mean(H_MEAN2019, TOT_EMP),
              H_MEAN_PPA2019 = weighted.mean(H_MEAN_PPA2019, TOT_EMP),
              H_COST2019 = weighted.mean(H_COST2019, TOT_EMP),
              H_COST_PPA2019 = weighted.mean(H_COST_PPA2019, TOT_EMP),
              TOT_EMP = sum(TOT_EMP))
)

# add national estimates to state data
national_hha2019 = rbind(hha2019, US_hha)

# save dataframe to file
fwrite(national_hha2019, paste0(FILEPATH,"HHA_wages_cost_PPA_2019_draws.csv"))
