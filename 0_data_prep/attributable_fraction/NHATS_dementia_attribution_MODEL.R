# NHATS dementia attribution model + attributable fractions
# 11/02/2022
# Author: Michael Breshock

setwd("FILEPATH")
rm(list = ls())

# load in libraries
library(ggplot2)
library(dplyr)
library(stringr)
library(data.table)
library(tidyr)
library(Amelia)
library(RColorBrewer)
library(MASS)
library(BAS, lib.loc = "FILEPATH")


# load in caregiving hours data
SP_hours = fread(paste0(FILEPATH,"SP_caregiving_hours.csv"))
# remove dementia and age variable from this  (will be merged in later with imputed data)
SP_hours[,c("dementia", "age"):=NULL]

# load in imputed data created with Amelia package 
load(file = paste0(FILEPATH,"cleanNHATS_Jan18.rda"))

# calculating the mean of all imputations to represent the input data
tot_imp = data.table(cleanNHATS$imputations[[1]]) # initializing with the first
for(n in c(2:10)){ # for the rest of imputations used
  imp = cleanNHATS$imputations[[n]]
  tot_imp = tot_imp + imp # summing total imputations
}
mean_imp = round(tot_imp/10) # dividing total by number of imputations to get mean (round to keep integers)

################### Bayesian Model Selection ###################

# merge in care hours with demographic data
model_df = merge(mean_imp, SP_hours, by = c("year", "spid"))

model_df[, log_care_hours_week := log(care_hours_week)]

# create list of dependent variable and covariates being assessed 
dependent_var = "log_care_hours_week"
control_list = c("diabetes", "heart_disease", "arthritis", "cancer", "lung_disease",
                 "hbp", "stroke", "depression", "anxiety")

bms_list <- c(dependent_var, control_list)
bms_data <- model_df[dementia == 1 & care_hours_week > 0, ..bms_list]

# create RHS formula
controls <- paste(control_list, collapse = " + ")

# create model formula
model_string <- as.formula(paste(dependent_var, '~', controls))

# run BMS -> do this for each imputation
# also try doing BMS on raw data -> not imputed
model_bms <- bas.lm(model_string,
                    data = bms_data,
                    prior = "JZS", # try JZS instead
                    modelprior = uniform())

# save BMS plots
mip_plot <- plot(model_bms, which = 4)
lpo_plot <- image(model_bms)

# save coefficients
temp_coef <- coef(model_bms)

bms_coefs <- data.table(factor = temp_coef$namesx, beta = temp_coef$postmean, prob = temp_coef$probne0, se = 0)

bms_coefs[, sig := ifelse(beta<0 & prob > 0.7, '-1',
                          ifelse(beta>0 & prob > 0.7, '1', '0'))]

bms_coefs <- bms_coefs[! factor=="Intercept",]

fwrite(bms_coefs,paste0(FILEPATH,"AF_BMS_results.csv"))

# save plots

# define colors
red <- brewer.pal(3, 'Set1')[1]
green <- brewer.pal(3, 'Set1')[3]
black <- '#000000'

# define plotting function
plot_effects_and_se <- function(title_name,coefs){
  p <- ggplot(coefs, aes(x=factor, y=beta, color=sig)) +
    geom_hline(yintercept = 0) +
    geom_point() +
    geom_errorbar(aes(ymin=beta-1.96*se, ymax=beta+1.96*se), width=0) + 
    scale_color_manual("", breaks=c("-1","0","1"), values=c(red, black, green)) +
    coord_flip() +
    ggtitle(title_name) +
    theme_bw() +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor.x = element_blank(),
          axis.ticks = element_blank(),
          panel.border = element_blank(),
          legend.position = "none")
  return(p)
}

plot_title = paste0("Care Hours Per Week: bayesian model coefficients")
treemap_bms <- plot_effects_and_se(plot_title,bms_coefs)

pdf(paste0(FILEPATH,"AF_BMS_plots.pdf"))
plot(model_bms, which = 4)
image(model_bms)
treemap_bms
dev.off()

# get significant covariates from BMS results
sig_controls_list = bms_coefs[sig == 1]$factor
sig_controls <- paste(sig_controls_list, collapse = " + ")

# set up model formula for AF calculation
sig_model_string <- as.formula(paste("log(care_hours_week)", '~', sig_controls))

# running linear model on each imputation output by amelia
# then combining the results from each model using mi.meld -> don't need to use Rubin's rule for the 90 imputations
beta_draws <- NULL
for(i in c(1:10)) { # looping through each imputation
  
  imp = merge(cleanNHATS$imputations[[i]], SP_hours, by = c("year", "spid"))
  
  dem_df = imp[dementia == 1 & care_hours_week > 0] # running model on only SPs with dementia
  # running linear model
  ols.out <- lm(sig_model_string, data = dem_df) # removed these co-variates since their confidence interval crossed zero
  # pull 10 draws from the model
  draws = data.table(mvrnorm(n = 100, ols.out$coefficients, vcov(ols.out))) # variance covariance matrix
  
  # storing beta coefficients
  beta_draws <- rbind(beta_draws, draws)
}
# add draw variable to beta dataframe
beta_draws[, draw := paste0("draw_",c(0:999))]
# delete intercept column 
beta_draws[, `(Intercept)` := NULL]

# remove TRUE suffix from column names and pivot to long format
beta_long = beta_draws %>% 
  rename_with(.cols = -draw, ~str_remove(., 'TRUE')) %>%
  pivot_longer(cols = -draw, names_to = "condition", values_to = "beta") 
setDT(beta_long)

# calculate dementia attributable fraction draws for each comorbidity modeled: 
AF_comorb = data.table() # initializing variable to store attributable fraction draws
for (d in unique(beta_long$draw)){ # for 1000 draws
  beta_d = beta_long[draw == d] # filter to draw being tested
  for (j in unique(beta_long$condition)){ # for each condition
    beta_dj = beta_d[condition == j] # filter to condition being tested
    # calculate proportion of people with dementia that also have condition j:
    p = nrow(mean_imp[dementia == 1 & eval(parse(text = j)) == 1]) / 
      nrow(mean_imp[dementia == 1])
    AF_j = p*(exp(beta_dj$beta) - 1) # estimate attr frac for condition j on draw d
    AF_j_df = data.table(draw = d, condition = j, AF = AF_j) 
    AF_comorb = rbind(AF_comorb, AF_j_df) # save draw
  }
}

# pivot wider to have each condition and draw on one row: 
AF = AF_comorb %>% 
  pivot_wider(id_cols = draw, names_from = condition, values_from = AF)
setDT(AF)
# calculate attributable fraction of dementia for each draw: 
AF[, dementia := 1 - (hbp + stroke + depression)]

# see mean, lower, and upper bound of dementia AF:
AF[,.(mean = mean(dementia), 
      lower = quantile(dementia, 0.025), 
      upper = quantile(dementia, 0.975))]

out_path = "FILEPATH"
fwrite(AF, file = paste0(out_path, "attributable_fraction_draws.csv"))

