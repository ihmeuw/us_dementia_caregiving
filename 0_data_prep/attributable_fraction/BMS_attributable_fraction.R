# Bayesian Model Selection for attributable fraction model
# 02/03/2023
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

# to do: run BMS on raw NHATS data and each amelia imputation 

# -----------------------------------------------------------------------------
### BMS on raw NHATS data ###

# load raw demographic data (wrangled in attributable fraction preprocessing script)

NHATS_raw = fread(file = paste0(FILEPATH,"NHATS_AF_input_raw.csv"))

# load in caregiving hours data
SP_hours = fread(paste0(FILEPATH,"SP_caregiving_hours.csv"))

# recode values raw columns: 
NHATS_raw[, ':=' (
  heart_disease = disescn2 == 1, # has heart disease
  hbp = disescn3 == 1, # has high blood pressure
  arthritis = disescn4 == 1, # has arthritis
  diabetes = disescn6 == 1, # has diabetes
  lung_disease = disescn7 == 1, # has lung disease
  stroke = disescn8 == 1, # had stroke
  cancer = disescn10 == 1 # has cancer
)]

# using cut off points from Kroenke et al 2009: An Ultra-Brief Screening Scale for Anxiety and Depression: The PHQ–4
# recoding PHQ-4 questions to appropriate scores 
# 1 not at all = 0
# 2 several days = 1
# 3 more than half the days = 2
# 4 nearly every day = 3
NHATS_raw[, ':=' (dep1 = depresan1 - 1, # little interest or pleasure
                  dep2 = depresan2 - 1, # down depressed hopeless
                  anx1 = depresan3 - 1, # nervous anxious
                  anx2 = depresan4 - 1  # unable to stop worry
)]

# calculating PHQ-2 and GAD-2 scores
NHATS_raw[, ':=' (PHQ = dep1 + dep2, # PHQ-2 score = sum of depression scores
                  GAD = anx1 + anx2)] # GAD-2 score = sum of anxiety scores

# classifying depression and anxiety
NHATS_raw[, ':=' (depression = PHQ >= 3, # PHQ cut-point = 3,
                  anxiety = GAD >= 3)] # GAD cut-point = 3, both from Kroenke 2009

################### Bayesian Model Selection ###################

# merge in care hours with demographic data
model_df = merge(NHATS_raw, SP_hours, by = c("year", "spid"))

# make new column of log of care hours
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

# run BMS on raw data
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

bms_coefs[, sig := ifelse(beta<0 & prob > 0.8, '-1',
                          ifelse(beta>0 & prob > 0.8, '1', '0'))]

bms_coefs <- bms_coefs[! factor=="Intercept",]

fwrite(bms_coefs,paste0(FILEPATH,"AF_BMS_results_raw.csv"))

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

pdf(paste0(FILEPATH,"AF_BMS_plots_raw.pdf"))
plot(model_bms, which = 4)
image(model_bms)
treemap_bms
dev.off()

# -----------------------------------------------------------------------------
### BMS on each amelia imputation ###

# load in imputed data created with Amelia package 
load(file = paste0(FILEPATH,"cleanNHATS_Jan18.rda"))

# for some reason plots made using the plot_effects_and_se() function above
# do not save to a pdf during for loop (super weird!!!)
# going to save the model plots to pdf, and then look through the tree maps manually on Rstudio
treemaps = list()
pdf(paste0(FILEPATH,"AF_BMS_plots_imputations.pdf"))
for(i in c(1:10)) { # looping through each imputation
  # grab one imputation from amelia output
  imp = merge(cleanNHATS$imputations[[i]], SP_hours, by = c("year", "spid"))
  # running model on only SPs with dementia
  model_df = imp[dementia == 1 & care_hours_week > 0] 
  
  # make new column of log of care hours
  model_df[, log_care_hours_week := log(care_hours_week)]
  
  # create list of dependent variable and covariates being assessed 
  dependent_var = "log_care_hours_week"
  control_list = c("diabetes", "heart_disease", "arthritis", "cancer", "lung_disease",
                   "hbp", "stroke", "depression", "anxiety")
  
  bms_list <- c(dependent_var, control_list)
  bms_data <- model_df[, ..bms_list]
  
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
  
  # save coefficients
  temp_coef <- coef(model_bms)
  
  bms_coefs <- data.table(factor = temp_coef$namesx, beta = temp_coef$postmean, prob = temp_coef$probne0, se = 0)
  
  bms_coefs[, sig := ifelse(beta<0 & prob > 0.8, '-1',
                            ifelse(beta>0 & prob > 0.8, '1', '0'))]
  
  bms_coefs <- bms_coefs[! factor=="Intercept",]
  
  # print plots to pdf
  plot(model_bms, which = 4)
  image(model_bms)
  
  plot_title = paste0("Care Hours Per Week: bayesian model coefficients | Imputation ", i)
  treemap_bms = plot_effects_and_se(plot_title,bms_coefs) # this function and the color palettes are defined above
  treemaps[[i]] <- treemap_bms
 
}
dev.off()
