# Number plugging - appendix table S9

rm(list = ls())

# load in libraries
library(dplyr)
library(data.table)



AF = fread(paste0(FILEPATH,"attributable_fraction_draws.csv"))

af_sum = AF[,.(hbp = mean(hbp), 
               stroke = mean(stroke),
               depression = mean(depression),
               dementia = mean(dementia))]
af_sum
