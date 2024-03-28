# Dementia Attributable Fraction Model

## Notes
This subfolder contains scripts that process and analyze NHATS caregiving and demographic data to model/estimate the fraction of caregiving time attributable to dementia.

### attributable_fraction_preprocessing.R
- script used to preprocess NHATS demogrpaphic data to prepare as input for the attributable fraction of dementia model
- Amelia package used to run multiple imputation to fill missing health condition data
- creates `FILEPATH/cleanNHATS_Jan18.rda`

### BMS_attributable_fraction.R
- script to conduct Bayesian model selection on NHATS input data to select the most significant covariates to include in when modeling the attributable fraction of dementia.

### NHATS_dementia_attribution_MODEL.R
- script to calculate the fraction of caregiving hours attributable to dementia 
- covariates included based on Bayesian model selection results: 
  - Depression
  - High Blood Pressure (HBP)
  - Stroke
- creates `FILEPATH/attributable_fraction_dep_hbp_stroke.csv`

**Author: Michael Breshock**\
*Last Update: 03/23/2023*