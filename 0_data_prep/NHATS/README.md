# NHATS Data Processing: Model Inputs

## Notes
This subfolder contains scripts that process and analyze NHATS caregiving data to create inputs for the replacement and opportunity cost models. This includes national cost and care hours estimates.

## Index: 

### SPID_with_dementia.R 
- script to create list of SPIDs that are classified as having dementia for each year of NHATS
- creates `FILEPATH/SPID_with_dementia.csv`

### OP_demographics.R 
- script to extract age, sex, and education variables from NHATS OP data files
- creates `FILEPATH/OP_age_sex_education.csv`

### NHATS_OP_caregiving_hours.R
- script to create .csv files of NHATS caregiving hours by SP and OP 
- two separate files:
  - one for total caregiving hours per care recipient (SP):
  `FILEPATH/SP_caregiving_hours.csv`
  - the other for caregiving hours per care giver (OP):
  `FILEPATH/OP_caregiving_hours.csv`

### hourly_cost_of_care_OP_CPS.R
- Script to calculate the hourly cost of care using NHATS OP care hours and CPS wage data
- this is for the opportunity cost model (CPS wages -> average hourly pay by age, sex, and education, how much they would be making if they were working instead of caregiving)
- the caregiving hours in this script are now top-coded to 40 hours/week.
- creates `FILEPATH/NHATS_OP_caregiving_opportunity_costs_topcode_40.csv`

### estimate_national_caregiving_hours_OP_NHATS.R
- sript to estimate the national caregiving hours from NHATS
- now uses caregiving hours from the OP data file 
- and sample weights from the SP data file (all from NHATS)
- creates: 
  - `FILEPATH/NHATS_OP_hours_2019_top_code_112.csv` for Replacement Cost
  - `FILEPATH/NHATS_OP_hours_2019_top_code_40.csv` for Forgone Wages


**Author: Michael Breshock**\
*Last Update: 03/27/2024*


