# Final Estimates

## Notes
The files in this subfolder are used calculate the final estimates for the US dementia informal care project. 

## Script Dictionary: 

### US_annual_replacement_cost_of_dementia_informal_care_per_case.R
This script calculates the annual replacement cost of informal dementia care for the US and each state. This is calculated by taking the average number of care hours per dementia case per week in each state and the US from ST-GPR and multiplying by the average cost of home health aides by state/year. The care hours are also scaled down using the attributable fraction of dementia to ensure all the care hours are only due to dementia and not other comorbidities. The ST-GPR output provides per-week care hours estimates, so these are multiplied by 52 weeks/year to convert the estimates from weekly to annual. This script also outputs a US map of the 2019 mean annual replacement cost per dementia case for each state. 

### US_annual_opportunity_cost_of_dementia_informal_care_per_case.R
This script calculates the annual forgone wage cost of informal dementia care for the US and each state. This model was previously called 'opportunity cost', hence the name of the script file. This script does essentially the same thing as the replacement cost script (see above), except instead of using home health aide wages, the caregiving hours top-coded to 40 hours per week are multiplied with the average wages by state/year output by the ST-GPR caregiver wages model. The rest of the estimate calculations mirror the replacement cost process, using the attributable fraction of dementia to scale down the hours and multiplying by 52 weeks/year to convert from weekly to annual estimates. This script also outputs a US map of the 2019 mean annual forgone wage cost per dementia case for each state. The end of this script also combines the replacement cost and forgone wage cost maps into a single figure using `cowplot`. 

### US_timescale_and_arrow_diagrams.R
This script creates scatter plots showing the US national mean replacement and forgone wage cost of informal dementia care over time. 

### RC_OC_summary_tables.R 
This script creates data tables summarizing the mean annual replacement and forgone wage costs of the US to be used in publication. The ADL/IADL/Supervision fractions are also applied here to provide low/medium/high estimates for both models. 

### dasgupta_table_frame.R
This script creates the input data for Das Gupta Decomposition analysis of the replacement and forgone wage cost final estimates in 2019. It combines the total mean/lower/upper estimates for replacement and forgone wage cost models with dementia prevalence, total population, mean care hours per case, and mean replacement/forgone wage costs per hour, all by state. This data can be used to conduct a decomposition analysis to assess exactly how much each component of the model are contributing to the variance of the final estimates. 

### dasgupta_calculation.R
This script should be run AFTER dasgupta_table_frame.R. It has two flags - one to specify whether the calculations are being done for replacement cost or forgone wage
cost, and one to specify whether the cost should be per capita or per person over 65. Based on these flags it grabs the appropriate data from the dasgupta_table_frame
and decomposes the cost into 4 components - state age profile, dementia prevalence, hourly wage, and caregiving hours. 

### dasgupta_decomp_chart.R
This script should be run AFTER dasgupta_calculation.R. This script is used to visualize the results of the Das Gupta decomposition analysis of the replacement and forgone wage cost model final estimates. 

### cost_forecast.R
This script forecasts our cost estimates through 2050 using both low growth and high growth scenarios. 

### forgone_wage_topcode112_sensitivity.R
This script calculates the final estimates of forgone wage costs under a sensitivity analysis. This sensitivity analysis uses a top-code of 112 caregiving hours/week for the forgone wage model, instead of the 40 hours/week top-code used in the base-case estimates. 

### sensitivity_forecast.R
This script forecasts our forgone wage cost sensitivity analysis estimates through 2050 under both low growth an high growth scenarios. This script essentially does the same thing as the cost_forecast.R script, however the caregiving hours used for the forgone wage estimates are top-coded at 112 hours/week here, instead of 40 hours/week. 

**Author: Michael Breshock**\
*Last Update: 03/27/2024*
