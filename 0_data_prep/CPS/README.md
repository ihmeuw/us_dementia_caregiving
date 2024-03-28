# Current Population Survey Wage Data

## Background
The data comes from the Current Population Survey (CPS) [Basic Monthly CPS](https://www.census.gov/data/datasets/time-series/demo/cps/cps-basic.html) datasets. The data is downloaded from the CPS API using the [cpsR package](https://cran.r-project.org/web/packages/cpsR/index.html) using the commented out code chunks at the end of CPS_exploration.Rmd. The cpsR package is used to call for the CPS monthly data for each month in 2010 - 2021. The data from the API is saved into a single csv file in `J:Project/IRH/Informal_Care_AD/US_dementia_spending_2022/data/CPS/monthly_cps_update_08_22_22.csv`. This is done so that future work done on this data can simply load the csv file rather than having to call the CPS API every time, as this can take up to several hours to complete. The cpsR functions will only call for the variables you specify. If a new variable is needed in the data set, the CPS API will need to be called from again with the updated variable list. This data should be saved and the file name should include the date of extraction to prevent any confusion. 

*Note about calling from the CPS API: At least once I ran into errors calling from the API that seemed to be due to the CPS server issues rather than any issue with my code. If you run into errors, try to troubleshoot them, but if the error seems to be inconsistent, it may be due to the CPS servers and you may need to try again at a later date or time. *

## download_monthly_CPS_code.R
This script is used to download the CPS monthly wage data. This output is saved to: J:/Project/IRH/Informal_Care_AD/US_dementia_spending_2022/data/CPS/monthly_cps_update_08_22_22.csv. This file should be used to do CPS data analysis rather than running the download code every time, as the download code takes a very long time to run. Only run this script again if the data needs to be updated. 

## CPS_wages_age_sex_educ.R
This is the main file used to analyze the CPS data. In the `mean wage by age sex and education` section, the CPS age data is put into bins in a new `age` column to match with the Hurd paper. Education data is then grouped into fewer bins than the raw data, also to match with the reference papers. A major part of this code chunk involves creating the `hours_per_week` column. This is being done with the end goal of calculating hourly wage from the reported weekly salaries for those who did not report an hourly rate. The `hours_per_week` variable was created to aggregate the many different variables containing information related to number of hours worked per week. All of these need to be captured as in many cases, a respondent only provides an answer to one of the questions, if any at all. In order to most accurately estimate the hourly wage of those who only reported a weekly salary, the hours per week they work must be represented as accurately as possible as well. 
The general logic of the `hours_per_week` variable is: 

- If the exact number of hours per week are provided and the respondent receives overtime pay:
  Take the number of hours per week reported as is.
- If did not provide exact number of hours but did provide a range:
  Take the midpoint of the range (cap at 40 unless they indicate receiving overtime pay).
- If they only indicated working part time:
  Assign 20 hours per week.
- If they only indicated working full time:
  Assign 40 hours per week.
- Otherwise:
  Take the reported number of hours as is, capped at 40. 
  
Next, the `all_hourly` variable is created to represent both hourly pay and salaried workers (ie those who did not report an hourly wage but did report a weekly). 
The general logic of the `all_hourly` variable is: 

- If an hourly wage is not reported but both the weekly salary and hours per week are reported:
  Calculate the hourly wage by dividing the weekly salary by hours per week
- If an hourly wage and hours per week are not reported but the weekly salary is:
  Calculate the hourly wage by dividing the weekly salary by 40 (assuming full time in this case - no other info provided)
- Otherwise: 
  Take the reported hourly wage
  
The hourly wage data is grouped and summarized in the `avg_wage` object. 

The labor force participation rate (LFP) are calculated, grouped, and summarized in the `calculating labor force participation rate by state, age, gender, education and year` section and the `lfp` object. 

The LFP and wage data are merged in the `merging wage and lfp data and saving to single .csv` section and saved to: `J:/Project/IRH/Informal_Care_AD/US_dementia_spending_2022/output/CPS/CPS_wages_LFP_2010_21.csv`

The average hours worked by age, sex, and education from CPS are also estimated and saved to: `Project/IRH/Informal_Care_AD/US_dementia_spending_2022/output/CPS/CPS_work_hours_age_sex_educ_2010_21.csv`. These are needed for modeling lost work hours in the GERAS-US data set. 

## CPS_for_HRS.R

This script creates national CPS wage and LFP estimates by a few different groupings to be merged with HRS data to create opportunity cost estimates. These groups are: 
  - Year and Sex
  - Year, Sex, and Age
  - Year, Sex, Age, and Education 
These are created for years 2010, 2012, 2014, 2016, and 2018 to match with the HRS rounds. This is done to account for the sparsity of demographic data from HRS survey respondents. In some cases, we only have data for sex, in others we only have sex and age, and in some we have sex, age, and education. Creating estimates for all the different options allows us to merge wages in for every survey respondent even if we are missing age or education data for them. The average wages for all three different grouping options are merged into a single dataframe and saved to this file: `J:Project/IRH/Informal_Care_AD/US_dementia_spending_2022/output/CPS/wages_for_HRS_2010_2018.csv`. 

**Author: Michael Breshock**\
