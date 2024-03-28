# BLS Home Health Aide Wage Data README

## Background 
The data comes from the US Bureau of Labor Statistics (BLS) Occupational Employment and Wage Statistics (OEWS) program. 

The data was downloaded from [stats.bls.gov/oes/tables.htm](https://stats.bls.gov/oes/tables.htm) by clicking on the State (XLS) link for each year from 2010-2021. The data has been saved in FILEPATH/BUREAU_OF_LABOR_STATISTICS_OEWS. Each year is saved in a separate folder.

## Notes about the Data
Data from 2010-2013 comes in .xls format and data from 2014+ is in .xlsx format. 

The 2019 data has all the column names in lower case while the rest of the years have column names in upper case. 

Data from 2019-2021 uses the column name AREA_TITLE (area_title for 2019) instead of the name STATE as used in previous years.

These differences needed to be accounted for in order to combine all the years of data. 

Additionally, the OEWS program changed their categorization scheme in 2019. Data from 2010-2018 has two separate occupational categories for Home Health Aides and Personal Care Aides. In 2019-2021 data, these categories are combined into a single Home Health and Personal Care Aides category. In order to accurately compare data across time, the 2010-2018 data needed to be wrangled into containing a singular Home Health and Personal Care Aides category like the more recent years of data. 

The raw files include data from US Territories Guam, Virgin Islands and Puerto Rico. These regions were removed from this analysis as they are out of the scope of this project. The data on the 50 US States and District of Columbia (DC) were retained. 

In a handful of cases there is missing data. The [OEWS documentation](https://stats.bls.gov/oes/oes_ques.htm) explains that in some cases data can not be released because of quality or confidentiality reasons. Sometimes only one or a few variables of data are missing. More rarely, there are cases where all of the data for a particular state, occupation, and year is missing. When this is the case, this observation is not included in the data file at all. This is a bit more tricky to handle then simply filling missing values. For example, the 2014 data doesn't include a row for Personal Care Aides in Vermont.

In order to have complete data, the missing rows had to be added to the raw data as well as computing missing (NA) values. 

## BLS_HHA_wages_by_state_year.R
This script creates the price parity adjusted home health aide wages by state and year used for the replacement cost model. The script first loads all the data and then fills in empty values for the states missing from each year of data. Any missing values are then filled using approximate interpolation. After this, the separate categories of "Home Health Aide" and "Personal Care Aide" in 2010-2018 data files are combined into a single category to match the 2019-2021 data. Once this is done, all years of data can be combined together. Following these steps, there is a section that loads in data from the Genworth Cost of Care survey to get the cost of home health aides for the consumer. This data is used to calculate an average markup of cost / wage for home health aide by year. This is done so that we may convert all the wage data we have to cost to the consumer, rather than wage for the home health aide. We only have data for 2012, 2015, 2018, 2021 from Genworth, so the average markup is saved in `J:/Project/IRH/Informal_Care_AD/US_dementia_spending_2022/output/HHA_markup.rda` and interpolated to fill the missing years in a separate script named `national_cost_of_HHA_care.R`. Finally, the wage data is price parity adjusted using regional price parities from `J:/Project/IRH/dex_us_county/maps/states.RData` and saved out to `J:/Project/IRH/Informal_Care_AD/US_dementia_spending_2022/output/BLS_HHA_wages_2010_2019.csv`. 


## national_cost_of_HHA_care.R
Script to calculate cost of home health aides using raw wages from BLS and markup ratios calculated from Genworth cost data. Saves the data to this csv file `FILEPATH/HHA_wages_cost_PPA_2019.csv` to be used later to calculate replacement costs from caregiving hours estimated by ST-GPR.

**Author: Michael Breshock**\
