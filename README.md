# US Dementia Caregiving

This repository contains the code for the paper "U.S. dementia care spending by state: 2010-2019" which can be accessed here: https://alz-journals.onlinelibrary.wiley.com/doi/full/10.1002/alz.13746

Paper Authors: Amy Lastuka, Michael R. Breshock, Theresa A. McHugh, William T. Sogge, Vivianne Swart, Joseph L. Dieleman.

The code in this repository was written by Michael Breshock, Amy Lastuka, and Will Sogge. All code is written in R. 

The purpose of this code is to create estimates of caregiving costs for people with dementia in the United States.  Hours of unpaid caregiving per person living with dementia are estimated using data from the [Health and Retirement Study (HRS)](https://hrs.isr.umich.edu/about), the [National Health and Aging Trends Study (NHATS)](https://www.nhats.org/), and the [Behavioral Risk Factor Surveillance System (BRFSS)](https://www.cdc.gov/brfss/index.html). The cost of each hour is estimated in two ways - replacement cost and forgone wage cost. The data for the replacement cost comes from the [Bureau of Labor Statistics (BLS)](https://www.bls.gov/oes/) and the data for forgone wages comes from the [Current Population Survey (CPS)](https://www.census.gov/data/datasets/time-series/demo/cps/cps-basic.html).  

The code is organized into subfolders that correspond to the different steps in the analysis. The subfolders are numbered in the order in which they should be run. There are README files in folders `2_stgpr`, `3_post_processing`, and `4_final_estimates` to provide additional detail on how to run the code in those sections. There are also READMEs in some of the `0_data_prep` child folders. The `functions` folder contains scripts that define custom-made functions used throughout the processing and modeling pipeline. Scripts in the `number_plugging` folder are used for creating numerical estimates that are reported in the publication. The `GHDx` folder contains a script that was used to format our final estimates and save them to a CSV file that will be uploaded to IHME's [Global Health Data Exchange (GHDx)](https://ghdx.healthdata.org/).

Contact: Amy Lastuka (amydash@uw.edu), Michael Breshock (mrb123@uw.edu)