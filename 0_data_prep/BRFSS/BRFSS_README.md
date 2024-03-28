# BRFSS code
Date: 2023-10-12

To prep all of the BRFSS data, the following steps are needed. (Need to re-run all of this if you want to change how hours are top-coded).

**Step 1**
Run the script BRFSS_2012_2013_prep.R
Make sure the variable **top_code** is set to the desired value

This script does three things:

1) write out the file *early_BRFSS_caregiving_distribution_[top_code].csv*

This file is simply a list of all of the hours from the BRFSS 2012-2013 caregiving surveys, but it is saved out AFTER top-coding (e.g. if it is top-coded 
to 112, any responses above 112 have been rewritten to be 112 hours). This is needed because from 2015 onward all of the caregiving hours responses are in buckets. We use this list as a distribution to sample from.

2) write out the file *BRFSS_caregiving_hours_2012_2013_[top_code].csv*

This file has hours so it's used for the replacement cost model

3) write out the file *BRFSS_caregiving_costs_2012_2013_[top_code].csv*

This file has wages which are based on the demographic mix of caregivers for each state/year. It is used for the forgone wage model


**Step 2**
Run the script BRFSS_2012_2013_draw_space.R
This is only needed for the replacement cost model

Make sure the variable **top_code** is set to the desired value

The hours estimates created in step 1 are total hours per state. To convert to per-case hours, we divide by prevalence estimates which are in draw space. 

**Step 3**
Run the script BRFSS_2015_2019_prep.R

Make sure the variable **top_code_string** is set to the desired value
Make sure the variables **cost_flag** is set to the desired value

If **cost_flag** is 0, outputs are in hours. These hour estimates are used for both the replacement cost model and the forgone wage model. (If replacement cost and forgone wage models use different top-coding values, which is currently the case for our US work, you need to have two sets of hours estimates. The estimates themselves will be different with different top-coding and also outliers may be different. )

* Script outputs the file *cg_hours_dementia_patients_by_state_year_envelope_[top_code].csv*

If **cost_flag** is 1, outputs are in dollars per hour. This is for the forgone wage model.

* Script outputs the file *dementia_caregiving_total_cost_[top_code].csv*

The output files from this step also include the state estimates from the 2012-2013 data, so these feed directly into the stage 1 models. 

**Step 4**
This is technically not part of BRFSS prep, but if you want to run a stage 1 model with a different top-coding, you also have to re-run the scripts for HRS and NHATS data:

*HRS_crosswave_function_final.R* for HRS hours and expected caregiver wage
*estimate_national_caregiving_hours_OP_NHATS.R* for NHATS hours
*hourly_cost_of_care_OP_CPS.R* for NHATS expected caregiver wage

This preps the HRS and NHATS data using the appropriate top-coding.


