# Running ST-GPR

## Notes
The files in this subfolder are used to run ST-GPR for both the replacement cost and forgone wage models. The following scripts are used to run ST-GPR for caregiving hours with different top-codes: 
  - `run_ST_GPR_care_hours_112.R` - This script runs ST-GPR for caregiving hours top-coded at 112 hours per week. This is used for replacement cost. 
  - `run_ST_GPR_care_hours_40.R` - This script runs ST-GPR for caregiving hours top-coded at 40 hours per week. This is used for forgone wages.
  
The third script in this subfolder is used to run ST-GPR for the caregivers' hourly wages. This is used for the forgone wage model: 
  - `run_ST_GPR_care_wages.R`

To run these scripts, first prepare the config file with the parameters you want to run ST-GPR with. There are config files associated with each of the scripts above. These files are located in the following directories: 
  - `/FILEPATH/care_hours_112/brfss_us_st_gpr_config_file.csv` - Hours used for Replacement Cost
  - `/FILEPATH/care_hours_40/care_hours_topcode40_config.csv` - Hours used for Forgone Wages
  - `/FILEPATH/care_wages/brfss_us_st_gpr_config_file.csv` - Wages used for Forgone Wages
  
Each row in these files contains the parameters for an ST-GPR model run. These are numbered with the `model_index_id` column. In the run_ST_GPR scripts, make sure the `model_num` variable is set to the `model_index_id` number associated with the paramaters you want to use in the config file. The `register_stgpr_model()` shared function will use these paramaters to register your ST-GPR run on the cluster and create a run ID/version ID (run ID is the old terminology, version ID is the most recent name, but they represent the same thing). 

`stgpr_sendoff()` is used to initiate ST-GPR with the version ID created by the register function. While the model is running, you can check the status of the model using `get_model_status()`. If the ST-GPR run has an error, the error logs will be saved to the directory specified in the `logs` variable defined in the beginning of the script. 

Once the run is finished, the rest of the script is used to read and save both the ST-GPR draws as well as the GPR means/bounds from the draws. We don't save the final (raked) estimates from ST-GPR for our project since we implement our own aggregation method in `~/3_post_processing/`.

**Author: Michael Breshock**\
*Last Update: 03/27/2024*
