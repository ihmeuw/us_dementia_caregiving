# ST-GPR Post Processing

## Notes
The scripts in this subfolder are used to process the outputs from ST-GPR. 

The scripts with the `aggregate_and_plot_STGPR_output_` prefix are used to aggregate the means from ST-GPR and plot the outputs. The plots are used to vet the outputs from ST-GPR. These scripts are: 
  - `aggregate_and_plot_STGPR_output_care_hours_112.R` for Replacement Cost
  - `aggregate_and_plot_STGPR_output_care_hours_40.R` for Forgone Wages
  - `aggregate_and_plot_STGPR_output_care_wages_40.R` for Forgone Wages

Once the ST-GPR outputs have been validated, the scripts with the `_aggregate_draws.R` suffix are used to take the draws from the ST-GPR output and create the US national estimates by aggregating the state-level estimates. The state-level estimates and the aggregated US national estimates are then concatenated at the draw-level and saved out in a single data file. These scripts are: 
  - `care_hours_112_aggregate_draws.R` for Replacement Cost
  - `care_hours_40_aggregate_draws.R` for Forgone Wages
  - `care_wages_40_aggregate_draws.R` for Forgone Wages
  
The outputs from these scripts are used to calculate the final cost estimates in `~/4_final_estimates/`.

**Author: Michael Breshock**\
*Last Update: 03/27/2024*