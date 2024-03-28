## Perform Das Gupta decomposition
## 02/21/2023
## Author: Amy Lastuka
### Purpose: calculate Das Gupta decomposition for the following factors: 
##          - care hours (scaled by attributable fraction)
##          - population (both total pop and 65+)
##          - cost per hour (either HHA wage or OC model wage)
##          - dementia prevalence
##########################################################################

rm(list=ls())

library(data.table)

data_dir <- "FILEPATH"
out_dir <- "FILEPATH"

today <- gsub('-','_',Sys.Date())
model_flag <- "RC" # can be "RC" or "OC"
top_code <- "112" # can be 168, 112, or 40
population_var <- "population" # can be "population" or "pop65plus"

output_file <- paste0("das_gupta_",model_flag,"_",population_var,"_",top_code,"_",today,".csv")


# read in total cost data - this has all info needed to make either an RC or an OC decomp table
cost_file <- "dasgupta_input_table_rtr.csv"
costData <- fread(paste0("FILEPATH/",cost_file))

# also read in the file with the dementia prevalence ratio (non-standardized to standardized ratio) and merge
dem_prev <- fread(paste0(data_dir,"dementia_prev_ratio.csv"))
costData <- merge(costData, dem_prev, by = "State")

## ---- set up the variable names to work for both RC and OC models 
if(model_flag=="RC"){
  if(top_code=="168"){
    setnames(costData,"care_hours_week_168", "caregiving_hours")
  }else if(top_code=="112"){
    setnames(costData,"care_hours_week_112", "caregiving_hours")
  }else{
    paste0("No results for RC model with top code",top_code)
  }
  setnames(costData,"Replacement_Cost_Per_Hour", "cost_per_hour")
  setnames(costData,c("Total_Replacement_Cost","Total_Replacement_Cost_lower","Total_Replacement_Cost_upper"), c("total_cost","total_cost_lower","total_cost_upper"))
}else{
  if(top_code=="112"){
    setnames(costData,"care_hours_week_112", "caregiving_hours")
  }else if(top_code=="40"){
      setnames(costData,"care_hours_week_40", "caregiving_hours")
  }else{
    paste0("No results for OC model with top code",top_code)
  }
  setnames(costData,"Opportunity_Cost_Per_Hour", "cost_per_hour")
  setnames(costData,c("Total_Opportunity_Cost","Total_Opportunity_Cost_lower","Total_Opportunity_Cost_upper"), c("total_cost","total_cost_lower","total_cost_upper"))
}

## ---- set up the variable names to work for population or pop65plus
if(population_var=="pop65plus"){
  setnames(costData,"pop65plus", "pop_var")
}else{
  costData[, pop_var := population]
}

# the OC model has a different cheapest state - set the baseline state for each model
if(model_flag=="RC"){
  baseline_state<-"Utah"
}else if(model_flag=="OC"){
  baseline_state<-"Alaska"
}

num_factors <- 4

# if we are doing 4 factors, that means we're using both age-standardized and non-standardized prevalence
# versus the original 3-factor chart which only used non-standardized prevalence
# NOTE: to create the age-standardized prevalence I used the get_outputs function to keep things simple 
#       (those variables, loaded into the dem_prev DT above, are created in demenetia_age_std.R)
#       We are only using these for the das Gupta visualization; if we were using them for further calculations
#       it would be better to use the dementia envelope prevalence numbers
if(num_factors==4){
  setnames(costData,"dementia_std", "prev_var") 
  # also if we are using these prevalence values we have to re-calculate total cost, because we are not using the dementia envelope numbers
  costData[, total_cost := pop_var*dementia_nonstd*caregiving_hours*cost_per_hour*52]
}else{
  setnames(costData,"prevalence", "prev_var") 
}

### ------ setup per-capita cost, diff from baseline, and bounds --------#
##  ------ before starting the actual DasGupta calculation --------------#

# convert cost to per-capita space and create bounds
costData[, per_capita_cost := (total_cost/pop_var)]
costData[, per_capita_cost_upper := (total_cost_upper/pop_var)]
costData[, per_capita_cost_lower := (total_cost_lower/pop_var)]
baseline_data <- costData[State==baseline_state,]
per_capita_baseline <- baseline_data$per_capita_cost
per_capita_baseline_upper <- baseline_data$per_capita_cost_upper
per_capita_baseline_lower <- baseline_data$per_capita_cost_lower


# the "total_diff" is the difference between each state and the baseline (as measured in that
# specific order: state - baseline. E.g. it is directional, not an absolute value.)
# So, to create a lower bound for that difference, I use the  highest possible number for the
# baseline and the lowest possible number for the state cost. That way those two estimates are the 
# closest together they could be. And vice versa for the upper bound. 
costData[, total_diff_lower := per_capita_cost_lower - per_capita_baseline_upper]
costData[, total_diff_upper := per_capita_cost_upper - per_capita_baseline_lower]

# following notation from the papera; 
# 4 factors - prev_ratio = a; prevalence = b; wages = c  hours = d; 

# -------- Replacement Cost -------------------------

# get baseline from the national cost, not just a simple mean
ratio_baseline <- baseline_data$dem_prev_ratio #ratio for the US is 1
prev_baseline <- baseline_data$prev_var
wage_baseline <- baseline_data$cost_per_hour
hours_baseline <- baseline_data$caregiving_hours

if(num_factors == 4){
  # dividing by 52 to convert annual costs back to weekly cost - caregiving hours are also weekly so these should match
  costData[, cost_diff :=(per_capita_cost - per_capita_baseline)/52 ]
  
  
  costData[, alpha_effect := ( (prev_var*cost_per_hour*caregiving_hours + prev_baseline*wage_baseline*hours_baseline)/4 + 
              (prev_var*cost_per_hour*hours_baseline + prev_var*wage_baseline*caregiving_hours + 
                 prev_baseline*cost_per_hour*caregiving_hours + prev_baseline*wage_baseline*caregiving_hours + 
                 prev_baseline*cost_per_hour*hours_baseline + prev_var*wage_baseline*hours_baseline )/12 )*(dem_prev_ratio - ratio_baseline)]
             
             
  costData[, beta_effect := ( (dem_prev_ratio*cost_per_hour*caregiving_hours + ratio_baseline*wage_baseline*hours_baseline)/4 + 
                                 (dem_prev_ratio*cost_per_hour*hours_baseline + dem_prev_ratio*wage_baseline*caregiving_hours + 
                                    ratio_baseline*cost_per_hour*caregiving_hours + ratio_baseline*wage_baseline*caregiving_hours + 
                                    ratio_baseline*cost_per_hour*hours_baseline + dem_prev_ratio*wage_baseline*hours_baseline )/12 )*(prev_var - prev_baseline)]
  
             
  costData[, gamma_effect := ( (prev_var*dem_prev_ratio*caregiving_hours + prev_baseline*ratio_baseline*hours_baseline)/4 + 
                                   (prev_var*dem_prev_ratio*hours_baseline + prev_var*ratio_baseline*caregiving_hours + 
                                      prev_baseline*dem_prev_ratio*caregiving_hours + prev_baseline*ratio_baseline*caregiving_hours + 
                                      prev_baseline*dem_prev_ratio*hours_baseline + prev_var*ratio_baseline*hours_baseline )/12 )*(cost_per_hour - wage_baseline)]
  
  
  costData[, delta_effect := ( (prev_var*cost_per_hour*dem_prev_ratio + prev_baseline*wage_baseline*ratio_baseline)/4 + 
                                 (prev_var*cost_per_hour*ratio_baseline + prev_var*wage_baseline*dem_prev_ratio + 
                                    prev_baseline*cost_per_hour*dem_prev_ratio + prev_baseline*wage_baseline*dem_prev_ratio + 
                                    prev_baseline*cost_per_hour*ratio_baseline + prev_var*wage_baseline*ratio_baseline )/12 )*(caregiving_hours - hours_baseline)]
  
  
  costData[, total_RC_diff := total_cost  - ratio_baseline*prev_baseline*wage_baseline*hours_baseline]
  
  costData[, sum_of_effects := alpha_effect + beta_effect + gamma_effect + delta_effect]
  

  costData[, ratio_percent_change := alpha_effect/(per_capita_cost/52)]
  costData[, prevalence_percent_change := beta_effect/(per_capita_cost/52)]
  costData[, replacement_wage_percent_change := gamma_effect/(per_capita_cost/52)]
  costData[, caregiving_hours_percent_change := delta_effect/(per_capita_cost/52)]
  
  
  # look at final outputs
  output_table <- costData[, .(State, total_cost,per_capita_cost,
                               total_diff = cost_diff*52,
                               total_diff_lower,
                               total_diff_upper, 
                               ratio_diff = alpha_effect*52,
                               prevalence_diff = beta_effect*52, 
                               cost_diff = gamma_effect*52, 
                               hours_diff = delta_effect*52)]
  
  fwrite(output_table, paste0(out_dir,output_file))
}else{ #num factors ==3

  ### ------- 3 factors -  prevalence = a; wages = b;  hours = c; 
  
  # dividing by 52 to convert annual costs back to weekly cost - caregiving hours are also weekly so these should match
  costData[, cost_diff :=(per_capita_cost - per_capita_baseline)/52 ]
  
  costData[, alpha_effect := ( (cost_per_hour*caregiving_hours + wage_baseline*hours_baseline)/3 + 
                                 (cost_per_hour*hours_baseline + wage_baseline*caregiving_hours)/6 )*(prev_var - prev_baseline)]
  
  costData[, beta_effect := ( (prev_var*caregiving_hours + prev_baseline*hours_baseline)/3 + 
                                 (prev_var*hours_baseline + prev_baseline*caregiving_hours)/6 )*(cost_per_hour - wage_baseline)]
  
  
  costData[, gamma_effect := ( (prev_var*cost_per_hour + prev_baseline*wage_baseline)/3 + 
                                (prev_var*wage_baseline + prev_baseline*cost_per_hour)/6 )*(caregiving_hours - hours_baseline)]
  
  
  costData[, prevalence_percent_change := alpha_effect/(per_capita_cost/52)]
  costData[, replacement_wage_percent_change := beta_effect/(per_capita_cost/52)]
  costData[, caregiving_hours_percent_change := gamma_effect/(per_capita_cost/52)]
  costData[, total_percent_change := (per_capita_cost - per_capita_baseline)/per_capita_baseline]
  

  
  output_table <- costData[, .(State, total_cost,per_capita_cost,
                               total_diff = cost_diff*52,
                               total_diff_lower,
                               total_diff_upper, 
                               prevalence_diff = alpha_effect*52, 
                               cost_diff = beta_effect*52, 
                               hours_diff = gamma_effect*52)]
  
  fwrite(output_table, paste0(out_dir,output_file))
}

### quick calculation to see which of the factors is the largest

total_diff <- sum(abs(output_table$total_diff))
total_ratio <- sum(abs(output_table$ratio_diff))
total_prev <- sum(abs(output_table$prevalence_diff))
total_cost <- sum(abs(output_table$cost_diff))
total_hours <- sum(abs(output_table$hours_diff))
