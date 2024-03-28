# BRFSS helper functions
# Author: Amy Lastuka
# Purpose: Define functions for processing BRFSS files and bootstrap data

# ---------------------------------------------------------------------
#      FUNCTIONS to process BRFSS files and bootstrap data
#       1)  nac_sample
#       2)  state_resample
#       3)  create_brfss_cg
# --------------------------------------------------------------------


source(file.path("FILEPATH/get_location_metadata.R"))
hierarchy <- get_location_metadata(location_set_id = 35, release_id = 6)
name_dict <- hierarchy[parent_id==102, .(location_id, location_name)]



# function that takes a distribution which is a list of lists, an index, and a number of counts
# the index indicates which of the sub-lists is being sampled from and the counts are how many times to sample 
# Whether we are using NAC, BRFSS, or something else as the reference distribution, it is broken up into 4 lists
# these lists are based on the buckets in the BRFSS caregiving question (0-8 hours, 9-19 hours, etc.)
nac_sample <- function(hours_list, index, counts=1, nac_flag){
  #index <- df$CRGVHRS1
  if(nac_flag==1){
    outlist <-  sample(nac_dist[[index]], size=counts, replace=T) 
  }else{
    outlist <-  sample(early_brfss_dist[[index]], size=counts, replace=T) 
  }
  return(outlist)
}


# function that samples from a reference distribution to get a point estimate for caregiving hours
#     This is performing a bootstrap to get an estimated distribution for caregiving hours
#     For each draw, I am sampling with replacement from the BRFSS caregiving data, then
#     for each BRFSS response, I am sampling from the reference distribution (either NAC or early BRFSS). 
#     The BRFSS data is in bins e.g. 0-8 hours, 9-19 hours, etc. So I split the NAC caregiving data into 4 lists, one
#     for each of those hour ranges. Then if someone chose 0-8 hours of caregiving, I sample
#     from the NAC 0-8 hours list. An alternative would be to just take the mean of each bin, 
#     e.g. if someone chose 0-8 hours I assign them 4 hours. But this way we don't have to assume
#     a uniform distribution. If I do 100 draws, I then have 100 estimates for mean caregiving hours.
#     These 100 estimates are my distribution, and I use the 'quantile' function later to get the bounds. 

#     TL;DR - takes a 'caregiving data frame' as an input (has to include the variable CRGVHRS1 )
#             returns a point estimate for caregiving hours per case, and a variance estimate
state_resample <- function(df,cost_flg=0, race_flag=0){
  # step 0: resample the BRFSS data first
  #         SEQNO is the unique ID for each BRFSS respondent 
  ids <- df[, unique(SEQNO)]
  sample_list <- data.table(SEQNO = sample(ids, replace = T))
  df2 <- merge(sample_list, df, by = "SEQNO", all.x = T, sort = F, allow.cartesian = T)
  
  # step 1: how many of each bin do we need? (The table function returns the count for each bin)
  # Note: ignore the 'don't know/no answer' folks for the purpose of getting a # of hours
  counts_df <- data.frame(df2[(CRGVHRS1 <=4), table(CRGVHRS1)])
  #print(counts_df)
  # step 2: sample w/ replacement from NAC
  temp_lists <- mapply(nac_sample, outlist, counts_df$CRGVHRS1, counts_df$Freq, nac_flag)
  full_list <- unlist(temp_lists)
  
  # extend full_list so we capture everyone who did caregiving
  # use average CG hours for everyone who didn't answer 
  estimated_mean <- mean(full_list)
  num_missing <- nrow(df) - length(full_list)
  
  extra_list <- rep(estimated_mean, num_missing)
  full_list <- append(full_list, extra_list)
  
  
  # Note about weights: because the different file types have slightly different names for the weights, I mapped them all
  #                     to "WEIGHT". This represents the final weight, usually called something close to _LLCPWT in the data
  #                     which stands for land line cell phone weight. This is the 'final weight' meaning that it accounts for
  #                     the survey design (PSU and strata) as well as non-response. Good documentation on the weight 
  #                     creation is here: https://www.cdc.gov/brfss/annual_data/2017/pdf/weighting-2017-508.pdf
  
  # append list onto df2
  df2 <- df2[order(CRGVHRS1)] #df2 was the re-sampled dataframe
  df2 <- cbind(df2, full_list)
  df2[, wtd_hrs := WEIGHT*full_list]
  
  if(cost_flg==1){
    df2[, wtd_wage := wtd_hrs*adj_wage]
  }
  
  # calculate hours for only dementia patients if dementia flag==1
  # CRGVPRB2 is the variable for the question 'What Is The Major Health Problem, Illness, Disability For Care For Person?'
  #  (it actually had a few different names over the years so I mapped them all to CRGVPRB2)
  # CRGALZD is a variable that is only present starting in 2019 - it asks 
  #   "Does the person you care for also have Alzheimer´s disease, dementia or other cognitive impairment disorder?"
  if(dementia_flag==1){
    df2 <- df2[CRGVPRB2==5,]
  }else if(dementia_flag==2){
    df2 <- df2[(CRGVALZD==1) | (CRGVPRB2==5),]
  }else{
    df2 <- df2[(CRGVALZD==1) & (CRGVPRB2!=5),]
  }
  
  if(cost_flg==0){
    if(race_flag==0){
      cg_metric <- df2[, sum(wtd_hrs)] # for the replacement cost model, the metric is total hours
    }else{
      cg_metric <- df2[, sum(wtd_hrs), by=c("RACE")]
    }
  }else if(cost_flg==1){
    cg_metric <- df2[, sum(wtd_wage)/sum(wtd_hrs)] # for the opportunity cost model, the metric is average wage
    #rename the age category variable
    #setnames(sum_hours,"AGE5YR","AGECAT")
  }else{
    print("In state_resample function. Unknown cost flag value")
  }
  
  sample_var <- wtd.var(df2$full_list, df2$WEIGHT)
  bootstrap_list <- list(cg_metric,sample_var )
  
  #return(bootstrap_list)
  #print(head(cg_metric))
  return(cg_metric)
}


# ---------------------------------------------------------------
# state resample function, but now including prevalence draws
# Note: prev_draws is subset to the year & state and shaped long (vs. wide) before passing here
state_resample_draw_space <- function(caregiving_df, prev_draws, draw_num, cost_flg=0){
  # step 0: resample the BRFSS data first
  #         SEQNO is the unique ID for each BRFSS respondent 
  ids <- caregiving_df[, unique(SEQNO)]
  sample_list <- data.table(SEQNO = sample(ids, replace = T))
  df2 <- merge(sample_list, caregiving_df, by = "SEQNO", all.x = T, sort = F, allow.cartesian = T)
  
  # step 1: how many of each bin do we need? (The table function returns the count for each bin)
  # Note: ignore the 'don't know/no answer' folks for the purpose of getting a # of hours
  counts_df <- data.frame(df2[(CRGVHRS1 <=4), table(CRGVHRS1)])
  
  
  # sometimes we have a bin with zero counts
  bin_list <- unique(counts_df$CRGVHRS1)
  outlist_temp <- outlist[c(bin_list)]
  
  # step 2: sample w/ replacement from NAC
  temp_lists <- mapply(nac_sample, outlist_temp, counts_df$CRGVHRS1, counts_df$Freq, nac_flag)
  full_list <- unlist(temp_lists)
  
  # extend full_list so we capture everyone who did caregiving
  # use average CG hours for everyone who didn't answer 
  estimated_mean <- mean(full_list)
  num_missing <- nrow(df2) - length(full_list)
  if(num_missing < 0){
    print(counts_df)
    print(paste0("Estimated mean: ",estimated_mean, " Num missing: ", num_missing, "DF2 length: ",nrow(df2), "Full list length: ",length(full_list) ))
  }
  
  extra_list <- rep(estimated_mean, num_missing)
  full_list <- append(full_list, extra_list)
  
  
  # Note about weights: because the different file types have slightly different names for the weights, I mapped them all
  #                     to "WEIGHT". This represents the final weight, usually called something close to _LLCPWT in the data
  #                     which stands for land line cell phone weight. This is the 'final weight' meaning that it accounts for
  #                     the survey design (PSU and strata) as well as non-response. Good documentation on the weight 
  #                     creation is here: https://www.cdc.gov/brfss/annual_data/2017/pdf/weighting-2017-508.pdf
  
  # append list onto df2
  df2 <- df2[order(CRGVHRS1)] #df2 was the re-sampled dataframe
  df2 <- cbind(df2, full_list)
  df2[, wtd_hrs := WEIGHT*full_list]
  if(cost_flg==1){
    df2[, wtd_wage := wtd_hrs*adj_wage]
  }
  
  # calculate hours for only dementia patients if dementia flag==1
  # CRGVPRB2 is the variable for the question 'What Is The Major Health Problem, Illness, Disability For Care For Person?'
  #  (it actually had a few different names over the years so I mapped them all to CRGVPRB2)
  if(dementia_flag==1){
    df2 <- df2[CRGVPRB2==5,]
  }else if(dementia_flag==2){
    df2 <- df2[(CRGVALZD==1) | (CRGVPRB2==5),]
  }else{
    df2 <- df2[(CRGVALZD==1) & (CRGVPRB2!=5),]
  }
  if(draw_num==1){
    print(paste0("After subsetting based on dementia flag, num samples: ", nrow(df2)))
  }
  
  if(cost_flg==0){
    cg_total_hours <- df2[, sum(wtd_hrs)] # for the replacement cost model, the metric is total hours
    draw_string <- paste0("draw_",draw_num)
    prevalence_draw <- prev_draws$dem_cases[prev_draws$variable == draw_string]
    cg_metric <- cg_total_hours/prevalence_draw
  }else if(cost_flg==1){
    cg_metric <- df2[, sum(wtd_wage)/sum(wtd_hrs)] # for the opportunity cost model, the metric is average wage
    #rename the age category variable
    #setnames(sum_hours,"AGE5YR","AGECAT")
  }else{
    print("In state_resample function. Unknown cost flag value")
  }
  
  sample_var <- wtd.var(df2$full_list, df2$WEIGHT)
  bootstrap_list <- list(cg_metric,sample_var )
  
  return(bootstrap_list)
  #return(cg_metric)
}


##### function to read in data, clean up variable names and subset to caregivers only
create_brfss_cg <- function(year, filename,ext,wgt){
  if(ext=="dta"){
    brfss_data <- read_dta(paste0(data_dir,year,"/",filename))
    setnames(brfss_data, c("_STATE","_AGEG5YR"), c("FIPS","AGE5YR"))
    if(year==2019){
      setnames(brfss_data,c("_SEX","CRGVPRB3"),c("SEX","CRGVPRB2"))
    }
    else if(year==2018){
      setnames(brfss_data,"SEX1","SEX")
    }
    else if(year==2015){
      setnames(brfss_data,"CRGVPRB1","CRGVPRB2")
    }
  }
  else{
    brfss_data <- read.xport(paste0(data_dir,year,"/",filename))
    setnames(brfss_data, c("X_STATE","X_AGEG5YR"), c("FIPS","AGE5YR"))
    if(year==2019){
      setnames(brfss_data,c("X_SEX","CRGVPRB3"),c("SEX","CRGVPRB2"))
    }
    else if(year==2018){
      setnames(brfss_data,"SEX1","SEX")
    }
    else if(year==2015){
      setnames(brfss_data,"CRGVPRB1","CRGVPRB2")
    }
  }
  # sometimes SEQNO is lowercase, so just convert everything to uppercase
  names(brfss_data) <- toupper(names(brfss_data))
  setnames(brfss_data,c(wgt),c("WEIGHT"))
  #print(head(brfss_data$WEIGHT))
  brfss_data <- data.table(brfss_data)
  # caregiv1 == 1 is "yes" to caregiving in past 30 days;
  #             8 is care recipient died in past 30 days
  cg_datatable <- brfss_data[(CAREGIV1==1) | (CAREGIV1==8)]
  return(cg_datatable)
}


# this function takes a caregiving datatable (BRFSS file that has been subset to caregivers via create_brfss_cg)
# loops over each state (BRFSS files generally contain multiple states), and performs boostrapping to get 
# estimates for either caregiving hours (if cost_flag is 0), or expected hourly wage for caregivers (if cost_flag is 1)
process_cg <- function(brfss_cg, prev_draws, cost_flag=0){
  print(paste0("inside process_cg, cost flag = ", cost_flag))
  # COLLAPSE DEMOG VARS AS NEEDED
  if(collapse_age==1){
    brfss_cg[AGE5YR <=3, age_tmp := "18-34"]
    brfss_cg[(AGE5YR >3) & (AGE5YR <=5), age_tmp := "35-44"]
    brfss_cg[(AGE5YR >5) & (AGE5YR <=7), age_tmp := "45-54"]
    brfss_cg[(AGE5YR >7) & (AGE5YR <=9), age_tmp := "55-64"]
    brfss_cg[(AGE5YR >9) & (AGE5YR <=11), age_tmp := "65-74"]
    brfss_cg[(AGE5YR >11) & (AGE5YR <=13), age_tmp := "75+"]
    brfss_cg[AGE5YR==14, age_tmp := NA]
    brfss_cg$AGE5YR <- brfss_cg$age_tmp
    brfss_cg <- brfss_cg[!is.na(AGE5YR),]
    #create new name because it's not a 5 year increment anymore
    brfss_cg[, AGECAT := AGE5YR]
  } else{
    brfss_cg <- brfss_cg[AGE5YR < 14,] #remove non-responses for age
  }
  
  if(collapse_educ==1){
    brfss_cg[EDUCA <=4, educ := "High school or less"]
    brfss_cg[EDUCA ==5, educ := "Some college"]
    brfss_cg[EDUCA==6, educ := "Bachelor's degree or more"]
    brfss_cg[EDUCA==9, educ := NA]
    brfss_cg$EDUCA <- brfss_cg$educ
    brfss_cg <- brfss_cg[!is.na(EDUCA),]
  } else{
    brfss_cg <- brfss_cg[EDUCA < 9,]
  }
  # remove if missing sex variable
  brfss_cg <- brfss_cg[(SEX==1) | (SEX==2),]
  
  
  # convert FIPS code to state name; create list of states to loop over
  brfss_cg[, state := cdlTools::fips(FIPS,to='Name')]
  state_list <- brfss_cg[, unique(state)]
  
  
  ###################################
  #### if cost_flag = 1, also merge in the CPS wage data
  ####################################
  if(cost_flag == 1){
    brfss_cg$year <- year
    brfss_cg <- merge(brfss_cg, cps_data, by = c("state", "SEX", "AGECAT", "EDUCA","year"))
  }
  
  # note: Puerto Rico can end up in the state list but has 0 rows of data
  state_list <- state_list[state_list != "Puerto Rico"]
  
  
  # ----- BEGIN: STATE LEVEL PROCESSING: boostrap caregiving hours by specified demographics for each state
  if(cost_flag==0){
    brfss_dt <- data.table( cg_per_case = as.numeric(), cg_var_per_case = as.numeric(), lower_per_case = as.numeric(), 
                            upper_per_case = as.numeric(), state = as.character(), year = as.character(), sample_size = as.numeric()) 
  } else if(cost_flag==1){
    brfss_dt <- data.table( cost_mean = as.numeric(),cost_var = as.numeric(),  cost_lower = as.numeric(), 
                            cost_upper = as.numeric(),state = as.character(), year = as.numeric(), sample_size = as.numeric()) 
  } else{
    print("Unknown value for cost flag")
  }
  
  for(st in state_list){
    df_state <- brfss_cg[state==st,]
    df_state$avg_wgt <- mean(df_state$WEIGHT)
    cg_draws = data.table()
    temp = data.table()
    print(paste0("State = ", st, " year = ", year))
    print(paste0("num samples: ", nrow(df_state)))
    # before calling state_resample, set up prev_draws for the current state/year
    prev_draws <- data.table(prev_draws)
    #print(head(prev_draws$year_id))
    prev_row <- prev_draws[(year_id==year) & (location_id==name_dict$location_id[name_dict$location_name==st]),]
    
    col_list <- paste0("draw_",0:999)
    prev_melt = melt(prev_row , id.vars = c("location_id", "age_group_id","sex_id","year_id","population"),
                 measure.vars = col_list)
    setDT(prev_melt)
    prev_melt[, dem_cases := value*population]
    for(i in 1:draws){
      if(nrow(cg_draws)==0){
        bootstrap_output <- state_resample_draw_space(caregiving_df = df_state,prev_draws = prev_melt, draw_num = (i-1), cost_flg = cost_flag) # this is now a list; the first item is the sum of cg hours; second item is sample variance
        cg_draws <- data.table(bootstrap_output[[1]])
        if(i < 5){
          print(paste0("Draw ",i,": per-case hours estimate ", cg_draws))
        }
        cg_var_draws <- data.table(bootstrap_output[[2]])
        #print(head(cg_draws))
        #print(i)
        #if(cost_flag==0){
        names(cg_draws) <- c(paste0("draw",i))
        names(cg_var_draws) <- c(paste0("draw",i))
      }
      else{
        bootstrap_output <- state_resample_draw_space(caregiving_df = df_state,prev_draws = prev_melt, draw_num = (i-1), cost_flg = cost_flag)
        cg_draws_temp <- data.table(bootstrap_output[[1]])
        cg_var_draws_temp <- data.table(bootstrap_output[[2]])
        #print(i)
        #if(cost_flag==0){
        names(cg_draws_temp) <- c(paste0("draw",i))
        names(cg_var_draws_temp) <- c(paste0("draw",i))
        cg_draws <- cbind(cg_draws_temp,cg_draws)
        cg_var_draws <- cbind(cg_var_draws_temp,cg_var_draws)
      }
    }
    # create mean, lower, and upper bound of draws
    col_list <- paste0("draw",1:draws,",")
    col_list <- substr(col_list,1,nchar(col_list)-1)
    
    cg_draws$cg_per_case <- apply(subset(cg_draws, select = col_list), 1, mean)
    cg_draws$cg_var <- apply(subset(cg_draws, select = col_list), 1, var)
    cg_draws$lower_per_case <- apply(subset(cg_draws, select = col_list), 1, quantile, probs=.025)
    cg_draws$upper_per_case <- apply(subset(cg_draws, select = col_list), 1, quantile, probs=.975)
    
    if(cost_flag==1){
      cg_draws$cg_var_per_case <- apply(subset(cg_var_draws, select = col_list), 1, mean)
    }else{
      # upper_per_case and lower_per_case are bounds from the N draws of caregiving hours per-capita
      # Using the variance of those N draws would give the variance of the sample estimator, which 
      # would be too small. We want the sample variance. So I am calculating the standard error from
      # the bounds, and then using N*(standard_error)^2 to get the sample variance. 
      cg_draws$st_error <- (cg_draws$upper_per_case - cg_draws$lower_per_case)/(1.96*2)
      cg_draws$cg_var_per_case <- draws*cg_draws$st_error^2
    }
    
    temp_dt <- cg_draws[,c("cg_per_case","cg_var_per_case", "lower_per_case", "upper_per_case")]
    #if(cost_flag==0){
    #  temp_dt <- cg_draws[,c("cg_sum","cg_var", "cg_lower", "cg_upper")]
    #} else if(cost_flag==1){
    if(cost_flag==1){
      setnames(temp_dt,c("cg_per_case","cg_var_per_case", "lower_per_case", "upper_per_case"),c("cost_mean","cost_var", "cost_lower", "cost_upper"))
    } 
    #else{
    #  print("Unknown cost flag value")
    #}
    temp_dt$state <- st
    temp_dt$year <- year
    
    #####################################################
    ##### also include sample size for each group
    ###################################################
    if(dementia_flag==1){
      df <- df_state[CRGVPRB2==5,]
    }else if(dementia_flag==2){
      df <- df_state[(CRGVALZD==1) | (CRGVPRB2==5),]
    }else{
      df <- df_state[(CRGVALZD==1) & (CRGVPRB2!=5),]
    }
    
    # add sample size for each state
    foo <- df[, .N]
    foo <- data.table(foo)
    names(foo) <- c("sample_size")
    temp_dt <- cbind(temp_dt, foo)
    
    brfss_dt <- rbind(brfss_dt, temp_dt)
  }
  
  # return a dataframe row(s) with results
  return(brfss_dt)
}


# takes a caregiving datatable and returns per-caregiver estimates of hours 
# this function can take a race_flag argument - if race_flag = 1, then hours are aggregated by race
# it calls state_resample; not state_resample_draw_space
# this function is used for exploratory analysis and appendix tables
hours_per_cg <- function(brfss_cg, race_flag=0){
  
  # COLLAPSE DEMOG VARS 
  brfss_cg[AGE5YR <=3, age_tmp := "18-34"]
  brfss_cg[(AGE5YR >3) & (AGE5YR <=5), age_tmp := "35-44"]
  brfss_cg[(AGE5YR >5) & (AGE5YR <=7), age_tmp := "45-54"]
  brfss_cg[(AGE5YR >7) & (AGE5YR <=9), age_tmp := "55-64"]
  brfss_cg[(AGE5YR >9) & (AGE5YR <=11), age_tmp := "65-74"]
  brfss_cg[(AGE5YR >11) & (AGE5YR <=13), age_tmp := "75+"]
  brfss_cg[AGE5YR==14, age_tmp := NA]
  brfss_cg$AGE5YR <- brfss_cg$age_tmp
  brfss_cg <- brfss_cg[!is.na(AGE5YR),]
  #create new name because it's not a 5 year increment anymore
  brfss_cg[, AGECAT := AGE5YR]

  brfss_cg[EDUCA <=4, educ := "High school or less"]
  brfss_cg[EDUCA ==5, educ := "Some college"]
  brfss_cg[EDUCA==6, educ := "Bachelor's degree or more"]
  brfss_cg[EDUCA==9, educ := NA]
  brfss_cg$EDUCA <- brfss_cg$educ
  brfss_cg <- brfss_cg[!is.na(EDUCA),]

  # remove if missing sex variable
  brfss_cg <- brfss_cg[(SEX==1) | (SEX==2),]
  
  
  # convert FIPS code to state name; create list of states to loop over
  brfss_cg[, state := cdlTools::fips(FIPS,to='Name')]
  state_list <- brfss_cg[, unique(state)]
  

  # note: Puerto Rico can end up in the state list but has 0 rows of data
  state_list <- state_list[state_list != "Puerto Rico"]
  
  
  # ----- BEGIN: STATE LEVEL PROCESSING: boostrap caregiving hours by specified demographics for each state
  if(race_flag==0){
    brfss_dt <- data.table( cg_sum = as.numeric(), cg_lower = as.numeric(), cg_upper = as.numeric(), 
                            state = as.character(), year = as.character(), num_caregivers = as.numeric(), 
                            sample_size = as.numeric()) 
  }else{
    brfss_dt <- data.table( cg_sum = as.numeric(), cg_lower = as.numeric(), race = as.numeric(),
                            cg_upper = as.numeric(), state = as.character(), year = as.character(), num_caregivers = as.numeric(), sample_size = as.numeric()) 
  }
  
  
  for(st in state_list){
    df_state <- brfss_cg[state==st,]
    df_state$avg_wgt <- mean(df_state$WEIGHT)
    cg_draws = data.table()
    temp = data.table()

    for(i in 1:draws){
      if(nrow(cg_draws)==0){
        bootstrap_output <- state_resample(df_state, cost_flg = 0, race_flag) # this is now a list; the first item is the sum of cg hours; second item is sample variance
        cg_draws <- data.table(bootstrap_output)
        #cg_var_draws <- data.table(bootstrap_output[[2]])
        #print("cg_draws: ")
        #print(dim(cg_draws))
        #print(head(cg_draws))
        #print(i)
        #if(cost_flag==0){
        if(race_flag==0){
          names(cg_draws) <- c(paste0("draw",i))
        }else {
          names(cg_draws) <- c("RACE",paste0("draw",i))
        }
        #names(cg_var_draws) <- c(paste0("draw",i))
      }
      else{
        bootstrap_output <- state_resample(df_state, cost_flg = 0, race_flag)
        cg_draws_temp <- data.table(bootstrap_output)
        #cg_var_draws_temp <- data.table(bootstrap_output[[2]])
        #print(i)
        #if(cost_flag==0){
        if(race_flag==0){
          names(cg_draws_temp) <- c(paste0("draw",i))
          cg_draws <- cbind(cg_draws_temp,cg_draws)
        }else {
          names(cg_draws_temp) <- c("RACE",paste0("draw",i))
          cg_draws <- merge(cg_draws_temp,cg_draws, by=c("RACE"), allow.cartesian=TRUE)
        }
        #names(cg_var_draws_temp) <- c(paste0("draw",i))
        #cg_var_draws <- cbind(cg_var_draws_temp,cg_var_draws)
      }
    }
    # create mean, lower, and upper bound of draws
    col_list <- paste0("draw",1:draws,",")
    col_list <- substr(col_list,1,nchar(col_list)-1)
    
    cg_draws$cg_sum <- apply(subset(cg_draws, select = col_list), 1, mean)
    #cg_draws$cg_var <- apply(subset(cg_draws, select = col_list), 1, var)
    cg_draws$cg_lower <- apply(subset(cg_draws, select = col_list), 1, quantile, probs=.025)
    cg_draws$cg_upper <- apply(subset(cg_draws, select = col_list), 1, quantile, probs=.975)
    
    #cg_draws$cg_var <- apply(subset(cg_var_draws, select = col_list), 1, mean)


    if(race_flag==0){
      temp_dt <- cg_draws[,c("cg_sum", "cg_lower", "cg_upper")]
    } else {
      temp_dt <- cg_draws[,c("RACE", "cg_sum", "cg_lower", "cg_upper")]
    }
  
    temp_dt$state <- st
    temp_dt$year <- year
    
    #####################################################
    ##### also include sample size for each group
    ###################################################
    if(dementia_flag==1){
      df <- df_state[CRGVPRB2==5,]
    }else if(dementia_flag==2){
      df <- df_state[(CRGVALZD==1) | (CRGVPRB2==5),]
    }else{
      df <- df_state[(CRGVALZD==1) & (CRGVPRB2!=5),]
    }
    
    
    # add sample size for each state
    if(race_flag==0){
      temp_dt$num_caregivers <- sum(df$WEIGHT)
      foo <- df[, .N]
      foo <- data.table(foo)
      names(foo) <- c("N")
      temp_dt <- cbind(temp_dt, foo)
    }
    else{
      foo <- df[, .(num_caregivers = sum(WEIGHT),sample_size = .N), by=list(RACE)]
      #print("foo: ")
      #print(head(foo))
      temp_dt <- merge(temp_dt, foo, by=c("RACE"))
    }
    
    #print("temp_dt: ")
    if(race_flag==1){
      setnames(temp_dt,"RACE","race")
    }
    
    setnames(temp_dt,"N","sample_size")
    
    brfss_dt <- rbind(brfss_dt, temp_dt)
  }
  
  # return a dataframe row(s) with results
  return(brfss_dt)
}
