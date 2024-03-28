# function to create dementia classification variable in NHATS public data
# this function assumes the data has been preprocessed so that the variables used 
# below have the round prefix removed from the column name
## Author: Michael Breshock 


classify_dementia_NHATS <- function(df, round_num){
  
  # unifying variable names across different rounds
  old_names = c(paste0("cp",round_num,"chgthink1"), paste0("cp",round_num,"chgthink2"),
               paste0("cp",round_num,"chgthink3"), paste0("cp",round_num,"chgthink4"),
               paste0("cp",round_num,"chgthink5"), paste0("cp",round_num,"chgthink6"),
               paste0("cp",round_num,"chgthink7"), paste0("cp",round_num,"chgthink8"),
               paste0("cp",round_num,"dad8dem"), paste0("cg",round_num,"dwrddlyrc"),
               paste0("cg",round_num,"dwrdimmrc"), paste0("cg",round_num,"todaydat1"),
               paste0("cg",round_num,"todaydat2"), paste0("cg",round_num,"todaydat3"),
               paste0("cg",round_num,"todaydat4"), paste0("cg",round_num,"todaydat5"),
               paste0("cg",round_num,"todaydat6"), paste0("cg",round_num,"presidna1"),
               paste0("cg",round_num,"presidna3"), paste0("cg",round_num,"vpname1"),
               paste0("cg",round_num,"vpname3"), paste0("cg",round_num,"dclkdraw"),
               paste0("hc",round_num,"disescn9"), paste0("cg",round_num,"reascano1"),
               paste0("is",round_num,"reasnprx1"))
  old_res = paste0("r",round_num,"dresid") # getting residence / deceased status 
  old_weights = grep("anfinwgt", names(df), value = T) # getting all weight variables
  old_vars = c(old_names, old_res, old_weights)
  
  new_res = "residence"
  new_names = sub(".{3}","", old_names) # removing subscripts from all variables
  new_weights = sub(".{2}","", old_weights) # weight subscript is shorter than the others, .{2} instead of .{3} here
  new_vars = c(new_names, new_res, new_weights)
  
  setnames(df, old = old_vars, new = new_vars, skip_absent = T)
  
  # recoding AD8 question varaibles to 1 = yes, 0 = no
  spAD8 = data.table(mutate(df, 
                 AD8chgthink1 = case_when(chgthink1 == 2 ~ 0, TRUE ~ chgthink1),
                 AD8chgthink2 = case_when(chgthink2 == 2 ~ 0, TRUE ~ chgthink2),
                 AD8chgthink3 = case_when(chgthink3 == 2 ~ 0, TRUE ~ chgthink3),
                 AD8chgthink4 = case_when(chgthink4 == 2 ~ 0, TRUE ~ chgthink4),
                 AD8chgthink5 = case_when(chgthink5 == 2 ~ 0, TRUE ~ chgthink5),
                 AD8chgthink6 = case_when(chgthink6 == 2 ~ 0, TRUE ~ chgthink6),
                 AD8chgthink7 = case_when(chgthink7 == 2 ~ 0, TRUE ~ chgthink7),
                 AD8chgthink8 = case_when(chgthink8 == 2 ~ 0, TRUE ~ chgthink8)))
  
  # creating AD8 score column
  spAD8[, AD8 := AD8chgthink1 + AD8chgthink2 + AD8chgthink3 + AD8chgthink4 + 
          AD8chgthink5 + AD8chgthink6 + AD8chgthink7 + AD8chgthink8]
  
  # accounting for responses other than "yes" or "no"
  if(round_num == 1){
    spAD8[, AD8 := case_when(AD8 < 0 ~ -1,
                             AD8 > 8 ~ 8, 
                             chgthink1 == 3 ~ 8, 
                             chgthink2 == 3 ~ 8,
                             chgthink3 == 3 ~ 8,
                             chgthink4 == 3 ~ 8,
                             chgthink5 == 3 ~ 8,
                             chgthink6 == 3 ~ 8,
                             chgthink7 == 3 ~ 8,
                             chgthink8 == 3 ~ 8,
                             TRUE ~ AD8)]
  } else{
    spAD8[, AD8 := case_when(AD8 < 0 ~ -1,
                             AD8 > 8 ~ 8, 
                             dad8dem == 1 ~ 8,
                             chgthink1 == 3 ~ 8, 
                             chgthink2 == 3 ~ 8,
                             chgthink3 == 3 ~ 8,
                             chgthink4 == 3 ~ 8,
                             chgthink5 == 3 ~ 8,
                             chgthink6 == 3 ~ 8,
                             chgthink7 == 3 ~ 8,
                             chgthink8 == 3 ~ 8,
                             TRUE ~ AD8)]
  }
  
  
  # calculating coginition module scores (memory, orientation, and executive function)
  sp_dem = spAD8[, ':=' (memory = dwrddlyrc + dwrdimmrc,
                         orientation = (todaydat1 == 1) + (todaydat2 == 1) +
                           (todaydat3 == 1) + (todaydat4 == 1) + 
                           (presidna1 == 1) + (presidna3 == 1) + 
                           (vpname1 == 1) + (vpname3 == 1),
                         exec_func = dclkdraw)]
  
  # calculating 1.5 SDs less than mean cut offs for cognition domains 
  # (if an SP is less than 1.5 SDs away from the mean in 2 or more domains, 
  # we categorize them as having dementia)
  memory_mn = mean(sp_dem[memory >= 0]$memory)
  memory_sd = sd(sp_dem[memory >= 0]$memory)
  memory_cut = memory_mn - 1.5*memory_sd
  
  if(round_num == 1){
    orient_mn = mean(sp_dem[orientation >= 0 & todaydat5 != 1]$orientation)
    orient_sd = sd(sp_dem[orientation >= 0 & todaydat5 != 1]$orientation)
  } else{
    orient_mn = mean(sp_dem[orientation >= 0 & todaydat5 != 1 & todaydat6 != 1]$orientation)
    orient_sd = sd(sp_dem[orientation >= 0 & todaydat5 != 1 & todaydat6 != 1]$orientation)
  }
  
  orient_cut = orient_mn - 1.5*orient_sd
  
  exec_mn = mean(sp_dem[exec_func >= 0]$exec_func)
  exec_sd = sd(sp_dem[exec_func >= 0]$exec_func)
  exec_cut = exec_mn - 1.5*exec_sd
  
  cutoffs = data.table(memory = round(memory_cut),
                       orientation = round(orient_cut),
                       executive_function = round(exec_cut))
  
  sp_dem[, cognition_cutoffs := (memory <= cutoffs$memory) + (orientation <= cutoffs$orientation) + 
           (exec_func <= cutoffs$executive_function)]
  
  sp_dem[, dementia := case_when(disescn9 == 1 ~ 1, # has dementia
                                 reascano1 == 1 ~ 1,  # cannot answer question b/c has dementia
                                 reasnprx1 == 1 ~ 1, # needs a proxy b/c has dementia
                                 AD8 >= 2 ~ 1, # AD8 score more than or equal to 2
                                 cognition_cutoffs >= 2 ~ 1, # 2 or more cognition module scores below cutoffs
                                 TRUE ~ 0)]
  return(sp_dem)
}