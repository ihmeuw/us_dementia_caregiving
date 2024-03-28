
##########################################################################
### Author: Will Sogge
### Date: 01/20/2023
### Project: US dementia spending
### Purpose: HRS caregiving hours estimate cross-wave function
##########################################################################


rm(list=ls())


library("haven")
library("dplyr")
library("Hmisc")
library("ggpubr")
library("data.table")

# data path (will need to update later)
input_path <- FILEPATH

output_path <- FILEPATH

# set up a top-code amount so we can easily vary this
top_code <- 40
top_code_string <- as.character(top_code)

##########
#HRS Codebook: https://hrs.isr.umich.edu/documentation/codebooks
#For all waves, data pulled from core and exit "G" HELPER file.
#For all waves, demographics pulled from RAND HRS Longitudinal file https://hrsdata.isr.umich.edu/data-products/rand
#For all waves, demographics for children pulled from RAND HRS Family file https://hrsdata.isr.umich.edu/data-products/rand
#For 2016 and 2018, the most up-to-date RAND HRS Family file only gathered data up until 2016, so
#we also gathered data from the respective wave's "PR" MEMBER CHILD file and "E" MEMBER CHILD file

#Langa Weir documentation: https://hrsdata.isr.umich.edu/data-products/langa-weir-classification-cognitive-function
###########

#rand has demographic information on participants and their spouses primarily, from all waves
rand <- read_dta(paste0(input_path,"randhrs1992_2018v2.dta"))

# inserting some code here to get the sample person's age from the rand file
var_list <- names(rand)[names(rand) %like% "agey_b"]

#rand family file contains demographics about all children of respondents, but only updated to 2014
rand_family2014 <- read_dta(paste0(input_path,"randhrsfamk1992_2014v1.dta"))

rand <- rand %>% rename("HHIDPN" = "hhidpn")
rand_family2014 <- rand_family2014 %>% rename("HHIDPN" = "hhidpn")
rand_family2014 <- rand_family2014 %>% rename("KIDID" = "kidid")

rand_family2014 <- rand_family2014[c("KIDID", "kabyearbg", "kaeduc")]
rand <- rand[c("HHIDPN", "rabyear", "ragender", "raedyrs", "r14iwend", var_list)]

#remove duplicate KIDID in rand_family2014
rand_family2014 <- rand_family2014[!duplicated(rand_family2014$KIDID), ]


#create caregiving hours dataframe to export as .csv at the end
cg_hours_csv <- data.frame(cg_mean=double(), cg_sum=double(), cg_lower=double(), cg_upper=double(), year=double(), sample_size=double(), cg_var=double())
#create hours and demographic information crosswave to export
helper_crosswave <- data.frame(HHIDPN=double(), hrs_per_week=double(), core_exit_weight=double())
#helper_demo_crosswave <- data.frame(year=double(), sex=double(), age_bins=character(), educ_bins=character(), sum_weighted_hrs_per_week=double())

HRS_cg_hours <- function(year){
  
  #langa_weir file has information about each participant's cognitive status
  langa_weir <- read_dta(paste0(input_path,"LangaWeir2018/cogfinalimp_9518wide.dta"))
  #tracker file has all the weights
  tracker_file <- read_dta(paste0(input_path,"trk2020tr_r.dta"))
  
  if(year == 2018){
    #preload information containing demographics about children of respondents
    hrs_pr_child <- read_dta(paste0(FILEPATH,"USA_HRS_2018_CORE_FINAL_V1_H18PR_MC_Y2022M03D22.DTA"))
    hrs_familystructure_child <- read_dta(paste0(FILEPATH,"USA_HRS_2018_CORE_FINAL_V1_H18E_MC_Y2022M03D22.DTA"))
    #helper file contains information about how much each respondent was helped by each helper
    hrs_g_helper <- read_dta(paste0(FILEPATH,"USA_HRS_2018_CORE_FINAL_V1_H18G_HP_Y2022M03D22.DTA"))
    hrs_g_helper_exit <- read_dta(paste0(input_path,"X18G_HP.dta"))
    
    hrs_g_helper <- hrs_g_helper %>% rename("HHID" = "hhid")
    hrs_g_helper <- hrs_g_helper %>% rename("QSUBHH" = "qsubhh")
    hrs_g_helper <- hrs_g_helper %>% rename("PN" = "pn")
    hrs_g_helper <- hrs_g_helper %>% rename("OPN" = "opn")
    
    hrs_pr_child <- hrs_pr_child %>% rename("HHID" = "hhid")
    hrs_pr_child <- hrs_pr_child %>% rename("QSUBHH" = "qsubhh")
    hrs_pr_child <- hrs_pr_child %>% rename("OPN" = "opn")
    
    hrs_familystructure_child <- hrs_familystructure_child %>% rename("HHID" = "hhid")
    hrs_familystructure_child <- hrs_familystructure_child %>% rename("QSUBHH" = "qsubhh")
    hrs_familystructure_child <- hrs_familystructure_child %>% rename("OPN" = "opn")
    
    #letter for variable names:
    wave <- "Q"
    prev_wave <- "P"
    
    hrs_g_helper_exit <- hrs_g_helper_exit %>% rename("XG069" = paste0("X",wave,"G069"))
    hrs_g_helper_exit <- hrs_g_helper_exit %>% rename("XG070" = paste0("X",wave,"G070"))
    hrs_g_helper_exit <- hrs_g_helper_exit %>% rename("XG071" = paste0("X",wave,"G071"))
    hrs_g_helper_exit <- hrs_g_helper_exit %>% rename("XG072" = paste0("X",wave,"G072"))
    hrs_g_helper_exit <- hrs_g_helper_exit %>% rename("XG073" = paste0("X",wave,"G073"))
    hrs_g_helper_exit <- hrs_g_helper_exit %>% rename("XG074" = paste0("X",wave,"G074"))
  } else if(year == 2016){
    #preload information containing demographics about children of respondents
    hrs_pr_child <- read_dta(paste0(input_path,"H16PR_MC.dta"))
    hrs_familystructure_child <- read_dta(paste0(input_path,"H16E_MC.dta"))
    #helper file contains information about how much each respondent was helped by each helper
    hrs_g_helper <- read_dta(paste0(input_path,"H16G_HP.dta"))
    hrs_g_helper_exit <- read_dta(paste0(input_path,"X16G_HP.dta"))
    
    wave <- "P"
    prev_wave <- "O"
    wave_exit <- "Z"
  } else if(year == 2014){
    #no need for family preloads since rand family covers family demographics up to 2014
    
    hrs_g_helper <- read_dta(paste0(input_path,"H14G_HP.dta"))
    hrs_g_helper_exit <- read_dta(paste0(input_path,"X14G_HP.dta"))
    
    wave <- "O"
    prev_wave <- "N"
    wave_exit <- "Y"
  } else if(year==2012){
    hrs_g_helper <- read_dta(paste0(input_path,"H12G_HP.dta"))
    hrs_g_helper_exit <- read_dta(paste0(input_path,"X12G_HP.dta"))
    
    wave <- "N"
    prev_wave <- "M"
    wave_exit <- "X"
  } else if(year==2010){
    hrs_g_helper <- read_dta(paste0(input_path,"H10G_HP.dta"))
    hrs_g_helper_exit <- read_dta(paste0(input_path,"X10G_HP.dta"))
    
    wave <- "M"
    prev_wave <- "L"
    wave_exit <- "W"
  } else{
    return("Not a year included in this analysis")
  }

  #remame variables without letter in front for each wave
  hrs_g_helper <- hrs_g_helper %>% rename("SUBHH" = paste0(wave,"SUBHH"))
  hrs_g_helper <- hrs_g_helper %>% rename("G069" = paste0(wave,"G069"))
  hrs_g_helper <- hrs_g_helper %>% rename("G070" = paste0(wave,"G070"))
  hrs_g_helper <- hrs_g_helper %>% rename("G071" = paste0(wave,"G071"))
  hrs_g_helper <- hrs_g_helper %>% rename("G072" = paste0(wave,"G072"))
  hrs_g_helper <- hrs_g_helper %>% rename("G073" = paste0(wave,"G073"))
  hrs_g_helper <- hrs_g_helper %>% rename("G074" = paste0(wave,"G074"))
  
  if(year!=2018){
    hrs_g_helper_exit <- hrs_g_helper_exit %>% rename("XG069" = paste0(wave_exit,"G069"))
    hrs_g_helper_exit <- hrs_g_helper_exit %>% rename("XG070" = paste0(wave_exit,"G070"))
    hrs_g_helper_exit <- hrs_g_helper_exit %>% rename("XG071" = paste0(wave_exit,"G071"))
    hrs_g_helper_exit <- hrs_g_helper_exit %>% rename("XG072" = paste0(wave_exit,"G072"))
    hrs_g_helper_exit <- hrs_g_helper_exit %>% rename("XG073" = paste0(wave_exit,"G073"))
    hrs_g_helper_exit <- hrs_g_helper_exit %>% rename("XG074" = paste0(wave_exit,"G074"))
  }
  
  tracker_file <- tracker_file %>% rename("WHY0RWT" = paste0(wave,"WHY0RWT"))
  tracker_file <- tracker_file %>% rename("WGTR" = paste0(wave,"WGTR"))
  tracker_file <- tracker_file %>% rename("WGTRNH" = paste0(wave,"WGTRNH"))
  tracker_file <- tracker_file %>% rename("prev_WGTRNH" = paste0(prev_wave,"WGTRNH"))
  tracker_file <- tracker_file %>% rename("prev_WGTR" = paste0(prev_wave,"WGTR"))
  
  # unique ids, a combination of household id and personal number
  langa_weir$HHIDPN <- (as.double(langa_weir$hhid) * 1000) + as.double(langa_weir$pn)
  hrs_g_helper$HHIDPN <- (as.double(hrs_g_helper$HHID) * 1000) + as.double(hrs_g_helper$PN)
  hrs_g_helper_exit$HHIDPN <- (as.double(hrs_g_helper_exit$HHID) * 1000) + as.double(hrs_g_helper_exit$PN)
  tracker_file$HHIDPN <- (as.double(tracker_file$HHID) * 1000) + as.double(tracker_file$PN)
  #RAND establishes a KIDID, which is different from a HHIDPN since it includes a SUBHH number, which is
  #applicable if a child comes from two households due to divorce or other circumstances
  if(year==2018 | year==2016){
    varSUBHH <- paste0(wave, "SUBHH")
    hrs_pr_child$KIDID <- paste(hrs_pr_child$HHID, hrs_pr_child[[varSUBHH]], hrs_pr_child$OPN, sep="")
    hrs_familystructure_child$KIDID <- paste(hrs_familystructure_child$HHID, hrs_familystructure_child[[varSUBHH]], hrs_familystructure_child$OPN, sep="")
  }
  
  #merge helper core and exit files
  #hrs_g_helper_exit <- hrs_g_helper_exit %>% rename("XOPN" = "OPN")
  hrs_g_helper <- merge(hrs_g_helper, hrs_g_helper_exit, by=c("HHIDPN", "OPN", "HHID", "PN"), all = TRUE)
  
  #get information from core and exit file into same variable
  hrs_g_helper <- hrs_g_helper %>%
    mutate(G071 = coalesce(G071, XG071))
  hrs_g_helper <- hrs_g_helper %>%
    mutate(G070 = coalesce(G070, XG070))
  hrs_g_helper <- hrs_g_helper %>%
    mutate(G072 = coalesce(G072, XG072))
  hrs_g_helper <- hrs_g_helper %>%
    mutate(G073 = coalesce(G073, XG073))
  hrs_g_helper <- hrs_g_helper %>%
    mutate(G069 = coalesce(G069, XG069))
  hrs_g_helper <- hrs_g_helper %>%
    mutate(G074 = coalesce(G074, XG074))
  
  #lists TRUE if demented under current wave langa-weir scale, NA if not
  langa_weir <- langa_weir %>% rename("cogtot27_imp_now" = paste0("cogtot27_imp",year))
  langa_weir <- langa_weir %>% rename("prxyscore_imp_now" = paste0("prxyscore_imp",year))
  langa_weir$demented_now <- ifelse(
    (
      (langa_weir$cogtot27_imp_now < 7) | (langa_weir$prxyscore_imp_now > 5)
    ),
    TRUE,
    NA
  )
  
  #lists TRUE if demented under prev wave langa-weir scale, NA if not
  langa_weir <- langa_weir %>% rename("cogtot27_imp_prev" = paste0("cogtot27_imp",(year - 2)))
  langa_weir <- langa_weir %>% rename("prxyscore_imp_prev" = paste0("prxyscore_imp",(year - 2)))
  langa_weir$demented_prev <- ifelse(
    (
      (langa_weir$cogtot27_imp_prev < 7) | (langa_weir$prxyscore_imp_prev > 5)
    ),
    TRUE,
    NA
  )
  
  #for 2018 participants, list as demented if they were considered demented in 2018 or 2016 (including 2016 demented status takes into account exit interview participants)
  langa_weir <- langa_weir %>%
    mutate(demented_now = coalesce(demented_now, demented_prev))
  
  #keep only variables needed from each dataset individually:
  tracker_file <- tracker_file[c("HHIDPN", "BIRTHYR", "WGTR", "WGTRNH", "WHY0RWT", "prev_WGTR", "prev_WGTRNH")]
  langa_weir <- langa_weir[c("HHIDPN", "demented_now")]
  hrs_g_helper <- hrs_g_helper[c("HHIDPN", "HHID", "G070", "G071", "G072", "G073", "OPN", "SUBHH", "G069", "G074")]
  
  
  #keep only rows with valid current wave participants from tracker file
  #(this step keeps only people who have a weight or who don't have a weight because they are dead)
  tracker_file <- tracker_file[tracker_file$WHY0RWT == 0 | tracker_file$WHY0RWT == 1 | tracker_file$WHY0RWT == 2, ]
  #turn NA in nursing home weight to 0
  tracker_file$WGTRNH[is.na(tracker_file$WGTRNH)] <- 0
  tracker_file$prev_WGTRNH[is.na(tracker_file$prev_WGTRNH)] <- 0
  #create a total weight, combining respondent weight and nursing home weight (using RAND methodology, since they are mutually exclusive so one will always be 0)
  tracker_file$totweightalive_now <- tracker_file$WGTR + tracker_file$WGTRNH
  tracker_file$totweightalive_prev <- tracker_file$prev_WGTR + tracker_file$prev_WGTRNH
  #remove participants in tracking file who are deceased and still have 0 weight (thus, people who died before 2016 interview)
  tracker_file <- tracker_file[tracker_file$WHY0RWT == 0 | (tracker_file$WHY0RWT == 1 & tracker_file$totweightalive_prev > 0) | tracker_file$WHY0RWT == 2, ]
  #combine core weight and exit weights (which are pulled from previous wave) into one final total weight
  tracker_file <- tracker_file %>%
    mutate(core_exit_weight = case_when(
      WHY0RWT == 1 ~ totweightalive_prev,
      TRUE ~ totweightalive_now
    ))
  
  
  #merge langaweir and tracker
  # the variable 'demented_now' is NA unless the person has dementia, so this is 
  #               subsetting to only those w/ dementia per Langa-Weir classifier
  langa_weir <- langa_weir[!is.na(langa_weir$demented_now), ]
  all <- merge(tracker_file, langa_weir, by="HHIDPN")
  
  
  #remove rows from helper dataset with 0 hours helped or with NA for all helper questions
  hrs_g_helper <- subset(hrs_g_helper, G070 !=0 | is.na(G070))
  hrs_g_helper <- subset(hrs_g_helper, G073 !=0 | is.na(G073))
  hrs_g_helper <- hrs_g_helper[!with(hrs_g_helper,is.na(G070) & is.na(G071) & is.na(G072) & is.na(G073)),]
  
  
  #merge helper dataset with tracker for weights and to only keep those with dementia
  hrs_g_helper <- merge(hrs_g_helper, all, by="HHIDPN")
  
  
  #for caregiving hours and frequency, replace 'don't know' or blank with NA
  hrs_g_helper$G070[which(hrs_g_helper$G070 == "99" | hrs_g_helper$G070 == "98" | hrs_g_helper$G070 == "-8")] <- NA
  hrs_g_helper$G071[which(hrs_g_helper$G071=="8")] <- NA
  hrs_g_helper$G072[which(hrs_g_helper$G072=="8" | hrs_g_helper$G072=="9")] <- NA
  hrs_g_helper$G073[which(hrs_g_helper$G073 == "99" | hrs_g_helper$G073 == "98" | hrs_g_helper$G073 == "-8")] <- NA
  
  
  #for caregiving hours, replace NA with average value
  avg_cg_hours_per_day_per_helper <- weighted.mean(hrs_g_helper$G073, hrs_g_helper$core_exit_weight, na.rm=TRUE)
  hrs_g_helper$G073 <- as.double(hrs_g_helper$G073)
  hrs_g_helper$G073[is.na(hrs_g_helper$G073)] <- avg_cg_hours_per_day_per_helper
  
  
  #replace NA with average caregiving frequency
  #first, convert all frequencies into week format in G070 and G072
  #G070 is coded as how many days you were helped per month
  #G072 is coded as 1 if a participant said they were helped every day
  hrs_g_helper$G070 <- hrs_g_helper$G070 * (7/30.437)
  hrs_g_helper$G070[hrs_g_helper$G070 > 7] <- 7
  hrs_g_helper$G072 <- hrs_g_helper$G072 * 7
  #then, bring data over from QG070 or QG072 when QG071 is NA
  hrs_g_helper <- hrs_g_helper %>%
    mutate(G071 = coalesce(G071, G070))
  hrs_g_helper <- hrs_g_helper %>%
    mutate(G071 = coalesce(G071, G072))
  #last, replace NA with average caregiving frequency
  avg_cg_freq_per_week_per_helper <- weighted.mean(hrs_g_helper$G071, hrs_g_helper$core_exit_weight, na.rm=TRUE)
  hrs_g_helper$G071 <- as.double(hrs_g_helper$G071)
  hrs_g_helper$G071[is.na(hrs_g_helper$G071)] <- avg_cg_freq_per_week_per_helper
  
  
  #calculate total hours per week per helper
  hrs_g_helper$hrs_per_week <- hrs_g_helper$G071 * hrs_g_helper$G073
  
  
  #recode to 168 hours per week if over since that is 24/7
  hrs_g_helper$hrs_per_week[which(hrs_g_helper$hrs_per_week > top_code)] <- top_code
  
  helper_crosswave_prep <- hrs_g_helper[c("HHIDPN", "hrs_per_week", "core_exit_weight")]
  helper_crosswave <<- rbind(helper_crosswave_prep, helper_crosswave)
  
  
  #create new dataframe with total hours of help per HHIDPN (so if a respondent had multiple helpers, the participant now has one total number)
  cg_hours_per_person <<- aggregate(hrs_g_helper$hrs_per_week, by=list(HHIDPN=hrs_g_helper$HHIDPN), FUN=sum)
  colnames(cg_hours_per_person)[2] = "total_hrs"
  
  
  #merge cg hours per person with all demented respondents (now, even those with no helpers)
  all <- merge(all, cg_hours_per_person, by="HHIDPN", all.x = TRUE)
  
  
  #make people with dementia who had no helpers reported have 0 caregiving hours
  all$total_hrs[is.na(all$total_hrs)] <- 0
  
  
  #variables of interest:
  all <- all[c("HHIDPN", "total_hrs", "BIRTHYR", "core_exit_weight")]
  
  # add rand file so we can get age of SP
  all <- merge(all, rand, by="HHIDPN", all.x = TRUE)
  
  # cut anyone who is under 55 
  all <- data.table(all)
  all[, current_age := case_when(year==2010 ~ r10agey_b,
                                 year==2012 ~ r11agey_b,
                                 year==2014 ~ r12agey_b,
                                 year==2016 ~ r13agey_b,
                                 year==2018 ~ r14agey_b)]
  
  print(summary(all$current_age))
  print(nrow(all))
  
  all <- all[current_age >=55,]
  print(nrow(all))
  
  # create just a dataframe with the hhid and age to use later
  age_df <- all[, .(current_age, HHIDPN_respondent = HHIDPN)]
  
  #weighted mean and weighted standard deviation of caregiving hours per prevalent dementia case
  mean_cg_hrs <<- weighted.mean(all$total_hrs, all$core_exit_weight)
  sum_cg_hrs <- sum(all$total_hrs * all$core_exit_weight)
  var_cg_hrs <<- wtd.var(all$total_hrs, all$core_exit_weight)
  sd_cg_hrs <<- sqrt(var_cg_hrs)
  sample_size_cg_hrs <- as.double(nrow(all))
  standard_error_cg_hrs <<- sd_cg_hrs/sqrt(sample_size_cg_hrs)
  margin <- qt(.975, df=(sample_size_cg_hrs - 1))*standard_error_cg_hrs
  lower_cg_hrs <- mean_cg_hrs - margin
  upper_cg_hrs <- mean_cg_hrs + margin
  
  cg_hours_csv[nrow(cg_hours_csv) + 1,] = list(cg_mean = mean_cg_hrs, cg_sum = sum_cg_hrs, cg_lower = lower_cg_hrs, cg_upper = upper_cg_hrs, year=year, sample_size = sample_size_cg_hrs, cg_var = var_cg_hrs)
  cg_hours_csv <<- cg_hours_csv
  
  ##-----------------------------------------------------------------------------------------------------------------
  #now, moving on to helper demographics and no longer working with the respondent-level questions
  
  
  #create unique ID for helpers - HHIDPN is now based on the OPN not the original sample person 
  hrs_g_helper <- hrs_g_helper %>% rename("HHIDPN_respondent" = "HHIDPN")
  hrs_g_helper$HHIDPN <- (as.double(hrs_g_helper$HHID) * 1000) + as.double(hrs_g_helper$OPN)
  
  #demographics of helpers:
  #first, create a subset of the helpers that are spouses:
  hrs_g_helper_spouses <- subset(hrs_g_helper, G069 == 2)
  #merge spouse helpers with RAND demographic info:
  spouse_helpers_and_rand <- merge(rand, hrs_g_helper_spouses, by="HHIDPN")
  
  # create current age so we can cut anyone who is under 55 
  #spouse_helpers_and_rand <- data.table(spouse_helpers_and_rand)
  spouse_helpers_and_rand <- spouse_helpers_and_rand %>% 
      mutate(current_age = case_when(year==2010 ~ r10agey_b,
                                 year==2012 ~ r11agey_b,
                                 year==2014 ~ r12agey_b,
                                 year==2016 ~ r13agey_b,
                                 year==2018 ~ r14agey_b))
  print(summary(spouse_helpers_and_rand$current_age))
  
  #demographics of children:
  
  if (year==2018){
    hrs_pr_child <- hrs_pr_child %>% rename("X067_MC" = "QX067_MC")
    hrs_familystructure_child <- hrs_familystructure_child %>% rename("E029" = "QE029")
    hrs_pr_child <- hrs_pr_child[c("KIDID", "X067_MC")]
    hrs_familystructure_child <- hrs_familystructure_child[c("KIDID", "E029")]
  } else if (year==2016){
    hrs_pr_child <- hrs_pr_child %>% rename("X067_MC" = "PX067_MC")
    hrs_familystructure_child <- hrs_familystructure_child %>% rename("E029" = "PE029")
    hrs_pr_child <- hrs_pr_child[c("KIDID", "X067_MC")]
    hrs_familystructure_child <- hrs_familystructure_child[c("KIDID", "E029")]
  }
  
  #create a subset of helpers that are children (G069 asks what the helper's relationship is to respondent):
  hrs_g_helper_children <- subset(hrs_g_helper, G069 == 3 | G069 == 4 | G069 == 6 | G069 == 7)
  hrs_g_helper_children$KIDID <- paste(hrs_g_helper_children$HHID, hrs_g_helper_children$SUBHH, hrs_g_helper_children$OPN, sep="")
  
  if (year==2016 | year==2018){
    #merge child family structure and child preload
    child_demographics <- merge(hrs_pr_child, hrs_familystructure_child, by="KIDID", all.x=TRUE)
    
    #merge child helpers with demographic info:
    child_demographics <- merge(hrs_g_helper_children, child_demographics)
    #de-duplicate child_demographics
    child_demographics <- child_demographics[!duplicated(child_demographics$KIDID), ]
    #merge rand and wave 2018 demographics for children all together
    child_helpers_demographics <- merge(hrs_g_helper_children, rand_family2014, by="KIDID")
    child_helpers_demographics <- merge(child_demographics, child_helpers_demographics, by="KIDID", all.x=TRUE)
    
    child_helpers_demographics <- child_helpers_demographics %>% rename("HHIDPN" = "HHIDPN.x")
    child_helpers_demographics <- child_helpers_demographics %>% rename("G074" = "G074.x")
    child_helpers_demographics <- child_helpers_demographics %>% rename("hrs_per_week" = "hrs_per_week.x")
  } else {
    child_helpers_demographics <- merge(hrs_g_helper_children, rand_family2014, by="KIDID")
  }
  
  #clean up child demographics
  #keep only relevant variables:
  if(year==2018 | year==2016){
    child_helpers_demographics <- child_helpers_demographics[c("KIDID", "HHIDPN", "kaeduc", "E029", "kabyearbg", "X067_MC", "G074", "hrs_per_week")]
    #pull education from hrs wave 2018 file if not included from rand data
    child_helpers_demographics <- child_helpers_demographics %>%
      mutate(kaeduc = coalesce(kaeduc, E029))
    child_helpers_demographics <- child_helpers_demographics %>%
      mutate(kabyearbg = coalesce(kabyearbg, X067_MC))
  } else {
    child_helpers_demographics <- child_helpers_demographics[c("KIDID", "HHIDPN", "kaeduc", "kabyearbg", "G074", "hrs_per_week")]
  }
  child_helpers_demographics$kaeduc <- as.double(child_helpers_demographics$kaeduc)
  
  #clean up spouse demographics
  spouse_helpers_and_rand <- spouse_helpers_and_rand %>%
    mutate(G074 = coalesce(ragender, G074))
  spouse_helpers_and_rand$raedyrs <- as.double(spouse_helpers_and_rand$raedyrs)
  
  
  #compile all helper demographic info:
  #first, keep only relevant variables
  child_helpers_demographics <- child_helpers_demographics[c("KIDID", "HHIDPN", "kabyearbg", "kaeduc")]
  #spouse_helpers_and_rand <- spouse_helpers_and_rand[c("HHIDPN", "rabyear", "raedyrs", "ragender")]
  spouse_helpers_and_rand <- spouse_helpers_and_rand[c("HHIDPN", "rabyear", "raedyrs", "ragender", "current_age",var_list)]
  hrs_g_helper <- hrs_g_helper[c("HHIDPN", "G069", "G074", "hrs_per_week", "core_exit_weight","HHIDPN_respondent")]
  #then, deduplicate the demographics before merging
  child_helpers_demographics <- child_helpers_demographics[!duplicated(child_helpers_demographics$HHIDPN), ]
  spouse_helpers_and_rand <- spouse_helpers_and_rand[!duplicated(spouse_helpers_and_rand$HHIDPN), ]

  
  #merge helper file with hours, weights, etc. with the demographic files
  all_helper_demographics <- merge(hrs_g_helper, child_helpers_demographics, by="HHIDPN", all.x=TRUE) 
  all_helper_demographics <- merge(all_helper_demographics, spouse_helpers_and_rand, by="HHIDPN", all.x=TRUE)
  print("column names of hrs_g_helper")
  print(colnames(hrs_g_helper))
  print("column names of all_helper_demographics")
  print(colnames(all_helper_demographics))
  
  print(head(age_df))
  print(paste0("YEAR = ",year))
  
  # all_helper demographics already has current_age, remove it
  all_helper_demographics <- all_helper_demographics %>% dplyr::select(-current_age)
  
  all_helper_demographics <- merge(all_helper_demographics, age_df, by = "HHIDPN_respondent")
  
  #clean up helper demographics after merge by combining variables from different sources into one variable
  all_helper_demographics <- all_helper_demographics %>%
    mutate(edu_years = coalesce(raedyrs, kaeduc))
  all_helper_demographics <- all_helper_demographics %>%
    mutate(year_born = coalesce(rabyear, kabyearbg))
  all_helper_demographics <- all_helper_demographics %>%
    mutate(sex = coalesce(ragender, G074))
  
  #create age buckets
  all_helper_demographics <- all_helper_demographics %>%
    mutate(age_bins = case_when(
      year_born > (year - 18) ~ "-8", #because we do not have wage data for people under 18
      year_born <= (year - 18) & year_born > (year - 35) ~ "18-34",
      year_born <= (year - 35) & year_born > (year - 45) ~ "35-44",
      year_born <= (year - 45) & year_born > (year - 55) ~ "45-54",
      year_born <= (year - 55) & year_born > (year - 65) ~ "55-64",
      year_born <= (year - 65) & year_born > (year - 75) ~ "65-74",
      year_born <= (year - 45) & year_born > 1900 ~ "75+",
      TRUE ~ "-8"
    ))
  
  #create education buckets
  all_helper_demographics <- all_helper_demographics %>%
    mutate(educ_bins = case_when(
      edu_years <= 12 ~ "High school or less",
      edu_years > 12 & edu_years < 16 ~ "Some college",
      edu_years >= 16 ~ "Bachelor's degree or more",
      TRUE ~ "-8"
    ))
  
  #remove helpers who we do not have gender information on
  all_helper_demographics <<- subset(all_helper_demographics, sex == 1 | sex == 2)
  all_helper_demographics <- data.table(all_helper_demographics)
  
  # remove SP who are under 55
  print("columns of all_helper_demographics")
  print(colnames(all_helper_demographics))
  all_helper_demographics <- all_helper_demographics[current_age >=55,]
  print(nrow(all_helper_demographics))
  
  #multiply hrs per week by sample weight
  all_helper_demographics$weighted_hrs_per_week <- all_helper_demographics$hrs_per_week * all_helper_demographics$core_exit_weight
  
  helper_demographics_by_bucket <- aggregate(all_helper_demographics$weighted_hrs_per_week,
                                             by=list(educ_bins = all_helper_demographics$educ_bins, age_bins = all_helper_demographics$age_bins, sex = all_helper_demographics$sex),
                                             FUN=sum)
  helper_demographics_by_bucket <- helper_demographics_by_bucket %>% rename("sum_weighted_hrs_per_week" = "x")
  helper_demographics_by_bucket$year <- year
  
  #helper_demo_crosswave <<- rbind(helper_demo_crosswave, helper_demographics_by_bucket)
  
  return(all_helper_demographics)
}

demographics2010 <- HRS_cg_hours(2010)
demographics2012 <- HRS_cg_hours(2012)
demographics2014 <- HRS_cg_hours(2014)
demographics2016 <- HRS_cg_hours(2016)
demographics2018 <- HRS_cg_hours(2018)

# bind all demographic data together
HRS_demographics = rbind(demographics2010, demographics2012, demographics2014, 
                         demographics2016, demographics2018)
# get percent of helpers that are spouses: 
nrow(HRS_demographics[G069 == 2]) / nrow(HRS_demographics) # 0.1525697

# testing some code here
test_dt <- demographics2010[!is.na(r10agey_b),]
ggplot(test_dt, aes(x=r10agey_b,y=hrs_per_week)) +
  geom_point() +
  geom_smooth(method = "lm") +
  stat_cor(aes(label = ..p.label..), p.digits = 3,label.x.npc="left", label.y.npc="bottom") +
  ggtitle("cg hrs vs age")

# end test

write.csv(cg_hours_csv, paste0(output_path,"HRS_caregiving_hours_2010_2018_55up_",top_code_string,".csv"), row.names = FALSE)
hrs_per_week <- helper_crosswave$hrs_per_week
hist(hrs_per_week)

# ------------------ working on boostrapping here

cps_wages = fread(file = paste0(FILEPATH,"wages_for_HRS_2010_2018.csv"))


cost_bootstrap <- function(cg_datatable, ndraws=100){
  cost_estimate_list <- list()
  cg_datable <- data.table(cg_datatable)
  for(i in 1:ndraws){
    ids <- cg_datatable[, unique(HHIDPN)]
    sample_list <- data.table(HHIDPN = sample(ids, replace = T))
    cg_resampled <- merge(sample_list, cg_datatable, by = "HHIDPN", all.x = T, sort = F, allow.cartesian = T)
    cg_resampled <- data.table(cg_resampled)
    #print(head(cg_resampled))
    # need year and state to merge with CPS data
    #cg_temp <- cg_resampled[, .(cg_sum = sum(weighted_hrs_per_week), year = mean(year), draw = i), by=c("educ_bins","age_bins","sex")]
    
    cg_resampled[, cg_cost := weighted_hrs_per_week*nat_wage_mean*nat_LFP]
    
    # sum up cost over all demographic groups 
    cg_temp <- cg_resampled[,.(total_cost=sum(cg_cost,na.rm=T),total_hours=sum(weighted_hrs_per_week,na.rm=T)), by=c("year")]
    cg_temp[, avg_cost_per_hour:= total_cost/total_hours]
    
    # append the avg_cost_per_hour to the end of cost estimate list
    cost_estimate_list <- c(cost_estimate_list, cg_temp$avg_cost_per_hour)
    
  }
  cost_estimates <- unlist(cost_estimate_list)
  bootstrap_mean <- mean(cost_estimates)
  bootstrap_lower <- quantile(cost_estimates, probs = c(0.025))
  bootstrap_upper <- quantile(cost_estimates, probs = c(0.975))
  bootstrap_var <- var(cost_estimates)
  
  
  #put mean/upper/lower into a dataframe and then return it
  cost_output <- data.table(cost_mean = bootstrap_mean, cost_lower = bootstrap_lower, cost_upper = bootstrap_upper, cost_var = bootstrap_var, year = mean(cg_resampled$year), sample_size = nrow(cg_datatable) )
  return(cost_output)
}

demo_list <- c("demographics2010","demographics2012","demographics2014","demographics2016","demographics2018")

demographics2010$year <- 2010
temp_df <- merge(demographics2010, cps_wages, by=c("year", "sex", "age_bins", "educ_bins"), all.x=TRUE)
cost_out_2010 <- cost_bootstrap(temp_df)

demographics2012$year <- 2012
temp_df <- merge(demographics2012, cps_wages, by=c("year", "sex", "age_bins", "educ_bins"), all.x=TRUE)
cost_out_2012 <- cost_bootstrap(temp_df)

demographics2014$year <- 2014
temp_df <- merge(demographics2014, cps_wages, by=c("year", "sex", "age_bins", "educ_bins"), all.x=TRUE)
cost_out_2014 <- cost_bootstrap(temp_df)

demographics2016$year <- 2016
temp_df <- merge(demographics2016, cps_wages, by=c("year", "sex", "age_bins", "educ_bins"), all.x=TRUE)
cost_out_2016 <- cost_bootstrap(temp_df)

demographics2018$year <- 2018
temp_df <- merge(demographics2018, cps_wages, by=c("year", "sex", "age_bins", "educ_bins"), all.x=TRUE)
cost_out_2018 <- cost_bootstrap(temp_df)

cost_out <- rbind(cost_out_2010, cost_out_2012, cost_out_2014, cost_out_2016, cost_out_2018)
write.csv(cost_out, paste0(output_path,"HRS_cost_estimate_2010_2018_55up_",top_code_string,".csv"), row.names = FALSE)

# ------------------ end of boostrapping section

