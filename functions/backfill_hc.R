# Function to back-fill NHATS health condition data
# in later rounds, the response of 7 indicates "previously answered" 
## Author: Michael Breshock 

backfill_hc <- function(old_round, current_round, r){ # r = round number
  if(r %in% c(2:9)){
    last_r = r-1
  }else{
    return("Incorrect round number")
  }
  
  health_conditions = c(2:4, 6:8, 10)
  # 10 health conditions are asked about in the questionnaire 
  # these questions are listed in the data files under variables with 
  # the naming scheme: hc*disescn# 
  # * = NHATS round number; # = health condition number 1-10
  # The health conditions we are interested are:
  # hc*disescn2 - heart disease
  # hc*disescn3 - high blood pressure
  # hc*disescn4 - arthritis 
  # hc*disescn6 - diabetes
  # hc*disescn7 - lung disease
  # hc*disescn8 - stroke 
  # hc*disescn9 - dementia or Alzheimer's (not included here as it is processed elsewhere)
  # hc*disescn10 - cancer
  
  for(hc in health_conditions){
    old_col = paste0("hc", last_r, "disescn", hc) # last round health condition column name
    cur_col = paste0("hc", r, "disescn", hc) # current round health condition column name
    prev_report_ids = current_round[eval(parse(text = cur_col)) == 7]$spid # spid that reported 7 = "answered in previous round" 
    prev_answer = old_round[spid %in% prev_report_ids][[old_col]] # answer of those spids in last round
    current_round[eval(parse(text = cur_col)) == 7][[cur_col]] <- prev_answer # updating current round ansewrs with last round answers 
    # for those with a response of 7 in the current round
  }
  
  return(current_round)
}