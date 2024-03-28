# Function to process and extract caregiving hours from raw NHATS OP data
# 01/12/2023
# Author: Michael Breshock

process_NHATS_OP <- function(df, round_num){
helper = paste0("op",round_num,"paidhelpr")
rel = paste0("op",round_num,"relatnshp")
# create new paid helper column to fill missing values
# if no response and relationship to SP = 92 Other Nonrelative
# assume paid helper. Otherwise, assume unpaid
df[, op_is_paid := case_when(eval(parse(text = helper)) == 2 ~ 0, 
                              eval(parse(text = helper)) == 1 ~ 1, 
                              eval(parse(text = helper)) < 0 & eval(parse(text = rel)) == 92 ~ 1, # Other Nonrelative
                              TRUE ~ 0)]

# filter to unpaid helpers
df_unpaid = df[op_is_paid == 0]

# change column names of interest - op#dhrsmth = derived monthly hours variable
# the derived monthly hours variable coalesces all the hrs / day and hrs/mth info into a single variable (so we dont have to!)
setnames(df_unpaid, paste0("op",round_num,"dhrsmth"), "monthly_hours")

# filter to helpers that reported care hours 
unpaid_hours = df_unpaid[monthly_hours >= 0 & monthly_hours < 9999] # 9999 = not codeable (<1 hour/month)
# create weekly hour variable by multiplying by days per week over days per month
unpaid_hours[, weekly_hours := monthly_hours * 7/31] # using 31 as this was the max days per month used in NHATS survey

df_summary = data.table(unpaid_hours %>% group_by(spid, opid) %>%
                          summarise(care_hours_week = sum(weekly_hours))
                          )
df_summary[, year := 2010 + round_num]
return(df_summary[, .(year, spid, opid, care_hours_week)])
}

