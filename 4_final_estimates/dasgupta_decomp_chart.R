## Plot DasGupta Decomposition Results
## GV Dementia Update March 2023 
## Author: Michael Breshock
## Date: 03/03/2023

# clear environment
rm(list = ls())

# load libraries
library(ggplot2)
library(dplyr)
library(data.table)
library(tidyr)
library(viridis)


# flag to indicate whether we want a subset of states or all states
subset_flag <- 0 # 0 means do not subset - keep all states

model <- "OC"

# load in decomp results
if(model=="RC"){
  filename <-"das_gupta_RC_population_112_2023_10_14.csv"
  plot_title <- "2019 Replacement cost per capita"
  baseline_state <- "Utah"
}else if(model=="OC"){
  filename <-"das_gupta_OC_population_40_2023_10_21.csv"
  plot_title <- "2019 Forgone wage cost per capita"
  baseline_state <- "Alaska"
}


dasgupta = fread(paste0(j_root,"Project/IRH/Informal_Care_AD/US_dementia_spending_2022/output/das_gupta/",filename))

# filter out USA data (dont want to plot this)
states = dasgupta[State != "USA"]
# filter to just 15 states to plot
ordered_states = states[order(states$per_capita_cost, decreasing = T)]
if(subset_flag==1){
  states = ordered_states[, head(.SD, 20)]
}else{
  states = ordered_states
}
# melt to long format for plotting reasons
state_melt = melt(states, id.vars = c("State", "total_cost", "per_capita_cost",
                                      "total_diff_lower", "total_diff_upper"), # dont want to melt the id columns
                  variable.name = "metric", value.name = "diff")
# im using the lower and upper bounds as id.vars here because i need them in seperate columns for plotting the bounds
setDT(state_melt)
# make metric column into factor
if(model=="RC"){
  state_melt[, 
           metric_factor := 
             factor(metric, levels = c("ratio_diff","prevalence_diff", "cost_diff","hours_diff", "total_diff"),
                    labels = c("Age profile","Dementia prevalence", "Home health aide\nhourly cost",
                               "Hours of informal care\nper prevalent case", "Total"))]
}else{
  state_melt[, 
             metric_factor := 
               factor(metric, levels = c("ratio_diff","prevalence_diff", "cost_diff","hours_diff", "total_diff"),
                      labels = c("Age profile","Dementia prevalence", "Expected caregiver wage",
                                 "Hours of informal care\nper prevalent case", "Total"))] 
  
}

# make state variable into factor
state_melt[, State := factor(State)]


# plot stacked bar chart: 
DG_chart = ggplot(state_melt, aes(x = reorder(State, per_capita_cost), y = diff, 
                       ymin = total_diff_lower, ymax = total_diff_upper)) + 
  geom_bar(data = state_melt[metric_factor!="Total"], aes(fill = metric_factor),
           stat="identity", width=0.8) + 
  geom_point(data = state_melt[metric_factor=="Total"], aes(color = "black"), shape = 15,  # 15 shape -> black box
             size = 3) + # if we want boxes to be out of line: position = position_nudge(x = 0.25)
  geom_errorbar(data = state_melt[metric_factor=="Total"]) + 
  #labs(y = "Difference from National Mean", x = "State") + 
  labs(y = paste0("Difference from ",baseline_state," ($ per capita)"), x = "State") + 
  ggtitle(plot_title) + 
  coord_flip() + # flip x-y axis (have states on Y)
  #scale_x_discrete(limits = rev(levels(state_melt$State))) + # need to reverse order of state names because of coordinate flip
  guides(fill = guide_legend(title = "Bars"), color = guide_legend(title = "Box")) + # change legend titles
  scale_color_identity(guide = "legend", labels = "Total") + # add label to legend
  scale_fill_viridis(option ="turbo", begin = 0.25, end = 0.75, discrete = T) + # change color palette
  #labs(color=NULL) + 
  theme(panel.background = element_rect(fill="white"), # add standard IHME figure theme
        #aspect.ratio = ((1 + sqrt(5))/2)^(-1), 
        ## All axes changes
        axis.ticks.length = unit(0.5, "char"), #longer ticks
        text = element_text(size = 14), # set figure font size 
        ## Horizontal axis changes
        axis.line.x = element_line(size = 0.2), # thinner axis lines
        axis.ticks.x = element_line(size = 0.2), # thinner ticks
        axis.text.x = element_text(color = "black", size=10),
        axis.title.x = element_text(size = 16, hjust = 0.45),
                                    #margin = margin(t = 0, r = 7.5, b = 0, l = 0)),
        ## Vertical axis changes
        axis.line.y = element_line(size = 0.2), # thinner axis lines
        axis.ticks.y = element_blank(), # no y axis ticks (gridlines suffice)
        axis.text.y = element_text(color = "black", size=10),
        axis.title.y = element_text(size = 16),
                                    #margin = margin(t = 7.5, r = 0, b = 0, l = 0)),
        ## Legend
        # legend.position = 'none',
        # legend.key = element_rect(fill = NA, color = NA),
        ## Gridlines
        panel.grid.major.x = element_line(color = "gray45", size = 0.1),
        panel.grid.major.y = element_line(color = "gray45", size = 0.1),
        legend.title=element_blank()
    )

# save to pdf: 
pdf_name = paste0(FILEPATH,"Figure3_das_gupta_chart,",model,".pdf")

pdf(width=10, height=8, file = pdf_name)
DG_chart
dev.off()

# save to EPS: 
eps_name = paste0(FILEPATH,"Figure3_das_gupta_chart",model,".eps")

setEPS()
postscript(eps_name, width=10, height=8)
DG_chart
dev.off()

