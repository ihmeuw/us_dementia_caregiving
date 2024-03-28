## Dementia Model Data Coverage Plot
## 02/15/2023
## Author: Michael Breshock

rm(list = ls())

# load in libraries
library(dplyr)
library(data.table)
library(ggplot2) 
library(grid)
library(scales)
library(gridExtra)
library(viridis)
source("/ihme/cc_resources/libraries/current/r/get_location_metadata.R")

# specify directory roots
if (Sys.info()["sysname"] == "Linux") {
  j_root <- "/home/j/" 
  h_root <- "~/"
  l_root <- "/ihme/limited_use/"
  functions_dir <- "/ihme/cc_resources/libraries/current/r/"
} else { 
  j_root <- "J:/"
  h_root <- "H:/"
  l_root <- "L:/"
  functions_dir <- "K:/libraries/current/r/"
}

# load raw input data: 
BRFSS = fread(paste0(j_root, "Project/IRH/Informal_Care_AD/US_dementia_spending_2022/ST-GPR/care_hours_168/caregiving_data_168.csv"))[location_id %in% c(523:573)]
NHATS <- fread(paste0(j_root,'Project/IRH/Informal_Care_AD/US_dementia_spending_2022/data/model_inputs/NHATS_OP_hours_2019_SP_weights_samp_var.csv'))
HRS <- fread(paste0(j_root,'Project/IRH/Informal_Care_AD/US_dementia_spending_2022/data/model_inputs/HRS_caregiving_hours_2010_2018_55up_168.csv'))

# add name of source column to each data source
BRFSS[, source := "BRFSS"]
NHATS[, source := "NHATS"]
HRS[, source := "HRS"]

# add location_id to NHATS and HRS data
NHATS[, location_id := 102] # 102 = united states of america
HRS[, location_id := 102]

# change year_id to year in BRFSS
setnames(BRFSS, old = "year_id", new = "year")

# extract just the year, location_id, and sample_size columns and 
# row bind all data sources together 
data_sources = rbind(BRFSS[,.(year, location_id, source, sample_size)], 
                     NHATS[,.(year, location_id, source, sample_size)], 
                     HRS[,.(year, location_id, source, sample_size)])

# get state names from location metadata
loc_meta = get_location_metadata(location_set_id = 22, release_id = 6)
locs = loc_meta[, .(location_id, lancet_label)]

sources_named = merge(data_sources, locs, by = "location_id")

fig <- ggplot(data = sources_named,
              aes(x = year,
                  y = factor(lancet_label, levels = rev(unique(sources_named$lancet_label))),
                  color = source)) + 
  # geom_point(position=position_dodge(w=0.1),alpha= 0.7) +
  geom_point(aes(size = sample_size), alpha= 0.4) +
  labs(title = "Dementia Model Inputs National Data Coverage", x = "", y = "") +
  guides(size = guide_legend(order = 1, nrow = 1, byrow = TRUE)) +
  scale_size_continuous(name = 'Sample Size') +
  theme_minimal() +
  theme(plot.title = element_text(hjust = -0.1, vjust = 0, margin = margin(t = 8)),
        plot.margin = unit(c(0, 0.5, -0.35, 0), "cm"),
        axis.text = element_text(size = 8), 
        strip.text.x = element_text(size = 8),
        legend.direction = "horizontal",
        legend.justification = 'bottom',
        legend.title.align = .5,
        legend.text = element_text(size = 8, margin = margin(l = 0, unit = "pt")),
        legend.title = element_text(size = 8),
        #legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"),
        legend.spacing.x = unit(0.05, 'cm'),
        legend.box.margin = margin(t = -20, r = 0, b = 0, l = 300 , unit = "pt"),
        legend.background = element_rect(colour = 1, size = 0.2),
        legend.position = "top", 
        axis.text.x = element_text(margin = margin(-5, 0, 0, 0))) + 
  scale_color_viridis(discrete = TRUE) + 
  scale_x_continuous(limits = c(2010, 2019), breaks = seq(2010, 2019, 2))

# save to pdf: 
pdf_name = paste0(j_root, "Project/IRH/Informal_Care_AD/US_dementia_spending_2022/figures/publication/appendix/FigureA1_data_coverage.pdf")

pdf(width=10, height=8, file = pdf_name)
fig
dev.off()

# save to EPS: 
eps_name = paste0(j_root, "Project/IRH/Informal_Care_AD/US_dementia_spending_2022/figures/publication/appendix/FigureA1_data_coverage.eps")

setEPS()
postscript(eps_name, width=10, height=8)
fig
dev.off()

