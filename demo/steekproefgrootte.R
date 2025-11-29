# load dependencies
library(tidyverse)
library(googledrive)
# if demo
library(n2khab)

#### MAIN ####

# Retrieve POC dir ids ~ version
poc_id_list = getPocIdList()

# Select poc version from list, e.g. 13
my_poc_id = poc_id_list$poc_0.13.0

# Read spatial_sample.csv for corresponding POC from GDrive
v13.data = readPocSampleData(my_poc_id)

# aggregate all data into steekproefgroottes (unique locations)
v13.spg = v13.data %>% makeSpgTable()

# make data selection and compression for the demo question
v13.demo = v13.spg %>% wrangleSpgTable.demo()

# compare with another POC version, lets take 11 and do it in one "line"
v14.demo = readPocSampleData(poc_id_list$poc_0.14.0) %>%
  makeSpgTable() %>% 
  wrangleSpgTable.demo()

compareSpgTables(v13.demo, v14.demo)
compareSpgTables(v14.demo, v13.demo)


#### TEST ####

# check
head(my_poc_data)
table(my_poc_data$hydr_class)
table(my_poc_data$meetnet)

# examples filter on column
head(my_poc_data %>% filter( meetnet == "HQ6510"))
head(my_poc_data %>% filter( hydr_class == "HC23"))
# filter on 2 columns
head(my_poc_data %>% filter( meetnet == "GW_03.3", hydr_class == "HC12"))

# Create dataset you want to compare

spg_table_A = readPocSampleData(poc_id_list$poc_0.13.0) %>% 
  makeSpgTable(.id = 'POC_v13') %>% 
  wrangleSpgTable.demo()

spg_table_B = readPocSampleData(poc_id_list$poc_0.14.0) %>% 
  makeSpgTable(.id = 'POC_v14') %>% 
  wrangleSpgTable.demo()

spg_plots = compareSpgPlot(spg_table_A, spg_table_B)

spg_plots$spg_barplot
spg_plots$spg_diff_barplot
spg_plots$spg_scatter

# add custom ggplot if you want
spg_plots$spg_diff_barplot + xlab("Stratum") + 
  ylab("Verschil in steekproefgrootte") + ylim(-55, 55) + 
  ggtitle("Gecombineerde data voor alle terrestrische waterhuishoudingsklassen (HC1, HC12 of HC2) ") + 
  guides(fill = guide_legend(title = "Meer proeven in:")) 
