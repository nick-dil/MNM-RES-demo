############################################################################
#### Tutorial on how to compare sample sizes for PAS-effectenmonitoring ####
############################################################################

# Load this package
library(spg)
# using tidy language
library(tidyverse)

##################################
#### Part 1: Read sample data ####
##################################

# List available datasets in remote
## if run first time in session -> gdrive authenticate
poc_id_list = getPocIdList()
poc_id_list

# Read the dataset you want to use
my_poc_data_raw = readPocSampleData(
  poc_id_list$poc_0.13.1,
  addVersion = F,
  isDemo = F)

# Inspect the dataset
head(my_poc_data_raw)

# now read the data per the demo instructions. Also see ?readPocSampleData
my_poc_data = readPocSampleData(
  poc_id_list$poc_0.13.1,
  isDemo = T)

head(my_poc_data)

# inspect (new) elements
table(my_poc_data$hydr_class)
table(my_poc_data$meetnet)

# examples on how to filter on column
head(my_poc_data %>% filter( meetnet == "HQ6510"))
head(my_poc_data %>% filter( hydr_class == "HC23"))

# filter on 2 columns
head(my_poc_data %>% filter( meetnet == "GW_03.3", hydr_class == "HC12"))

#######################################
#### Part 2: Calculate sample size ####
#######################################

# Aggregate number of sample locations per group. 
# Lets try on the raw data
my_spg_data = my_poc_data_raw %>% makeSpgTable()

# This gives an error. To continue we need the data from the demo because we will
# need the extra column to aggragate
my_spg_data = my_poc_data %>% makeSpgTable()

# Check dim reduction by aggregation
dim(my_poc_data)
dim(my_spg_data)

# Show first lines of sample size data
head(my_spg_data)

# To continue, the demonstration requires another aggregation to combine different hydr_classes
# and a selection of only 1 meetnet

# Example for 1 meetnet aggregate all hydr_class
my_demo = my_spg_data %>% 
  wrangleSpgTable(
    meetnet_select = "SOIL_03.2",
    hydr_class_select = levels(my_spg_data$hydr_class))

#####################################################################
#### Part 3: Compare the sample size per type for 2 POC versions ####
#####################################################################

# Create the final data tables for 2 different POC versions
# note: The default parameters for data wrangling are all ~ demonstration exercise
# so we don't need to provide them explicitly
# We will show that you can include multiple meetnetten

my_meetnet_select = c("SOIL_03.2", "GW_03.3")

demo.v131 = readPocSampleData(poc_id_list$poc_0.13.1) %>% 
  makeSpgTable() %>% wrangleSpgTable(meetnet_select = my_meetnet_select)

demo.v140 = readPocSampleData(poc_id_list$poc_0.14.0) %>% 
  makeSpgTable() %>% wrangleSpgTable(meetnet_select = my_meetnet_select)

# A plotting function is available to generate some (maybe) helpful plots to 
# determine differences in sample size per POC dataset

compare_plots = plotSpgComparison(demo.v131, demo.v140)

# Inspect (and create) the plots
compare_plots$spg_barplot

compare_plots$spg_diff_barplot

compare_plots$spg_scatter

###############################################
#### EXTRA: Customize and save the ggplots ####
###############################################

# add custom ggplot if you want.
compare_plots$spg_diff_barplot + xlab("waterhuishoudingsklassen") + 
  ylab("Verschil in steekproefgrootte") + ylim(-250, 255) + 
  ggtitle("Steeproefgrootte voor alle terrestrische waterhuishoudingsklassen") + 
  guides(fill = guide_legend(title = "Meer proeven in:")) 

# Store the plots in a data/ dir
sapply(names(compare_plots), function(x) ggsave(paste0(x, ".png"), compare_plots[[x]], path = "examples/data" ))

# All done!