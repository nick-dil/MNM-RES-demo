# load dependencies
library(tidyverse)
library(googledrive)
# if demo
library(n2khab)

### GDRIVE connection
getPocIdList = function(
    poc_gdrive_link = "https://drive.google.com/drive/folders/1gzrB-5AG-KYHmiQUThyTEhpsboMPHXeT"){
  
  # Authenticate and search for data
  poc_ls = drive_ls(poc_gdrive_link)
  
  # create easier to use (interactively) poc-folder to id mapping in list
  poc_id_list = as.list(poc_ls$id)
  names(poc_id_list) = poc_ls$name
  
  return(poc_id_list)
  
}

readPocSampleData = function(poc_drive_id, addVersion = TRUE, demo = TRUE){
  
  # Get matching CSV file from children
  my_poc_ls = drive_ls(poc_drive_id, recursive = T,
                       pattern = "spatial_samples", type = "csv")
  # my_poc_ls
  
  # Warning if more than 1 CSV found -> take first
  # Stop if no CSV found
  if ( nrow(my_poc_ls) > 1 ) {
    my_poc_ls = my_poc_ls[1,]
    warning("More than 1 spatial_samples CSV found in folder: ", poc_drive_id, "\nUsing first: ", my_poc_ls$id)
  } else if (nrow(my_poc_ls) < 1) {
    stop("No spatial_samples CSV found in folder: ", poc_drive_id)
  }
  
  # Read file from GDrive ID
  mycsv = read.table(
    text = drive_read_string(
      as_id(my_poc_ls)
    ),
    header = T,
    sep = ",",
    fill = T
  )
  
  if (demo){
    
    # Load specifc dependency
    require(n2khab)
    hydr_class_lookup = n2khab::read_types()[, c("type", "hydr_class")]
    
    # Select hardcoded colnames and translate
    mycsv = mycsv[, c("scheme", "stratum", "grts_address")]
    colnames(mycsv) = c('meetnet', "type", "locatie")
    
    # Use lookup-table to match stratum (type) with HC
    mycsv = merge(mycsv, hydr_class_lookup)
  }
  
  if (addVersion){
    # Finally, add POC version
    mycsv$id = drive_get(id=poc_drive_id)$name 
  }
  
  return(mycsv)
}

### DATA wrangling

### Plotting
makeSpgTable = function(poc_table, .id=NA){
  
  # count number of unique values for "locatie" per group
  spg_table = aggregate(locatie ~ meetnet + type + hydr_class,
                        data = poc_table,
                        function(x) length(unique(x)))
  
  colnames(spg_table) = c('meetnet', "type", "hydr_class", "steekproefgrootte" )
  
  # Add identifier column
  if (!is.na(.id)) {
    # manual version tag
    spg_table$id = as.character(.id)
  } else if (length(unique(poc_table$id)) == 1){
    # unique versoin tag in input data -> recuperate
    spg_table$id = as.character(unique(poc_table$id))
  } 
  
  return(spg_table)
}

wrangleSpgTable.demo = function(spg_table,
                                meetnet_select = "GW_03.3",
                                hydr_class_select = c("HC1", "HC12", "HC2") ) {
  out = spg_table  %>% 
    filter( meetnet %in% meetnet_select, hydr_class %in% hydr_class_select ) %>%
    reframe(steekproefgrootte = sum(steekproefgrootte), .by = c(meetnet, type, id)) 
  
  return(out)
}

compareSpgTables = function(spg_table_A, spg_table_B) {
  
  # def output
  plot_out = list("spg_diff_barplot" = NA,
                  "spg_barplot" = NA,
                  "spg_scatter" = NA)
  
  # PLOT 1: plot difference in steekproefgrootte
  # Check data naive
  req_cols = c("meetnet", "type", "steekproefgrootte")
  spg_diff = merge(
    spg_table_A[, req_cols],
    spg_table_B[, req_cols],
    by = c("meetnet", "type"), all = T)
  
  if (sum(is.na(spg_diff[, c(3,4)])) > 0) {
    warning("Not all combinations have data in both tables -> replacing missing values with 0 values")
    spg_diff[is.na(spg_diff)] = 0
  }
  
  # Make difference
  spg_diff$steekproefgrootte.diff = spg_diff$steekproefgrootte.x - spg_diff$steekproefgrootte.y
  
  # Order types based on difference
  spg_diff = spg_diff %>% arrange(desc(steekproefgrootte.diff))
  spg_diff$type = factor(spg_diff$type, levels = unique(spg_diff$type))
  
  # Add id (POCversion) labels
  spg_diff$higher_spg = NA
  spg_diff$higher_spg[spg_diff$steekproefgrootte.diff <= 0] = unique(spg_table_B$id)
  spg_diff$higher_spg[spg_diff$steekproefgrootte.diff > 0] = unique(spg_table_A$id)
  
  # plot
  plot_out$spg_diff_barplot =  spg_diff %>%
    ggplot(aes(x = type, y = steekproefgrootte.diff, fill = higher_spg)) +
    geom_col() +
    facet_grid(vars(meetnet)) + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
    ggtitle(paste0(
      "Difference in steekproefgrootte: ", unique(spg_table_A$id), " v.s. ",
      unique(spg_table_B$id) ))
  
  # PLOT 2: bar plot steekproefgrootte
  # bind data and make sure all fields have a value. If no data -> assume 0  
  spg_table = bind_rows(spg_table_A, spg_table_B) %>% 
    complete(meetnet, type, id, fill = list("steekproefgrootte" = 0))
  
  # also use the same ordenning on the x-axis by making factor
  spg_table$type = factor(spg_table$type, levels = unique(spg_diff$type))
  
  plot_out$spg_barplot = spg_table %>% 
    ggplot(aes(x = type, y = steekproefgrootte, fill = id)) + 
    geom_col(position = "dodge") +
    facet_grid(vars(meetnet)) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
    ggtitle(paste0("Totale steekproefgrootte: ", unique(spg_table_A$id), " v.s. ",unique(spg_table_B$id) ))
  
  # PLOT 3: difference on scatter
  plot_out$spg_scatter = spg_diff %>%
    ggplot(aes(x = steekproefgrootte.x, y = steekproefgrootte.y, label=type)) +
    geom_label(size = 3) +
    geom_abline(intercept = 0, slope = 1) +
    facet_grid(vars(meetnet)) + 
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
    ggtitle(paste0("Steekproefgrootte: ", unique(spg_table_A$id), " v.s. ",unique(spg_table_B$id) )) +
    xlab(paste0("steekproefgrootte ", unique(spg_table_A$id))) +
    ylab(paste0("steekproefgrootte ", unique(spg_table_B$id)))
  
  return(plot_out)
}


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
