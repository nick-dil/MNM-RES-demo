library(googledrive)

getPocIdList = function(
    poc_gdrive_link = "https://drive.google.com/drive/folders/1gzrB-5AG-KYHmiQUThyTEhpsboMPHXeT"){
  
  # Authenticate and search for data
  poc_ls = drive_ls(poc_gdrive_link)
  
  # create easier to use (interactively) poc-folder to id mapping in list
  poc_id_list = as.list(poc_ls$id)
  names(poc_id_list) = poc_ls$name
  
  return(poc_id_list)
  
}

readPocSampleData = function(poc_drive_id, demo = TRUE){
  
  # Get matching CSV file from children
  my_poc_ls = drive_ls(poc_drive_id, recursive = T,
                       pattern = "spatial_samples", type = "csv")
  my_poc_ls
  
  # Warning if more than 1 CSV found -> take first
  # Stop if no CSV found
  if ( nrow(my_poc_ls) > 1 ){
    my_poc_ls = my_poc_ls[1,]
    warning("More than 1 spatial_samples CSV found in folder: ", poc_drive_id, "\nUsing first found: ", my_poc_ls$id)
  } else if (nrow(my_poc_ls) < 1){
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
    
    # Load dependency
    require(n2khab)
    hydr_class_lookup = n2khab::read_types()[, c("type", "hydr_class")]
    
    # Select hardcoded colnames and translate
    mycsv = mycsv[, c("scheme", "stratum", "grts_address")]
    colnames(mycsv) = c('meetnet', "type", "locatie")
    
    # Use lookup-table to match stratum (type) with HC
    mycsv = merge(mycsv, hydr_class_lookup)
  }
  
  return(mycsv)
}


#### MAIN ####

# Retrieve POC dir ids ~ version
poc_id_list = getPocIdList()

# Select poc version from list, e.g. 10
my_poc_id = poc_id_list$poc_0.10.0

# Read spatial_sample.csv for corresponding POC from GDrive
my_poc_data = readPocSampleData(my_poc_id)

head(my_poc_data)
