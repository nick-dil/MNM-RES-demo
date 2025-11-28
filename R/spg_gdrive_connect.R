### GDRIVE connection

#' List contents of public POC folder on shared gdrive
#'
#' @param poc_gdrive_link URL to public gdrive folder. DEFAULT is link from demo.
#'
#' @returns named list.
#' @export
#'
#' @examples
#' getPocIdList()
getPocIdList = function(
    poc_gdrive_link = "https://drive.google.com/drive/folders/1gzrB-5AG-KYHmiQUThyTEhpsboMPHXeT"){
  
  # Authenticate and search for data
  poc_ls = googledrive::drive_ls(poc_gdrive_link)
  
  # create easier to use (interactively) poc-folder to id mapping in list
  poc_id_list = as.list(poc_ls$id)
  names(poc_id_list) = poc_ls$name
  
  return(poc_id_list)
  
}

#' Read spatial_sample.csv from gdrive folder
#'
#' @param poc_drive_id gdrive id (see \link[googledrive]{as_id})
#' @param addVersion BOOL to add an extra column to the output with parent folder name (POC_version)
#' @param isDemo BOOL whether select appropriate columns for demostration and, if "n2khab" is available, to add "hydr_class" from n2khab::\link[n2khab]{read_types}[,c("type", "hydr_class")]
#'
#' @returns An object of \link[base]{data.frame}
#' @export
#'
#' @examples
#' my_id = getPocIdList()[[1]]
#' readPocSampleData(my_id)
readPocSampleData = function(poc_drive_id, addVersion = TRUE, isDemo = TRUE){
  
  # Get matching CSV file from children
  my_poc_ls = googledrive::drive_ls(poc_drive_id, recursive = T,
                       pattern = "spatial_samples", type = "csv")
  
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
    text = googledrive::drive_read_string(
      googledrive::as_id(my_poc_ls)
    ),
    header = T,
    sep = ",",
    fill = T
  )
  
  if (isDemo){
    # Select hardcoded colnames and translate
    mycsv = mycsv[, c("scheme", "stratum", "grts_address")]
    colnames(mycsv) = c('meetnet', "type", "locatie")
    
    # Load from specific dependency, else just output selected cols
    if (requireNamespace("n2khab")){
      hydr_class_lookup = n2khab::read_types()[, c("type", "hydr_class")]
      
      # Use lookup-table to match stratum (type) with HC
      mycsv = merge(mycsv, hydr_class_lookup)
    }

  }
  
  if (addVersion){
    # Finally, add POC version
    mycsv$id = googledrive::drive_get(id=poc_drive_id)$name 
  }
  
  return(mycsv)
}
