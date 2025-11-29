
# util function
checkIsDemoCols = function(x){
  demoCols = c('meetnet', "type", "hydr_class")
  for (colname.i in demoCols) {
    if ( !colname.i %in% colnames(x) ) {
      stop("Column '",colname.i, "' not found in input!\nMake sure input is DEMO data\nSee ?readPocSampleData")
    }
  }
}

#' Aggregate sample table (DEMO)
#'
#' @param poc_table \link[base]{data.frame} generated with \link[spg]{readPocSampleData} with isDemo = TRUE (IMPORTANT)
#' @param .id If NA (default), try to recuperate id from input. If set, a string to add as id column.
#'
#' @returns Aggregated data into sample size per meetnet + type + hydr_class
#' @export
#'
#' @examples
#' my_id = getPocIdList()[[1]]
#' makeSpgTable(readPocSampleData(my_id))
makeSpgTable = function(poc_table, .id=NA){
  
  # assert isDemo
  checkIsDemoCols(poc_table)
  
  # count number of unique values for "locatie" per group
  spg_table = stats::aggregate(locatie ~ meetnet + type + hydr_class,
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

#' Prune and summarize data for demonstration exercise objective (DEMO)
#'
#' @param spg_table Aggregated sample size per group. See \link[spg]{makeSpgTable}
#' @param meetnet_select (optional) Choose 1 or more meetnetten to include in table, can be string or character vector. DEFAULT is "GW_03.3" per demo objective.
#' @param hydr_class_select (optional) Choose 1 or more "hydr_class" to summarize by, can be string or character vector. DEFAULT is c("HC1", "HC12", "HC2"), the terrestrial "hydr_classes", per demo objective.
#'
#' @returns Object of \link[base]{data.frame}. Column "steekproefgrootte" corresponds to unique locations per group, and translates to "sample size" in English.
#' @export
#'
#' @examples
#' my_id = getPocIdList()[[1]]
#' wrangleSpgTable(makeSpgTable(readPocSampleData(my_id)))
wrangleSpgTable = function(spg_table,
                           meetnet_select = "GW_03.3",
                           hydr_class_select = c("HC1", "HC12", "HC2") ) {
  
  checkIsDemoCols(spg_table)
  
  out_slice = spg_table[spg_table$meetnet %in% meetnet_select & spg_table$hydr_class %in% hydr_class_select,]
  out = stats::aggregate(steekproefgrootte ~ meetnet + type + id, data = out_slice, FUN = sum)
  
  return(out)
}
