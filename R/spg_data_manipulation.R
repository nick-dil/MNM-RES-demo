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
