#################################
### Run script DEMO excercise ###
#################################
# Load this pkg
library(spg)
library(dplyr)

main = function(){
  # Retrieve POC dir ids ~ version
  poc_id_list = getPocIdList()
  
  # Read and create aggregated sample size ~ dataset, see exercise
  demo.v131 = readPocSampleData(poc_id_list$poc_0.13.1) %>% 
    makeSpgTable() %>% wrangleSpgTable()
  
  demo.v140 = readPocSampleData(poc_id_list$poc_0.14.0) %>% 
    makeSpgTable() %>% wrangleSpgTable()
  
  # Make comparison plots
  demo_plots = plotSpgComparison(demo.v131, demo.v140)
  
  # Store plots
  sapply(
    names(demo_plots),
    function(x) ggplot2::ggsave(
      paste0(x, ".png"),
      demo_plots[[x]],
      path = "data", create.dir = TRUE)
  )
}

#### MAIN ####

main()
