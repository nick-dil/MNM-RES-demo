
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

