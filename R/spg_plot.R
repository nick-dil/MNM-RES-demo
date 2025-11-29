
#' Plot comparison of sample size (DEMO)
#'
#' @param spg_table_A Data table as created by \link[spg]{wrangleSpgTable}, see also See \link[spg]{readPocSampleData}
#' @param spg_table_B Data table as created by \link[spg]{wrangleSpgTable}, see also See \link[spg]{readPocSampleData}
#'
#' @returns Named list containing 3 plots.
#' \itemize{
#'   \item $spg_diff_barplot: sample size differences between the two datasets A vs B. Negative values have B more samples than A, and vice versa.
#'   \item $spg_barplot: barplot indicating the absolute sample size per group, per dataset. Ordered on decreasing difference.
#'   \item #spg_scatter: scatterplot to check sample size differences between the two datasets.
#' }
#' @export
#'
#' @importFrom rlang .data
#' @examples
#' spg.df.A = wrangleSpgTable(makeSpgTable(readPocSampleData(getPocIdList()[[6]])))
#' spg.df.B = wrangleSpgTable(makeSpgTable(readPocSampleData(getPocIdList()[[5]])))
#' plots.AB = plotSpgComparison(spg.df.A, spg.df.B)
#' plots.AB$spg_diff_barplot
plotSpgComparison = function(spg_table_A, spg_table_B) {
  
  # def output
  plot_out = list("spg_diff_barplot" = NA,
                  "spg_barplot" = NA,
                  "spg_scatter" = NA)
  
  # PLOT 1: plot difference in steekproefgrootte
  # Check data naive
  req_cols = c("meetnet", "hydr_class", "steekproefgrootte")
  spg_diff = merge(
    spg_table_A[, req_cols],
    spg_table_B[, req_cols],
    by = c("meetnet", "hydr_class"), all = T)
  
  if (sum(is.na(spg_diff[, c(3,4)])) > 0) {
    warning("Not all combinations have data in both tables -> replacing missing values with 0 values")
    spg_diff[is.na(spg_diff)] = 0
  }
  
  # Make difference
  spg_diff$steekproefgrootte.diff = spg_diff$steekproefgrootte.x - spg_diff$steekproefgrootte.y
  
  # Order hydr_classes based on difference
  spg_diff = spg_diff[order(spg_diff$steekproefgrootte.diff, decreasing = T),]
  spg_diff$hydr_class = factor(spg_diff$hydr_class, levels = unique(spg_diff$hydr_class))
  
  # Add id (POCversion) labels
  spg_diff$higher_spg = NA
  spg_diff$higher_spg[spg_diff$steekproefgrootte.diff <= 0] = unique(spg_table_B$id)
  spg_diff$higher_spg[spg_diff$steekproefgrootte.diff > 0] = unique(spg_table_A$id)
  
  # plot
  plot_out$spg_diff_barplot =  ggplot2::ggplot(
    data = spg_diff,
    ggplot2::aes(x = .data$hydr_class,
                 y = .data$steekproefgrootte.diff,
                 fill = .data$higher_spg)) +
    ggplot2::geom_col() +
    ggplot2::facet_grid(ggplot2::vars(.data$meetnet)) + 
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1)) +
    ggplot2::ggtitle(paste0(
      "Difference in steekproefgrootte: ", unique(spg_table_A$id), " v.s. ",
      unique(spg_table_B$id) ))
  
  # PLOT 2: bar plot steekproefgrootte
  spg_table = rbind(spg_table_A, spg_table_B)
  
  # also use the same ordering on the x-axis by making factor
  spg_table$hydr_class = factor(spg_table$hydr_class, levels = unique(spg_diff$hydr_class))
  
  plot_out$spg_barplot = ggplot2::ggplot(
    data = spg_table,
    ggplot2::aes(x = .data$hydr_class,
                 y = .data$steekproefgrootte,
                 fill = .data$id)) + 
    ggplot2::geom_col(position = "dodge") +
    ggplot2::facet_grid(ggplot2::vars(.data$meetnet)) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1)) +
    ggplot2::ggtitle(paste0("Totale steekproefgrootte: ", unique(spg_table_A$id), " v.s. ", unique(spg_table_B$id) ))
  
  # PLOT 3: difference on scatter
  plot_out$spg_scatter = ggplot2::ggplot(
    data = spg_diff, 
    ggplot2::aes(x = .data$steekproefgrootte.x,
                 y = .data$steekproefgrootte.y,
                 label = .data$hydr_class)) +
    ggplot2::geom_label(size = 3) +
    ggplot2::geom_abline(intercept = 0, slope = 1) +
    ggplot2::facet_grid(ggplot2::vars(.data$meetnet)) + 
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1)) +
    ggplot2::ggtitle(paste0("Steekproefgrootte: ", unique(spg_table_A$id), " v.s. ",unique(spg_table_B$id) )) +
    ggplot2::xlab(paste0("steekproefgrootte ", unique(spg_table_A$id))) +
    ggplot2::ylab(paste0("steekproefgrootte ", unique(spg_table_B$id)))
  
  return(plot_out)
}

