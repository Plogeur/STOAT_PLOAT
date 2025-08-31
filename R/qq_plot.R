#' Q-Q Plots for STOAT GWAS Results
#'
#' @description Generate Q-Q plots for quantitative or binary for STOAT GWAS Results.
#' @name qq_plot

#' @export
qq_plot_quantitative <- function(file_path, output_qqplot = "qq_plot.png") {
  data <- data.table::fread(file_path, sep = "\t", data.table = FALSE)
  data <- na.omit(data[ , c("P")])
  png(output_qqplot, width = 600, height = 600)
  qqman::qq(data$P, main = "Q-Q Plot (Quantitative)")
  dev.off()
}

#' @export
qq_plot_binary <- function(file_path, output_qqplot = "qq_plot.png") {
  data <- data.table::fread(file_path, sep = "\t", data.table = FALSE)
  data <- na.omit(data[ , c("P_CHI2")])
  png(output_qqplot, width = 600, height = 600)
  qqman::qq(data$P_CHI2, main = "Q-Q Plot (Binary)")
  dev.off()
}
