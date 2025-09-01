#' Q-Q Plot for Quantitative GWAS Results
#'
#' @description Generate a Q-Q plot for quantitative STOAT GWAS results using the `P` column.
#'
#' @param file_path Path to the input TSV file (must contain a column named 'P').
#' @param output_qqplot Filename for the output PNG plot (default: "qq_plot.png").
#'
#' @return Saves a Q-Q plot image.
#' 
#' @export
qq_plot_quantitative <- function(file_path, output_qqplot = "qq_plot.png") {
  data <- data.table::fread(file_path, sep = "\t", data.table = FALSE)
  data <- na.omit(data[ , c("P")])
  png(output_qqplot, width = 600, height = 600)
  qqman::qq(data$P, main = "Q-Q Plot (Quantitative)")
  dev.off()
}

#' Q-Q Plot for Binary GWAS Results
#'
#' @description Generate a Q-Q plot for binary STOAT GWAS results using the `P_CHI2` column.
#'
#' @param file_path Path to the input TSV file (must contain a column named 'P_CHI2').
#' @param output_qqplot Filename for the output PNG plot (default: "qq_plot.png").
#'
#' @return Saves a Q-Q plot image.
#' @export
qq_plot_binary <- function(file_path, output_qqplot = "qq_plot.png") {
  data <- data.table::fread(file_path, sep = "\t", data.table = FALSE)
  data <- na.omit(data[ , c("P_CHI2")])
  png(output_qqplot, width = 600, height = 600)
  qqman::qq(data$P_CHI2, main = "Q-Q Plot (Binary)")
  dev.off()
}
