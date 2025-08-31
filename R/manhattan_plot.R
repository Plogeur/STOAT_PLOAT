#' Manhattan Plots for STOAT GWAS Results
#'
#' @description Generate Manhattan plots for quantitative or binary for STOAT GWAS results.
#' @name manhattan_plot

#' @export
plot_manhattan_quantitative <- function(file_path, output_manhattan = "manhattan_plot.png") {
  data <- data.table::fread(file_path, sep = "\t", data.table = FALSE)
  data <- na.omit(data[ , c("CHR", "POS", "P")])
  data$POS <- as.integer(gsub(",", "", as.character(data$POS)))
  data$P <- pmax(as.numeric(data$P), 1e-300)
  colnames(data) <- c("CHR", "BP", "P")
  data$SNP <- paste0("snp", seq_len(nrow(data)))
  png(output_manhattan, width = 1200, height = 400)
  qqman::manhattan(data, genomewideline = -log10(1e-6), suggestiveline = FALSE,
                   main = "Manhattan Plot (Quantitative)")
  dev.off()
}

#' @export
plot_manhattan_binary <- function(file_path, output_manhattan = "manhattan_plot.png") {
  data <- data.table::fread(file_path, sep = "\t", data.table = FALSE)
  data <- na.omit(data[ , c("CHR", "POS", "P_CHI2")])
  data$POS <- as.integer(gsub(",", "", as.character(data$POS)))
  data$P_CHI2 <- pmax(as.numeric(data$P_CHI2), 1e-300)
  colnames(data) <- c("CHR", "BP", "P")
  data$SNP <- paste0("snp", seq_len(nrow(data)))
  png(output_manhattan, width = 1200, height = 400)
  qqman::manhattan(data, genomewideline = -log10(1e-6), suggestiveline = FALSE,
                   main = "Manhattan Plot (Binary)")
  dev.off()
}
