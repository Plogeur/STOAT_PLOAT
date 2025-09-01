#' P-value Histogram for STOAT GWAS Binary Results
#'
#' @description Generate histogram of P-values from STOAT GWAS Binary results TSV.
#'
#' @param file_path Path to input TSV file.
#' @param output_file Filename to save the output plot (default: "pvalue_distribution_plot.png").
#' @param p_threshold Maximum P-value threshold to include in the plot (default: 0.1).
#' @param bin Number of bins in the histogram (default: 200).
#'
#' @return Saves a histogram plot as an image file.
#' @export
plot_pvalue_hist_binary <- function(file_path, output_file="pvalue_distribution_plot.png", p_threshold = 0.1, bin = 200) {
  # Read the TSV file safely as all characters
  data <- read.table(file_path, header = TRUE)  # read everything as character

  # Check if P column exists
  if (!"P_CHI2" %in% colnames(data)) {
    stop("Column 'P_CHI2' not found in the data.")
  }

  # Convert P column to numeric safely
  data$P <- as.numeric(data$P_CHI2)

  # Remove NAs and filter by threshold
  data_filtered <- data[!is.na(data$P_CHI2) & data$P_CHI2 <= p_threshold, ]

  if (nrow(data_filtered) == 0) {
    stop("No valid P-values found within the specified threshold.")
  }

  # Plot histogram
  p <- ggplot2::ggplot(data_filtered, ggplot2::aes(x = data$P_CHI2)) +
    ggplot2::geom_histogram(bins = bin, fill = "skyblue", color = "black") +
    ggplot2::theme_bw() +
    ggplot2::labs(title = paste("Distribution of P-values (0 -", p_threshold, ")"),
         x = "P-value",
         y = "Frequency")

  ggplot2::ggsave(output_file, plot = p, device = "jpeg", width = 8, height = 6, dpi = 300)
}

#' P-value Histogram quantitative for STOAT GWAS Results
#'
#' @description Generate histogram of P-values from STOAT GWAS quantitative results TSV.
#'
#' @param file_path Path to input TSV file.
#' @param output_file Filename to save the output plot (default: "pvalue_distribution_plot.png").
#' @param p_threshold Maximum P-value threshold to include in the plot (default: 0.1).
#' @param bin Number of bins in the histogram (default: 200).
#'
#' @return Saves a histogram plot as an image file.
#' @export
plot_pvalue_hist_quantitave <- function(file_path, output_file="pvalue_distribution_plot.png", p_threshold = 0.1, bin = 500) {
  # Read the TSV file safely as all characters
  data <- read.table(file_path, header = TRUE)  # read everything as character

  # Check if P column exists
  if (!"P" %in% colnames(data)) {
    stop("Column 'P' not found in the data.")
  }

  # Convert P column to numeric safely
  data$P <- as.numeric(data$P)

  # Remove NAs and filter by threshold
  data_filtered <- data[!is.na(data$P) & data$P <= p_threshold, ]

  if (nrow(data_filtered) == 0) {
    stop("No valid P-values found within the specified threshold.")
  }

  # Plot histogram
  p <- ggplot2::ggplot(data_filtered, ggplot2::aes(x = data$P_CHI2)) +
    ggplot2::geom_histogram(bins = bin, fill = "skyblue", color = "black") +
    ggplot2::theme_bw() +
    ggplot2::labs(title = paste("Distribution of P-values (0 -", p_threshold, ")"),
         x = "P-value",
         y = "Frequency")

  ggplot2::ggsave(output_file, plot = p, device = "jpeg", width = 8, height = 6, dpi = 300)
}
