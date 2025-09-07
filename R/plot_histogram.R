#' P-value Histogram for STOAT GWAS Results
#'
#' @description Generate histogram of P-values from STOAT GWAS results TSV.
#'
#' @param input Path to input TSV file.
#' @param output_file Filename to save the output plot (default: "pvalue_distribution_plot.png").
#' @param p_threshold Maximum P-value threshold to include in the plot (default: 0.1).
#' @param bin Number of bins in the histogram (default: 200).
#'
#' @return Saves a histogram plot as an image file.
#' @export
plot_pvalue_hist <- function(input, output_file = "pvalue_distribution_plot.png", p_threshold = 0.1, bin = 200) {

  # Read data
  data <- read.table(input, header = TRUE, sep = "\t", stringsAsFactors = FALSE, 
                     check.names = FALSE, comment.char = "" )
  colnames(data)[1] <- sub("^#", "", colnames(data)[1])  # remove leading #

  # Check if P column exists
  if (!("P" %in% colnames(data)) && !("P_CHI2" %in% colnames(data))) {
    stop("Neither 'P' nor 'P_CHI2' column found in the input file.")
  }

  # Convert P column to numeric safely
  if ("P_CHI2" %in% colnames(data)) {
    data$P <- as.numeric(as.character(data$P_CHI2))
  } else {
    data$P <- as.numeric(as.character(data$P))
  }

  # Remove NAs and filter by threshold
  data_filtered <- data[!is.na(data$P) & data$P <= p_threshold, ]

  if (nrow(data_filtered) == 0) {
    stop("No valid P-values found within the specified threshold.")
  }

  # Plot histogram
  p <- ggplot2::ggplot(data_filtered, ggplot2::aes(x = P)) +
    ggplot2::geom_histogram(bins = bin, fill = "skyblue", color = "black") +
    ggplot2::theme_bw() +
    ggplot2::labs(title = paste("Distribution of P-values (0 -", p_threshold, ")"),
                  x = "P-value",
                  y = "Frequency")

  # Save plot
  ggplot2::ggsave(output_file, plot = p, width = 8, height = 6, dpi = 300)
}
