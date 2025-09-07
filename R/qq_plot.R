#' Q-Q Plot for GWAS Results
#' @description Generate QQ plot from STOAT GWAS results using P or P_CHI2 column.
#'
#' @param input Path to the input TSV file (must contain a column named 'P').
#' @param output_qqplot Filename for the output PNG plot (default: "qq_plot.png").
#'
#' @return Saves a Q-Q plot image.
#' @name qq_plot
#' @export
qq_plot <- function(input, output_qq = "qq_plot.png") {

  # Read input file
  data <- read.table(input, header = TRUE, sep = "\t", stringsAsFactors = FALSE, check.names = FALSE, comment.char = "" )
  colnames(data)[1] <- sub("^#", "", colnames(data)[1])  # remove leading #

  # Check for P or P_CHI2 column
  if (!("P" %in% colnames(data)) && !("P_CHI2" %in% colnames(data))) {
    stop("Neither 'P' nor 'P_CHI2' column found in the input file.")
  }

  p_column <- if ("P" %in% colnames(data)) "P" else "P_CHI2"

  # Clean P-values
  pvals <- as.numeric(data[[p_column]])
  pvals <- pvals[!is.na(pvals) & pvals > 0]
  pvals <- pmax(pvals, 1e-300)  # Avoid log(0)

  # Expected and observed -log10(P)
  n <- length(pvals)
  expected <- -log10((1:n) / (n + 1))
  observed <- -log10(sort(pvals))

  # Data frame for plotting
  plot_df <- data.frame(Expected = expected, Observed = observed)

  # Plot
  p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = Expected, y = Observed)) +
    ggplot2::geom_point(size = 1.5, color = "steelblue") +
    ggplot2::geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
    ggplot2::labs(
      title = "QQ Plot",
      x = "Expected -log10(P)",
      y = "Observed -log10(P)"
    ) +
    ggplot2::theme_bw(base_size = 14) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5)
    )

  # Save plot
  ggplot2::ggsave(output_qq, plot = p, width = 6, height = 6)
}
