#' Manhattan Plot for GWAS Results
#' @description Generate Manhattan plots from STOAT GWAS results (keeps CHR names like 'chr1', 'chrX', etc.)
#' 
#' @param input Path to the input GWAS TSV file.
#' @param output Path to save the output plot image.
#'
#' @return Saves a manhattan_plot to the specified file.
#' @name manhattan_plot
#' @export
manhattan_plot <- function(input, output_manhattan = "manhattan_plot.png") {
  data <- read.table(input, header = TRUE, sep = "\t", stringsAsFactors = FALSE, check.names = FALSE, comment.char = "" )
  colnames(data)[1] <- sub("^#", "", colnames(data)[1])  # remove leading #

  # Check for P-value column
  if (!("P" %in% colnames(data)) && !("P_CHI2" %in% colnames(data))) {
    stop("Neither 'P' nor 'P_CHI2' column found in the input file.")
  }
  p_column <- if ("P" %in% colnames(data)) "P" else "P_CHI2"

  # Required columns
  if (!all(c("CHR", "START_POS") %in% colnames(data))) {
    stop("Input file must contain columns: 'CHR' and 'START_POS'")
  }

  # Clean START_POS
  data$START_POS <- as.integer(gsub(",", "", as.character(data$START_POS)))

  # Subset and clean
  data <- data[!is.na(data$CHR) & !is.na(data$START_POS) & !is.na(data[[p_column]]), ]
  data <- data.frame(
    CHR = data$CHR,
    BP = data$START_POS,
    P = pmax(as.numeric(data[[p_column]]), 1e-300),
    stringsAsFactors = FALSE
  )

  # Ensure chromosome is treated as a factor to control plotting order
  data$CHR <- factor(data$CHR, levels = unique(data$CHR[order(as.character(data$CHR))]))

  # Sort and calculate cumulative START_POSition
  data <- data[order(data$CHR, data$BP), ]
  chr_lengths <- tapply(data$BP, data$CHR, max)
  chr_offsets <- c(0, cumsum(as.numeric(chr_lengths))[-length(chr_lengths)])
  names(chr_offsets) <- names(chr_lengths)
  data$cum_bp <- data$BP + chr_offsets[as.character(data$CHR)]

  # Calculate -log10(P)
  data$logp <- -log10(data$P)

  # Midpoints for axis labels
  axis_df <- aggregate(cum_bp ~ CHR, data = data, FUN = function(x) (min(x) + max(x)) / 2)

  # Plot
  p <- ggplot2::ggplot(data, ggplot2::aes(x = cum_bp, y = logp)) +
    ggplot2::geom_point(ggplot2::aes(color = CHR), alpha = 0.6, size = 0.7) +
    ggplot2::scale_color_manual(values = rep(c("black", "grey50"), length.out = length(levels(data$CHR)))) +
    ggplot2::scale_x_continuous(breaks = axis_df$cum_bp, labels = axis_df$CHR) +
    ggplot2::labs(
      x = "Chromosome",
      y = expression(-log[10](P)),
      title = "Manhattan Plot"
    ) +
    ggplot2::theme_bw(base_size = 14) +
    ggplot2::theme(
      legend.START_POSition = "none",
      panel.border = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor.x = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, size = 10)
    )

  # Save plot
  ggplot2::ggsave(output_manhattan, plot = p, width = 12, height = 4)
}
