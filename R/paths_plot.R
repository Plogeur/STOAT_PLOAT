#' Dot Plot of Path Length Frequencies
#'
#' @description Create a dot plot from a TSV file containing a `path_length` column.
#'
#' @param input Path to the input TSV file.
#' @param output Path to save the output plot image.
#'
#' @return Saves a dot plot to the specified file.
#' @name plot_path_length_distribution
#' @export
plot_path_length_distribution <- function(input, output="paths_length_distribution_plot.png") {
  df <- read.table(input, header = TRUE)

  if (!"path_length" %in% colnames(df)) {
    stop("Column 'path_length' not found in the data.")
  }

  freq_table <- as.data.frame(table(df$path_length))
  colnames(freq_table) <- c("PathLength", "Frequency")
  freq_table$PathLength <- as.numeric(as.character(freq_table$PathLength))

  plot <- ggplot2::ggplot(freq_table, ggplot2::aes(x = PathLength, y = Frequency)) +
    ggplot2::geom_point(size = 3, color = "steelblue") +
    ggplot2::labs(
      title = "Dot Plot of Path Length Frequency",
      x = "Path Length",
      y = "Frequency"
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(text = ggplot2::element_text(size = 14))

  ggplot2::ggsave(output, plot, width = 6, height = 4)
}