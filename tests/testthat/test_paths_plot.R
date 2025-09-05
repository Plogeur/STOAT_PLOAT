test_that("plot_path_length_distribution creates PNG plot from valid input file", {
  # Locate test data file
  input_file <- system.file("data/snarl_paths", "snarl_analyse.tsv", package = "StoatPlot")

  # Create temporary output file path
  output_file <- tempfile(fileext = ".png")

  # Run the function
  plot_path_length_distribution(input = input_file, output = output_file)

  # Assert file is created
  expect_true(file.exists(output_file))

  # Optional: file is not empty
  expect_gt(file.info(output_file)$size, 0)
})
