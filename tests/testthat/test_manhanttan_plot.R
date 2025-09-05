test_that("plot_manhattan_binary creates Manhattan plot image from input file", {

  input_file <- system.file("data/gwas", "binary_table_vcf.tsv", package = "StoatPlot")

  # Temporary output file
  output_file <- tempfile(fileext = ".png")

  # Run the function
  plot_manhattan_binary(file_path = input_file, output_manhattan = output_file)

  # Check that PNG file was created and is not empty
  expect_true(file.exists(output_file))
  expect_gt(file.info(output_file)$size, 0)
})

test_that("plot_manhattan_quantitative creates Manhattan plot image from input file", {

  input_file <- system.file("data/gwas", "binary_table_vcf.tsv", package = "StoatPlot")

  # Temporary output file
  output_file <- tempfile(fileext = ".png")

  # Run the function
  plot_manhattan_quantitative(file_path = input_file, output_manhattan = output_file)

  # Check that PNG file was created and is not empty
  expect_true(file.exists(output_file))
  expect_gt(file.info(output_file)$size, 0)
})