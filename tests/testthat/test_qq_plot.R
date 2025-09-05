test_that("qq_plot_binary creates QQ plot image from input file", {
  # Path to test data in package
  input_file <- system.file("data/gwas", "binary_table_vcf.tsv", package = "StoatPlot")

  # Temporary output file
  output_file <- tempfile(fileext = ".png")

  # Call the function
  qq_plot_binary(file_path = input_file, output_qqplot = output_file)

  # Check that the PNG file was created
  expect_true(file.exists(output_file))

  # Optional: check if file is non-empty
  expect_gt(file.info(output_file)$size, 0)
})

test_that("qq_plot_quantitative creates QQ plot image from input file", {
  # Path to test data in package
  input_file <- system.file("data", "quantitative_table_vcf.tsv", package = "StoatPlot")

  # Temporary output file
  output_file <- tempfile(fileext = ".png")

  # Call the function
  qq_plot_quantitative(file_path = input_file, output_qqplot = output_file)

  # Check that the PNG file was created
  expect_true(file.exists(output_file))

  # Optional: check if file is non-empty
  expect_gt(file.info(output_file)$size, 0)
})