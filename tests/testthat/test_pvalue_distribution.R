test_that("plot_pvalue_hist binary creates JPEG histogram from input file", {
  # Locate test input file
  input_file <- system.file("data/gwas", "binary_table_vcf.tsv", package = "StoatPlot")

  # Temporary output file
  output_file <- tempfile(fileext = ".jpeg")

  # Run the function
  plot_pvalue_hist(file_path = input_file, output_file = output_file)

  # Check that the JPEG file exists
  expect_true(file.exists(output_file))

  # Optional: check that file is non-empty
  expect_gt(file.info(output_file)$size, 0)
})

test_that("plot_pvalue_hist quantitative creates JPEG histogram from input file", {
  # Locate test input file
  input_file <- system.file("data/gwas", "quantitative_table_vcf.tsv", package = "StoatPlot")

  # Temporary output file
  output_file <- tempfile(fileext = ".jpeg")

  # Run the function
  plot_pvalue_hist(file_path = input_file, output_file = output_file)

  # Check that the JPEG file exists
  expect_true(file.exists(output_file))

  # Optional: check that file is non-empty
  expect_gt(file.info(output_file)$size, 0)
})
