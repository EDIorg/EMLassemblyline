
context('utilities.R')
library(EMLassemblyline)


test_that("get_eol works", {
  
  # Create tempfiles
  
  tmp_newline <- tempfile(fileext = '.txt')
  tmp_carriage <- tempfile(fileext = '.txt')
  tmp_carriage_newline <- tempfile(fileext = '.txt')
  
  # Create text
  
  newline <- paste(rep("A line\n", 10), collapse = "")
  carriage <- paste(rep("A line\r", 10), collapse = "")
  carriage_newline <- paste(rep("A line\r\n", 10), collapse = "")
  
  # Put test text into temp files
  
  writeChar(newline, tmp_newline, eos = NULL, useBytes = TRUE)
  writeChar(carriage, tmp_carriage, eos = NULL, useBytes = TRUE)
  writeChar(carriage_newline, tmp_carriage_newline, eos = NULL, useBytes = TRUE)
  
  # Test expectations
  
  expect_equal(
    object = get_eol(tempdir(), file.name = basename(tmp_newline)),
    expected = "\\n"
  )
  expect_equal(
    object = get_eol(tempdir(), file.name = basename(tmp_carriage)),
    expected = "\\r"
  )
  expect_equal(
    object = get_eol(tempdir(), file.name = basename(tmp_carriage_newline)),
    expected = "\\r\\n"
  )
  
})


testthat::test_that("is_template()", {
  testdir <- paste0(tempdir(), "/pkg")
  dir_files <- copy_test_package(testdir)
  i <- is_template(dir_files)
  not_tmplts <- basename(dir_files[!i])
  expected <- c(
    "ancillary_data.zip",
    "decomp.csv",
    "nitrogen.csv", 
    "processing_and_analysis.R"
  )
  expect_true(setequal(x = not_tmplts, y = expected))
  unlink(testdir, recursive = TRUE, force = TRUE)
})


testthat::test_that('init_attributes() default settings', {
  # Default returns a data frame with expected columns and no rows.
  res <- init_attributes()
  expected_cols <- c(
    'attributeName',
    'attributeDefinition',
    'class',
    'unit',
    'dateTimeFormatString',
    'missingValueCode',
    'missingValueCodeExplanation'
  )
  actual_cols <- colnames(res)
  expect_true(setequal(expected_cols, actual_cols))
  expect_equal(nrow(res), 0)
  
  # Control the number of rows with the nrows parameter
  res <- init_attributes(nrows = 3)
  expect_equal(nrow(res), 3)
})


testthat::test_that('name_attribute_templates()', {
  f <- c("decomp.csv", "nitrogen.csv")
  tmplts <- name_attribute_templates(f)
  expected <- c("attributes_decomp.txt", "attributes_nitrogen.txt")
  expect_true(setequal(tmplts, expected))
})


testthat::test_that('read_data_objects()', {
  test_dir <- paste0(tempdir(), "/pkg")
  pkg_files <- copy_test_package(test_dir)
  d <- read_data_objects(test_dir)
  # Result should list likely data file names and associated R objects, with 
  # unsupported types being an NA object
})


testthat::test_that('set_methods_md()', {
  # Parameterize
  myfile <- paste0(tempdir(), "/test.md")
  writeLines(text = "File content", con = myfile)
  node <- set_methods_md(myfile)
  # TEST: Text is wrapped in <markdown> and node has expected nesting
  expect_equal(names(node), "methodStep")
  expect_equal(names(node[[1]]), "description")
  expect_equal(names(node[[1]][[1]]), "markdown")
  # Clean up
  unlink(myfile)
})
