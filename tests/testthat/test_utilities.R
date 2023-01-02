
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


testthat::test_that('name_attributes_templates()', {
  f <- c("decomp.csv", "nitrogen.csv")
  tmplts <- name_attributes_templates(f)
  expected <- c("attributes_decomp.txt", "attributes_nitrogen.txt")
  expect_true(setequal(tmplts, expected))
})


testthat::test_that('name_data_objects()', {
  testdir <- paste0(tempdir(), "/pkg")
  dir_files <- copy_test_package(testdir)
  attr_files <- c("attributes_decomp.txt", "attributes_nitrogen.txt")
  data_objects <- name_data_objects(attr_files, testdir)
  expected <- c("decomp.csv", "nitrogen.csv")
  expect_true(setequal(expected, data_objects))
  unlink(testdir, recursive = TRUE, force = TRUE)
})


testthat::test_that("read_data_objects()", {
  # Parameterize the test
  test_dir <- paste0(tempdir(), "/pkg")
  pkg_files <- copy_test_package(test_dir)
  expected_object_names <- c(
    "ancillary_data.zip",
    "decomp.csv",
    "nitrogen.csv",
    "processing_and_analysis.R"
  )
  expected_object_properties <- c(
    "content", 
    "eml_entity_type", 
    "mime_type", 
    "file_path"
  )
  other_entities <- c("ancillary_data.zip", "processing_and_analysis.R")
  
  # Assert properties of the returned object
  d <- read_data_objects(test_dir)
  expect_type(d, "list")
  for (i in seq_along(d)) {
    expect_true(is.element(names(d[i]), expected_object_names))
    expect_true(setequal(names(d[[i]]), expected_object_properties))
    if (is.element(names(d[i]), other_entities)) {
      expect_true(is.na(d[[i]]$content))
    }
    expect_true(!is.na(d[[i]]$eml_entity_type))
    expect_true(!is.na(d[[i]]$mime_type))
    expect_true(!is.na(d[[i]]$file_path))
  }
  
  # Clean up
  unlink(test_dir, recursive = TRUE, force = TRUE)
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
