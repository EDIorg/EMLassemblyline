
context('utilities.R')
library(EMLassemblyline)

# set_methods_md() ------------------------------------------------------------

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
