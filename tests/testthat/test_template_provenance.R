
context("template_provenance()")
library(EMLassemblyline)

testthat::test_that("provenance.txt characteristics", {
  
  # Create provenance.txt
  template_provenance(path = tempdir())
  expect_true(
    file.exists(
      paste0(tempdir(), "/provenance.txt")))
  
  # Clean up
  unlink(
    paste0(tempdir(), "/provenance.txt"), 
    recursive = TRUE, 
    force = TRUE)
  
  # Return as data frame
  d <- template_provenance(return.obj = TRUE)
  expect_true(
    is.data.frame(d))
  
})