
context("template_raster_attributes()")
library(EMLassemblyline)

testthat::test_that("raster_attributes.txt characteristics", {
  
  # Create provenance.txt
  template_raster_attributes(path = tempdir())
  expect_true(
    file.exists(
      paste0(tempdir(), "/raster_attributes.txt")))
  
  # Clean up
  unlink(
    paste0(tempdir(), "/raster_attributes.txt"), 
    recursive = TRUE, 
    force = TRUE)
  
  # Return as data frame
  d <- template_raster_attributes(return.obj = TRUE)
  expect_true(
    is.data.frame(d))
  
})
