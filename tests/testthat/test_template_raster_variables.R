
context("template_raster_variables()")
library(EMLassemblyline)

testthat::test_that("raster_variables.txt characteristics", {
  
  # Create provenance.txt

  r <- data.frame(
    filename = "test_file",
    numberType = "categorical"
  )
  
  utils::write.table(
    r,
    paste0(tempdir(), "/", "raster_attributes.txt"),
    sep = "\t",
    row.names = F,
    quote = F,
    fileEncoding = "UTF-8")
  
  template_raster_variables(path = tempdir())
  expect_true(
    file.exists(
      paste0(tempdir(), "/raster_catvars.txt")))
  
  # Clean up
  unlink(
    paste0(tempdir(), "/raster_catvars.txt"), 
    recursive = TRUE, 
    force = TRUE)
  
  # Return as data frame
  d <- template_raster_variables(path = tempdir(), write.file = FALSE, return.obj = TRUE)
  expect_true(
    is.data.frame(d))
  
  
  # Clean up
  unlink(
    paste0(tempdir(), "/raster_attributes.txt"), 
    recursive = TRUE, 
    force = TRUE)
  
})
