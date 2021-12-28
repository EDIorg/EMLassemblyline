
context("template_spatial_attributes()")
library(EMLassemblyline)

testthat::test_that("raster_attributes.txt characteristics", {
  
  # Create provenance.txt
  template_spatial_attributes(path = tempdir(), spatial.type = "raster")
  expect_true(
    file.exists(
      paste0(tempdir(), "/raster_attributes.txt")))
  
  # Clean up
  unlink(
    paste0(tempdir(), "/raster_attributes.txt"), 
    recursive = TRUE, 
    force = TRUE)
  
  # Return as data frame
  d <- template_spatial_attributes(return.obj = TRUE, spatial.type = "raster")
  expect_true(
    is.data.frame(d))
  
})

testthat::test_that("shape_attributes.txt characteristics", {
  
  # Create provenance.txt
  template_spatial_attributes(path = tempdir(), spatial.type = "shape")
  expect_true(
    file.exists(
      paste0(tempdir(), "/shape_attributes.txt")))
  
  # Clean up
  unlink(
    paste0(tempdir(), "/shape_attributes.txt"), 
    recursive = TRUE, 
    force = TRUE)
  
  # Return as data frame
  d <- template_spatial_attributes(return.obj = TRUE, spatial.type = "shape")
  expect_true(
    is.data.frame(d))
  
})

testthat::test_that("vector_attributes.txt characteristics", {
  
  # Create provenance.txt
  template_spatial_attributes(path = tempdir(), spatial.type = "vector")
  expect_true(
    file.exists(
      paste0(tempdir(), "/vector_attributes.txt")))
  
  # Clean up
  unlink(
    paste0(tempdir(), "/vector_attributes.txt"), 
    recursive = TRUE, 
    force = TRUE)
  
  # Return as data frame
  d <- template_spatial_attributes(return.obj = TRUE, spatial.type = "vector")
  expect_true(
    is.data.frame(d))
  
})
