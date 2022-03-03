context("create_spatialVector()")
library(EMLassemblyline)

#FIXME once there is test data

testthat::test_that("create_spatialVector validates template", {
  
  
  # Create shape_attribute object
  s <- template_spatial_attributes(spatial.type = 'shape', spatial.files = c('missingDef', 'missingShape'), write.file = FALSE, return.obj = TRUE)
  
  # Add test files
  cat(NULL,file=paste0(tempdir(), '/missingDef'))
  
  # Create filler 
  
  s$description <- "test"
  s$geoDescription <- "test"
  
  # Remove def form "missingDef
  
  s[1,3] <- ""
  
  # Test that "missingShape" can't be found, "missingDef" has no definition.
  
  expect_warning(create_spatialVector(path = tempdir(), vector_attributes = s), regexp = 'Could not locate \'missingShape\'')
  
  expect_warning(create_spatialVector(path = tempdir(), vector_attributes = s), regexp = 'File \'missingDef\' does not have a description')
}) 
