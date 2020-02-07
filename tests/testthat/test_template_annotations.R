
context("template_annotations()")
library(EMLassemblyline)

testthat::test_that("annotations.txt characteristics", {
  
  # Create annotations.txt
  
  file.copy(
    from = system.file(
      "/examples/pkg_260", 
      package = "EMLassemblyline"
    ),
    to = tempdir(),
    recursive = TRUE
  )
  
  unlink(
    paste0(tempdir(), "/pkg_260/metadata_templates/annotations.txt"),
    force = TRUE
  )
  
  template_annotations(
    path = paste0(tempdir(), "/pkg_260/metadata_templates"),
    data.path = paste0(tempdir(), "/pkg_260/data_objects"),
    data.table = c("nitrogen.csv", "decomp.csv"),
    other.entity = c("ancillary_data.zip", "processing_and_analysis.R")
  )
  
  df <- data.table::fread(
    paste0(tempdir(), "/pkg_260/metadata_templates/annotations.txt")
  )
  
  # Test for expected characteristics
  
  expect_true(
    is.data.frame(df)
  )
  
  expect_true(
    all(
      colnames(df) %in% c("id", "element", "context", "subject", 
                          "predicate_label", "predicate_uri",
                          "object_label", "object_uri")
    )
  )
  
  expect_true(
    nrow(df) != 0
  )
  
  # Clean up
  
  unlink(
    paste0(tempdir(), "/metadata_templates"), 
    recursive = TRUE, 
    force = TRUE
  )
  
})
