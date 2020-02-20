
context("annotate_eml()")
library(EMLassemblyline)

testthat::test_that("from EML.xml", {
  
  # new -------
  annotations <- "C:\\Users\\Colin\\Documents\\EDI\\data_sets\\eml22\\metadata_templates\\annotations.txt"
  
  # old ------
  
  # Read an EML record and create annotations.txt
  
  file.copy(
    from = system.file(
      "/examples/eml/edi.260.3.xml", 
      package = "EMLassemblyline"
    ),
    to = tempdir(),
    recursive = TRUE
  )
  
  template_annotations(
    path = tempdir(),
    eml = "edi.260.3.xml"
  )
  
  df <- data.table::fread(
    paste0(tempdir(), "/annotations.txt")
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
    paste0(tempdir(), "/edi.260.3.xml"),
    force = TRUE
  )
  
  unlink(
    paste0(tempdir(), "/annotations.txt"),
    force = TRUE
  )
  
})
