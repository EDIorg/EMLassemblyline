
context("annotate_eml()")
library(EMLassemblyline)

# Testing of other annotation characteristics is implemented in the annotations
# section of test_make_eml.R

testthat::test_that("from .xml", {
  
  file.copy(
    from = system.file(
      "/examples/pkg_260", 
      package = "EMLassemblyline"
    ),
    to = tempdir(),
    recursive = TRUE
  )
  
  eml <- annotate_eml(
    annotations = paste0(
      tempdir(),
      "/pkg_260/metadata_templates/annotations.txt"
    ),
    eml.in = system.file(
      "/examples/eml/edi.260.3.xml", 
      package = "EMLassemblyline"
    ),
    eml.out = paste0(
      tempdir(),
      "/pkg_260/eml/edi.260.4.xml"
    )
  )
  
  expect_true(
    EML::eml_validate(eml)
  )
  
  # Clean up
  
  unlink(
    paste0(tempdir(), "/pkg_260"), 
    recursive = TRUE, 
    force = TRUE
  )
  
})
