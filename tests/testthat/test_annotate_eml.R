
context("annotate_eml()")
library(EMLassemblyline)

# Testing of other annotation characteristics is implemented in the annotations
# section of test_make_eml.R

# from .xml -------------------------------------------------------------------

testthat::test_that("from .xml", {

  file.copy(
    from = system.file(
      "/examples/pkg_260", 
      package = "EMLassemblyline"),
    to = tempdir(),
    recursive = TRUE)
  
  eml <- annotate_eml(
    annotations = paste0(
      tempdir(),
      "/pkg_260/metadata_templates/annotations.txt"),
    eml.in = system.file(
      "/examples/eml/edi.260.3.xml", 
      package = "EMLassemblyline"),
    eml.out = paste0(
      tempdir(),
      "/pkg_260/eml/edi.260.4.xml"))

  expect_true(
    EML::eml_validate(eml))
  
  # Clean up
  
  unlink(
    paste0(tempdir(), "/pkg_260"), 
    recursive = TRUE, 
    force = TRUE)
  
})




# from metadata templates -----------------------------------------------------
# Other tests are implemented in test_make_eml.R under the "annotations" 
# heading.

testthat::test_that("metadata templates", {
  
  # Parameterize
  
  attr_tmp <- read_template_attributes()
  x <- template_arguments(
    path = system.file(
      '/examples/pkg_260/metadata_templates',
      package = 'EMLassemblyline'),
    data.path = system.file(
      '/examples/pkg_260/data_objects',
      package = 'EMLassemblyline'),
    data.table = c("decomp.csv", "nitrogen.csv"),
    other.entity = c("ancillary_data.zip", "processing_and_analysis.R"))
  x$x$template$taxonomic_coverage.txt <- NULL
  x$x$template$annotations.txt <- NULL
  x$data.path <- system.file('/examples/pkg_260/data_objects', package = 'EMLassemblyline')
  x$data.table <- c("decomp.csv", "nitrogen.csv")
  x$data.table.name <- c("Decomp file name", "Nitrogen file name")
  x$data.table.description <- c("Decomp file description", "Nitrogen file description")
  x$data.table.quote.character  <- c("\\'", "\\'")
  x$data.table.url <- c("https://url/to/decomp.csv", "https://url/to/nitrogen.csv")
  x$dataset.title <- 'Sphagnum and Vascular Plant Decomposition under Increasing Nitrogen Additions: 2014-2015'
  x$eml.path <- system.file('/examples/pkg_260/eml', package = 'EMLassemblyline')
  x$geographic.coordinates <- c('55.895', '112.094','55.895', '112.094')
  x$geographic.description <- 'Alberta, Canada, 100 km south of Fort McMurray, Canada'
  x$maintenance.description <- 'Completed'
  x$other.entity <- c("ancillary_data.zip", "processing_and_analysis.R")
  x$other.entity.name <- c("ancillary_data file name", "processing_and_analysis file name")
  x$other.entity.description <- c("ancillary_data file description", "processing_and_analysis file description")
  x$other.entity.url <- c("https://url/to/ancillary_data.zip", "https://url/to/processing_and_analysis.R")
  x$package.id <- "edi.100.1"
  x$path <- system.file('/examples/pkg_260/metadata_templates', package = 'EMLassemblyline')
  x$provenance <- NULL
  x$return.obj <- T
  x$temporal.coverage <- c('2014-05-01', '2015-10-31')
  x$user.domain <- c("EDI", "LTER")
  x$user.id <- c("userid1", "userid2")
  x$write.file <- F
  
  # Get emld object from make_eml()
  
  x1 <- x
  r <- do.call(make_eml, x1[names(x1) %in% names(formals(make_eml))])
  
  # Annotate the emld object
  
  x <- template_arguments(
    path = system.file(
      '/examples/pkg_260/metadata_templates',
      package = 'EMLassemblyline'),
    data.path = system.file(
      '/examples/pkg_260/data_objects',
      package = 'EMLassemblyline'),
    data.table = c("decomp.csv", "nitrogen.csv"),
    other.entity = c("ancillary_data.zip", "processing_and_analysis.R"))
  
  eml <- annotate_eml(
    annotations = x$x$template$annotations.txt$content,
    eml.in = r)
  
  expect_true(
    EML::eml_validate(r))
  
})
