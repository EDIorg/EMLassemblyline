context("eml2eal()")
library(EMLassemblyline)

# validate_eml_content() ------------------------------------------------------

testthat::test_that("validate_eml_content()", {
  eml <- system.file("eml2eal_test.xml", package = "EMLassemblyline")
  expect_warning( # human readable warnings
    validate_eml_content(eml), 
    regexp = paste0(
      "header lines  > 1 are not supported | ",
      "orientations other than 'column' are not supported | ", 
      "measurement scale from ordinal to nominal of attribute | ",
      "measurement scale from interval to ratio of attribute | ",
      "\'spatialRaster\', \'spatialVector\', \'storedProcedure\', and  | ",
      "more info lost during translation.. See \'eml2eal_losses\\(\\)\'"))
})

# eml2eal_losses() ------------------------------------------------------------

testthat::test_that("eml2eal_losses()", {
  path_eml <- system.file("eml2eal_test.xml", package = "EMLassemblyline")
  path_emleal <- system.file(
    "/examples/pkg_260/eml/edi.260.1.xml", 
    package = "EMLassemblyline")
  losses <- eml2eal_losses(path_eml, path_emleal)
  expect_true(is.list(losses))             # is a vector of character strings
  expect_true(length(losses) == 49)        # has expected amount of unsupported content
  expect_true(length(names(losses)) == 49) # has names
})

# eml2eal() -------------------------------------------------------------------

# Round trip (eml to eal to eml) exercises most functionality
testthat::test_that("eml2eal()", {
  dirname <- paste0(tempdir(), "/eml2eal") # prep
  unlink(dirname, recursive = T, force = T)
  dir.create(dirname)
  suppressWarnings(                                              # eml to eal
    eml2eal(
      eml = system.file("eml2eal_test.xml", package = "EMLassemblyline"),
      path = dirname,
      file.type = ".txt"))
  x <- template_arguments(dirname)$x
  invisible(suppressWarnings(validate_templates("make_eml", x))) # creates template_issues obj
  expect_true(length(template_issues) == 1)                      # only one issue about project parsing
  expect_true(
    stringr::str_detect(template_issues[[1]], "Missing funding information"))
  funcall <- paste0(dirname, "/make_eml.R")                      # eal to eml
  dfiles <- dir(
    system.file(
      "/examples/pkg_260/data_objects/",
      package = "EMLassemblyline"), 
    full.names = T)
  for (i in dfiles) {
    invisible(file.copy(from = i, to = dirname, recursive = T))
  }
  suppressWarnings(source(funcall))
  eml <- dir(dirname, pattern = ".xml", full.names = T)
  expect_true(EML::eml_validate(eml))                            # is schema valid
  unlink(dirname, recursive = T, force = T)
})