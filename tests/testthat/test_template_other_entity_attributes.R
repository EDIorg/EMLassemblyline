context("template_other_entity_attributes()")
library(EMLassemblyline)


testthat::test_that("Templates can be returned as a list of data frames.", {
  # Setup
  testdir <- paste0(tempdir(), "/pkg")
  pkg_files <- copy_test_package(testdir)
  on.exit(unlink(testdir, recursive = TRUE, force = TRUE))
  attributes_files <- dir(testdir, pattern = "attributes_", full.names = TRUE)
  file.remove(attributes_files)
  files <- c("ancillary_data.zip", "processing_and_analysis.R")
  expected <- name_attributes_templates(files)
  # Test
  tmplts <- template_other_entity_attributes(
    path = testdir, 
    other.entity = files,
    write.file = FALSE
  )
  expect_equal(typeof(tmplts), "list")
  expect_true(setequal(x = names(tmplts), y = basename(expected)))
  for (tmplt in tmplts) {
    expect_equal(class(tmplt[1]), "data.frame")
  }
})


testthat::test_that("Templates can be returned as files.", {
  # Setup
  testdir <- paste0(tempdir(), "/pkg")
  pkg_files <- copy_test_package(testdir)
  on.exit(unlink(testdir, recursive = TRUE, force = TRUE))
  attributes_files <- dir(testdir, pattern = "attributes_", full.names = TRUE)
  file.remove(attributes_files)
  files <- c("ancillary_data.zip", "processing_and_analysis.R")
  expected <- name_attributes_templates(files)
  # Test
  tmplts <- template_other_entity_attributes(
    path = testdir, 
    other.entity = files,
    write.file = TRUE
  )
  expect_true(all(is.element(expected, dir(testdir))))
})
