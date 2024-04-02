context('Create entities template')
library(EMLassemblyline)

testthat::test_that('Test usage with file inputs', {
  
  # Missing path results in error
  
  expect_error(
    suppressMessages(
      template_entities(
        data.path = system.file(
          '/examples/data',
          package = 'EMLassemblyline'
        ),
        write.file = FALSE
      )
    )
  )
  
  # Invalid data path results in error
  
  expect_error(
    suppressMessages(
      template_entities(
        path = system.file(
          '/examples',
          package = 'EMLassemblyline'
        ),
        write.file = FALSE
      )
    )
  )
  
  # Invalid data object file results in error
  
  expect_error(
    suppressMessages(
      template_entities(
        path = system.file(
          '/examples',
          package = 'EMLassemblyline'
        ),
        data.path = system.file(
          '/examples/data',
          package = 'EMLassemblyline'
        ),
        data.objects = c(
          "ancillary_dataaaa.zip",
          "processing_and_analysiiiis.R"
        ),
        write.file = FALSE
      )
    )
  )
})  


testthat::test_that('data.objects defined which files to describe', {
  # Setup
  testdir <- paste0(tempdir(), "/pkg")
  pkg_files <- copy_test_package(testdir)
  
  # data.object = NULL result in files with attribute template are described
  
  tmplt <- template_entities(
    path = testdir,
    write.file = FALSE
  )
  
  expect_equal(nrow(tmplt), 15)
  expect_equal(unique(tmplt$objectName), c("ancillary_data.zip", "geojson_test_file.GeoJSON", "geotiff_test_file.tif"))
  
  # data.object != NULL result in only specified files are described
  
  tmplt <- template_entities(
    path = testdir,
    data.objects = c(
      "ancillary_data.zip",
      "processing_and_analysis.R"
    ),
    write.file = FALSE
  )
  
  expect_equal(nrow(tmplt), 2)
  expect_equal(
    tmplt$objectName, 
    c("ancillary_data.zip", "processing_and_analysis.R")
  )
})


testthat::test_that('Template can be returned as data.frame', {
  # Setup
  testdir <- paste0(tempdir(), "/pkg")
  pkg_files <- copy_test_package(testdir)
  
  # Test
  tmplt <- template_entities(
    path = testdir,
    write.file = FALSE
  )
  expect_equal(class(tmplt), "data.frame")
})


testthat::test_that('Template can be returned as file', {
  # Setup
  testdir <- paste0(tempdir(), "/pkg")
  pkg_files <- copy_test_package(testdir)
  file.remove(paste0(testdir, "/entities.txt"))
  
  # Test
  tmplt <- template_entities(
    path = testdir,
    write.file = TRUE
  )
  expect_true(file.exists(paste0(testdir, "/entities.txt")))
})


testthat::test_that("Return empty templates if instructed.", {
  # Setup
  testdir <- paste0(tempdir(), "/pkg")
  pkg_files <- copy_test_package(testdir)
  
  # Test
  tmplt <- template_entities(
    path = testdir,
    write.file = FALSE,
    empty = TRUE
  )
  
  expect_equal(nrow(tmplt), 0)
})
