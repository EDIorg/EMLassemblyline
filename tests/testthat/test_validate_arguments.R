context('validate_arguments()')
library(EMLassemblyline)

testthat::test_that('template_annotations()', {
  
  
  # Missing path
  expect_error(
    validate_arguments(
      fun.name = 'template_annotations',
      fun.args = list(path = NULL)
    )
  )
  
  # Invalid path
  expect_error(
    validate_arguments(
      fun.name = 'template_annotations',
      fun.args = list(path = "/an/invalid/path")
    )
  )
  
})