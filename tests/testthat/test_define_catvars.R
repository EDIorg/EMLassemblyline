
context('Define categorical variables')
library(EMLassemblyline)

# Parameterize ----------------------------------------------------------------

path <- system.file('/examples/templates/abstract.txt', package = 'EMLassemblyline')
parent_dir <- substr(path, 1, nchar(path)-23)

# Tests -----------------------------------------------------------------------

testthat::test_that('Error when attribute files are missing', {
  expect_error(
    define_catvars(path = paste0(parent_dir, '/data'),
                   data.path = paste0(parent_dir, '/data'))
  )
})

testthat::test_that('Return message when catvars file already exists', {
  output <- suppressMessages(
    define_catvars(
      path = paste0(parent_dir, '/templates'),
      data.path = paste0(parent_dir, '/data')
      )
  )
  expect_equal(output, NULL)
})

testthat::test_that('Return data frame when files have not yet been created', {
  output <- suppressMessages(
    define_catvars(
      path = paste0(parent_dir, '/templates_cv'),
      data.path = paste0(parent_dir, '/data')
      )
    )
  expect_equal(colnames(output), c('attributeName', 'code', 'definition'))
  expect_equal(class(output), 'data.frame')
})

