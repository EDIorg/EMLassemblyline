context('Import templates')
library(EMLassemblyline)

# Parameterize ----------------------------------------------------------------

path <- system.file(
  '/examples/templates/abstract.txt', 
  package = 'EMLassemblyline'
)
path <- substr(path, 1, nchar(path) - 23)

data.path <- paste0(path, '/data')

path <- paste0(path, '/templates')

# Tests -----------------------------------------------------------------------

# Errors

testthat::test_that('Error out when required arguments are missing', {
  
  expect_error(
    import_templates(
      license = 'CC0',
      data.path = data.path
    )
  )
  
  expect_error(
    import_templates(
      path = path,
      data.path = data.path
    )
  )
  
})

# Messages

testthat::test_that('Return messages when templates already exist', {

  expect_message(
    import_templates(
      path = path,
      license = 'CC0',
      data.path = data.path,
      data.table = c('decomp.csv', 'nitrogen.csv')
    )
  )
})
