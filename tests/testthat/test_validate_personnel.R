context('Validate personnel')
library(EMLassemblyline)

# Parameterize ----------------------------------------------------------------

path <- system.file(
  '/examples/templates/abstract.txt', 
  package = 'EMLassemblyline'
)
path <- substr(path, 1, nchar(path) - 23)

personnel <- read.table(
  paste0(
    path,
    '/templates/personnel.txt'
  ),
  header = T,
  sep = '\t',
  as.is = T
)

# Expect errors ---------------------------------------------------------------

testthat::test_that('Expect errors', {
  
  expect_error(
    validate_personnel(
      personnel[personnel$role != 'creator', ]
    )
  )
  
  expect_error(
    validate_personnel()
  )
  
  expect_error(
    validate_personnel(
      personnel[personnel$role != 'contact', ]
    )
  )
  
  funding_info <- personnel[1, ]
  personnel[ , c('projectTitle', 'fundingAgency', 'fundingNumber')] <- ''
  personnel[2, ] <- funding_info
  expect_error(
    validate_personnel(personnel)
  )
  
})
  
# Expect equal ----------------------------------------------------------------

testthat::test_that('Expect equal', {

  expect_equal(
    class(validate_personnel(personnel)),
    'data.frame'
  )
  
})
