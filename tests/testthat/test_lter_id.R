context('Search for an LTER CV term ID')

library(EMLassemblyline)

# Expect numeric output -------------------------------------------------------

output <- lter_id(
  x = 'water temperature')

testthat::test_that('Output should be numeric.', {
  
  expect_equal(
    class(
      output
    ),
    'numeric'
  )
  
})

# Expect error ----------------------------------------------------------------

testthat::test_that('Error should result from invalid terms.', {
  
  expect_error(
    lter_id(
      x = 'ast'
    )
  )
  
})
