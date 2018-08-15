context('Search for a term in the LTER CV')

library(EMLassemblyline)

# Expect logical response -----------------------------------------------------

output <- lter_term(
  x = 'water temperature')

testthat::test_that('Output should be logical.', {
  
  expect_equal(
    class(
      output
      ),
    'logical'
    )

})

# Expect messages -------------------------------------------------------------

testthat::test_that('Messages should be displayed if terms are not found, and when messages = T.', {
  
  expect_message(
    lter_term(
      x = 'ast',
      messages = T
    )
  )
  
})
