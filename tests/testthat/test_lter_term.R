context('Search for a term in the LTER CV')
library(EMLassemblyline)

# term_found T or F
# if term_found = F and messages = T, expect a report
# Validate a random collection of terms ?


# Expect logical response -----------------------------------------------------

testthat::test_that('Output should be logical.', {

  output <- lter_term(
    x = 'water temperature')
  
  expect_output(
    class(
      output
      ),
    'logical'
    )

})
