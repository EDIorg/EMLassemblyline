context('Search for a LTER CV scope description.')

library(EMLassemblyline)

# Expect character ------------------------------------------------------------

output <- lter_scope(
  id = 65)

testthat::test_that('Output should be character.', {
  
  expect_equal(
    class(
      output
    ),
    'character'
  )
  
})

