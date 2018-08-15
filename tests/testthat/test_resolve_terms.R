context('Test resolve_terms.')

library(EMLassemblyline)

# Expect data frame -----------------------------------------------------------

list_of_terms <- c(
  'air temperature',
  'maximum temperature',
  'minimum temperature',
  'soil temperature')

testthat::test_that('Output should be a data frame', {
  
  output <- resolve_terms(
    x = list_of_terms,
    cv = 'lter'
    )
  
  expect_equal(
    class(
      output
    ),
    'data.frame'
  )
  
})

# Expect no resolution --------------------------------------------------------

list_of_terms <- c(
  'nonsense terms',
  'one eyed one horned flying purple people eater',
  '8he89ghgpo'
  )

testthat::test_that('No resolution expected', {
  
  output <- resolve_terms(
    x = list_of_terms,
    cv = 'lter'
  )
  
  use_i <- output$controlled_vocabulary == ''
  
  expect_equal(
    sum(
      use_i
    ),
    3
  )
  
})

