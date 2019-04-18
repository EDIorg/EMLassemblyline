
context('Create directories')
library(EMLassemblyline)

# Directories exist -----------------------------------------------------------

testthat::test_that('Directories exist', {
  
  # Existing directories result in error
  
  expect_error(
    suppressMessages(
      create_directories(
        path = system.file(
          '/examples',
          package = 'EMLassemblyline'
        ),
        dir.name = 'templates'
      ) 
    )
  )
  
})

# Directories don't exist -----------------------------------------------------

testthat::test_that("Directories don't exist", {

  # Normal use results in directory structure

  suppressMessages(
    create_directories(
      path = tempdir(),
      dir.name = 'edi_101'
    ) 
  )
  
  expect_true(
    dir.exists(
      paste0(
        tempdir(),
        '/edi_101'
      )
    )
  )

})
