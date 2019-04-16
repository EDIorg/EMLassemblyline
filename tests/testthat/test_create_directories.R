
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

# testthat::test_that('Directories do not exist', {
#   
#   # Mock these tests
#   
#   expect_error(
#     suppressMessages(
#       create_directories(
#         path = system.file(
#           '/examples',
#           package = 'EMLassemblyline'
#         ),
#         dir.name = 'templates'
#       ) 
#     )
#   )
#   
# })
