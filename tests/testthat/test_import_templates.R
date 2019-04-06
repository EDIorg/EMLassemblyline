context('Import templates')
library(EMLassemblyline)

# Parameterize ----------------------------------------------------------------

# Set paths

path <- system.file(
  '/examples/templates/abstract.txt', 
  package = 'EMLassemblyline'
)
path <- substr(path, 1, nchar(path) - 23)

data.path <- paste0(path, '/data')

path <- paste0(path, '/templates')

# Errors ----------------------------------------------------------------------

testthat::test_that('Error out when required arguments are missing', {
  
  expect_error(
    suppressMessages(
      import_templates(
        license = 'CC0',
        data.path = data.path
      )
    )
  )
  
  expect_error(
    suppressMessages(
      import_templates(
        path = path,
        data.path = data.path
      )
    )
  )
  
})

# Messages --------------------------------------------------------------------

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

# Input x ---------------------------------------------------------------------

# No data.table or other.entity

testthat::test_that('Argument x does not have data.table or other.entity', {
  
  # Initialize list
  
  x_list <- read_files(
    path = system.file(
      '/inst', 
      package = 'EMLassemblyline'
    )
  )
  
  # Expect new content notifications
  
  expect_message(
    import_templates(
      x = x_list,
      license = 'CC0'
    )
  )

  output <- suppressMessages(
    import_templates(
      x = x_list,
      license = 'CC0'
    )
  )
  
  # Expect old content notifications
  
  expect_message(
    import_templates(
      x = output,
      license = 'CC0'
    )
  )
  
})

# Two data.tables

testthat::test_that('Argument x has 2 data.tables and no other.entity', {
  
  # Initialize list
  
  x_list <- read_files(
    path = system.file(
      '/inst', 
      package = 'EMLassemblyline'
    ),
    data.path = system.file(
      '/inst/examples/data',
      package = 'EMLassemblyline'
    ),
    data.table = c(
      'decomp.csv',
      'nitrogen.csv'
    )
  )

  # Expect new content notifications

  expect_message(
    import_templates(
      x = x_list,
      license = 'CC0'
    )
  )
  
  output <- suppressMessages(
    import_templates(
      x = x_list,
      license = 'CC0'
    )
  )
  
  # Expect old content notifications
  
  expect_message(
    import_templates(
      x = output,
      license = 'CC0'
    )
  )
  
  # Expect addition of attributes
  
  # Has level-1 names
  
  expect_equal(
    all(
      c('template', 'data.table', 'other.entity') %in% 
        names(output)
    ),
    TRUE
  )
  
  # Level-2 has attribute files for each data.table
  
  expect_equal(
    all(
      paste0(
        'attributes_',
        names(output$data.table),
        '.txt'
      ) %in% 
        names(output$template)),
    TRUE
  )
  
})







