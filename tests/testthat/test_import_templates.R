context('Import templates')
library(EMLassemblyline)

# Errors ----------------------------------------------------------------------

testthat::test_that('Error when required arguments are missing', {
  
  # Path is missing
  
  expect_error(
    suppressMessages(
      import_templates(
        license = 'CC0',
        data.path = data.path
      )
    )
  )
  
  # License is missing
  
  expect_error(
    suppressMessages(
      import_templates(
        path = path,
        data.path = data.path
      )
    )
  )
  
  # Deprecated argument is in use
  
  expect_error(
    suppressMessages(
      import_templates(
        path = path,
        data.path = data.path,
        license = 'CC0',
        data.files = 'file1.csv'
      )
    )
  )
  
  # path is missing but x != NULL and write.file = T
  
  expect_error(
    suppressMessages(
      import_templates(
        x = read_files(
          path = system.file(
            '/inst',
            package = 'EMLassemblyline'
          )
        ),
        license = 'CC0',
        write.file = TRUE
      )
    )
  )
  
})

# Default usage (write files to path) -----------------------------------------

testthat::test_that('Default usage (write files to path)', {

  # Receive messages when write.file = FALSE
  
  expect_message(
    import_templates(
      path = system.file(
        '/examples',
        package = 'EMLassemblyline'
      ),
      license = 'CC0',
      data.path = system.file(
        '/examples/data',
        package = 'EMLassemblyline'
      ),
      data.table = c('decomp.csv', 'nitrogen.csv'),
      write.file = FALSE
    )
  )
  
  # Receive messages when templates already exist
  
  expect_message(
    import_templates(
      path = system.file(
        '/examples/templates',
        package = 'EMLassemblyline'
      ),
      license = 'CC0',
      data.path = system.file(
        '/examples/data',
        package = 'EMLassemblyline'
      ),
      data.table = c('decomp.csv', 'nitrogen.csv')
    )
  )
  
})

# Input x (No data.table or other.entity) -------------------------------------

testthat::test_that('x with no data, no path, and write.file = T', {
  
  # Expect error
  
  expect_error(
    suppressMessages(
      import_templates(
        x = read_files(
          path = system.file(
            '/inst', 
            package = 'EMLassemblyline'
          )
        ),
        license = 'CC0',
        write.file = TRUE
      )
    )
  )

})

testthat::test_that('x with no data, no path, and write.file = F', {
  
  # x with added templates and path = NA (already tested in test_read_files)

  expect_message(
    import_templates(
      x = read_files(
        path = system.file(
          '/inst', 
          package = 'EMLassemblyline'
        )
      ),
      license = 'CC0',
      write.file = FALSE
    )
  )
  
})

testthat::test_that('x with path, no data, and write.file = F', {
  
  # path is added to x but not written to file
  
  output <- import_templates(
    path = system.file(
      '/inst', 
      package = 'EMLassemblyline'
    ),
    x = read_files(
      path = system.file(
        '/inst', 
        package = 'EMLassemblyline'
      )
    ),
    license = 'CC0',
    write.file = FALSE
  )
  
  # For each template ...
  
  for (i in 1:length(output$template)){
    
    template_name <- names(output$template[i])
    
    # Exclude templates not created by import_templates() ...
    
    if ((template_name != 'taxonomicCoverage.xml') & 
        (template_name != 'geographic_coverage.txt')){
      
      # Path should have been added
      
      expect_equal(
        !is.na(output$template[[i]]$path),
        TRUE
      )
      
    }

  }
  
})

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
      license = 'CC0',
      write.file = FALSE
    )
  )

  output <- suppressMessages(
    import_templates(
      x = x_list,
      license = 'CC0',
      write.file = FALSE
    )
  )
  
  # Expect old content notifications
  
  expect_message(
    import_templates(
      x = output,
      license = 'CC0',
      write.file = FALSE
    )
  )
  
})

# Input x (2 data.tables) -----------------------------------------------------

# Two data.tables

testthat::test_that('Argument x has 2 data.tables and no other.entity', {
  
  # Initialize list
  
  x_list <- read_files(
    path = system.file(
      '/inst', 
      package = 'EMLassemblyline'
    ),
    data.path = system.file(
      '/examples/data',
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
      license = 'CC0',
      write.file = FALSE
    )
  )
  
  output <- suppressMessages(
    import_templates(
      path = system.file(
        '/inst',
        package = 'EMLassemblyline'
      ),
      x = x_list,
      license = 'CC0',
      write.file = FALSE
    )
  )
  
  # Expect old content notifications
  
  expect_message(
    import_templates(
      x = output,
      license = 'CC0',
      write.file = FALSE
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
        stringr::str_replace_all(
          names(output$data.table),
          '\\.[:alpha:]*$',
          '.txt'
        )
      ) %in% 
        names(output$template)),
    TRUE
  )
  
  # path is added to x but not written to file

  # For each template ...
  
  for (i in 1:length(output$template)){
    
    template_name <- names(output$template[i])
    
    # Exclude templates not created by import_templates() ...
    
    if ((template_name != 'taxonomicCoverage.xml') & 
        (template_name != 'geographic_coverage.txt')){
      
      # Path should have been added
      
      expect_equal(
        !is.na(output$template[[i]]$path),
        TRUE
      )
      
    }
    
  }
  
})
