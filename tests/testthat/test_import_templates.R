context('Import templates')
library(EMLassemblyline)

# Parameterize ----------------------------------------------------------------

# Get template file attributes

attr.templates <- utils::read.table(
  file = system.file(
    '/templates/template_characteristics.txt',
    package = 'EMLassemblyline'
  ), 
  header = T,
  sep = '\t',
  as.is = T
)

# Test usage with file inputs -------------------------------------------------

testthat::test_that('Test usage with file inputs', {
  
  # Correct usage results in messages (templates don't yet exist)
  
  expect_message(
    import_templates(
      path = system.file(
        '/examples',
        package = 'EMLassemblyline'
      ),
      data.path = system.file(
        '/examples/data',
        package = 'EMLassemblyline'
      ),
      license = 'CC0',
      data.table = c(
        'nitrogen.csv',
        'decomp.csv'
      ),
      write.file = FALSE
    )
  )
  
  # Correct usage results in messages (templates already exist)
  
  expect_message(
    import_templates(
      path = system.file(
        '/examples/templates',
        package = 'EMLassemblyline'
      ),
      data.path = system.file(
        '/examples/data',
        package = 'EMLassemblyline'
      ),
      license = 'CC0',
      data.table = c(
        'nitrogen.csv',
        'decomp.csv'
      ),
      write.file = FALSE
    )
  )
  
  # Missing path results in error
  
  expect_error(
    suppressMessages(
      import_templates(
        data.path = system.file(
          '/examples/data',
          package = 'EMLassemblyline'
        ),
        license = 'CC0',
        data.table = c(
          'nitrogen.csv',
          'decomp.csv'
        ),
        write.file = FALSE
      )
    )
  )
  
  # Missing data.path with data.table results in error
  
  expect_error(
    suppressMessages(
      import_templates(
        path = system.file(
          '/examples',
          package = 'EMLassemblyline'
        ),
        license = 'CC0',
        data.table = c(
          'nitrogen.csv',
          'decomp.csv'
        ),
        write.file = FALSE
      )
    )
  )
  
  # Missing data.path and data.table result in message
  
  expect_message(
    import_templates(
      path = system.file(
        '/examples',
        package = 'EMLassemblyline'
      ),
      license = 'CC0',
      write.file = FALSE
    )
  )
  
  # Missing license results in error
  
  expect_error(
    suppressMessages(
      import_templates(
        path = system.file(
          '/examples',
          package = 'EMLassemblyline'
        ),
        data.path = system.file(
          '/examples/data',
          package = 'EMLassemblyline'
        ),
        data.table = c(
          'nitrogen.csv',
          'decomp.csv'
        ),
        write.file = FALSE
      )
    )
  )
  
})

# Test usage with x inputs (empty x) ------------------------------------------

testthat::test_that('Test usage with x inputs (empty x)', {
  
  # Create x with no templates or data

  x_empty <- read_files(
    path = system.file(
      '/examples', 
      package = 'EMLassemblyline'
    )
  )

  # First import results in messages
  
  expect_message(
    import_templates(
      license = 'CC0',
      x = x_empty,
      write.file = FALSE
    )
  )
  
  # All arguments are supported:
  # - /x/template/* is populated with template
  # - content and paths are present
  # - data.table and other.entity are NULL
  
  x_empty <- suppressMessages(
    expect_message(
      import_templates(
        license = 'CC0',
        x = x_empty,
        write.file = FALSE
      )
    )
  )
  
  for (i in 1:length(x_empty$template)){
    
    expect_equal(
      any(names(x_empty$template[i]) %in% attr.templates$regexpr),
      TRUE
    )
    
    if (!names(x_empty$template[i]) %in% 
        c('taxonomicCoverage.xml',
          'geographic_coverage.txt')){
      
      expect_equal(
        all(!is.na(x_empty$template[[i]]$content)),
        TRUE
      )
      
    }
    
    expect_equal(
      is.na(x_empty$template[[i]]$path),
      TRUE
    )

  }
  
  # Second import notifies of existing content

})

# Test usage with x inputs (half empty x, no templates but 2 data.table) ------

testthat::test_that('Test usage with x inputs (half empty x, no templates but 2 data.table)', {
  
  # Create x with no template but 2 data.table

  x_half_empty <- read_files(
    path = system.file(
      '/examples', 
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

  
})

# Test usage with x inputs (full x, all templates and 2 data.table) -----------

testthat::test_that('Test usage with x inputs (full x, all templates and 2 data.table)', {
  
  # Create x with full templates and 2 data.table
  
  x_full <- read_files(
    path = system.file(
      '/examples/templates', 
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


# -------------------------------------

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
