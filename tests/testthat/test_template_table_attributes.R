context('Create table attributes template')
library(EMLassemblyline)

# Parameterize ----------------------------------------------------------------

# Read template attributes

attr_templates <- utils::read.table(
  file = system.file(
    '/templates/template_characteristics.txt',
    package = 'EMLassemblyline'
  ), 
  header = T,
  sep = '\t',
  as.is = T
)

# File inputs = two data tables -----------------------------------------------

testthat::test_that('Test usage with file inputs', {
  
  # Missing path results in error
  
  expect_error(
    suppressMessages(
      template_table_attributes(
        data.path = system.file(
          '/examples/data',
          package = 'EMLassemblyline'
        ),
        data.table = c(
          'decomp.csv',
          'nitrogen.csv'
        ),
        write.file = FALSE
      )
    )
  )
  
  # Invalid data path results in error
  
  expect_error(
    suppressMessages(
      template_table_attributes(
        path = system.file(
          '/examples',
          package = 'EMLassemblyline'
        ),
        data.table = c(
          'decomp.csv',
          'nitrogen.csv'
        ),
        write.file = FALSE
      )
    )
  )
  
  # Invalid data tables result in error
  
  expect_error(
    suppressMessages(
      template_table_attributes(
        path = system.file(
          '/examples',
          package = 'EMLassemblyline'
        ),
        data.path = system.file(
          '/examples/data',
          package = 'EMLassemblyline'
        ),
        data.table = c(
          'decompppp.csv',
          'nitrogennnnn.csv'
        ),
        write.file = FALSE
      )
    )
  )
  
  
  # New imports result in messages
  
  expect_message(
    template_table_attributes(
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
      ),
      write.file = FALSE
    )
  )
  
  # Attempt to import templates when they already exist results in messages
  
  expect_message(
    template_table_attributes(
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
      ),
      write.file = FALSE
    )
  )
  
  # write.file = TRUE writes files to path
  
  file.remove(
    paste0(
      tempdir(),
      '/attributes_decomp.txt'
    )
  )
  
  file.remove(
    paste0(
      tempdir(),
      '/attributes_nitrogen.txt'
    )
  )
  
  file.remove(
    paste0(
      tempdir(),
      '/custom_units.txt'
    )
  )
  
  expect_message(
    template_table_attributes(
      path = tempdir(),
      data.path = system.file(
        '/examples/data',
        package = 'EMLassemblyline'
      ),
      data.table = c(
        'decomp.csv',
        'nitrogen.csv'
      ),
      write.file = TRUE
    )
  )
  
  expect_true(
    'custom_units.txt' %in% list.files(tempdir())
  )
  
})

# x inputs = data tables ------------------------------------------------------

testthat::test_that('x inputs = data tables', {
  
  # Make function call
  
  x <- template_arguments(
    data.path = system.file(
      '/examples/data',
      package = 'EMLassemblyline'
    ),
    data.table = c(
      'decomp.csv',
      'nitrogen.csv'
    )
  )
  
  x <- x$x
  
  # Missing path results in messages
  
  expect_message(
    template_table_attributes(
      x = x,
      write.file = FALSE
    )
  )
  
  # Missing path results in messages
  
  expect_message(
    template_table_attributes(
      data.path = system.file(
        '/examples/data',
        package = 'EMLassemblyline'
      ),
      x = x,
      write.file = FALSE
    )
  )
  
  # Missing data path results in messages
  
  expect_message(
    template_table_attributes(
      path = system.file(
        '/examples',
        package = 'EMLassemblyline'
      ),
      x = x,
      write.file = FALSE
    )
  )
  
  # Valid data path and data tables results in messages
  
  expect_message(
    template_table_attributes(
      data.path = system.file(
        '/examples/data',
        package = 'EMLassemblyline'
      ),
      data.table = c(
        'decomp.csv',
        'nitrogen.csv'
      ),
      x = x,
      write.file = FALSE
    )
  )
  
  # Valid data path and data tables result in addition of attributes templates
  # with expected names, class, column names, and nrows > 1. Custom units
  # template is also added.
  
  output <- suppressMessages(
    template_table_attributes(
      data.path = system.file(
        '/examples/data',
        package = 'EMLassemblyline'
      ),
      data.table = c(
        'decomp.csv',
        'nitrogen.csv'
      ),
      x = x,
      write.file = FALSE
    )
  )
  
  attr_names <- paste0(
    'attributes_',
    stringr::str_remove(
      string = names(output$data.table),
      pattern = '.csv'
    ),
    '.txt'
  )
  
  for (i in 1:length(attr_names)){
    
    expect_equal(
      attr_names[i] %in% names(output$template),
      TRUE
    )
    
    expect_equal(
      class(output$template[[attr_names[i]]]$content),
      'data.frame'
    )
    
    expect_equal(
      all(
        colnames(output$template[[attr_names[i]]]$content) %in%
          c('id', 'attributeName', 'attributeDefinition', 'class', 'unit', 
            'dateTimeFormatString', 'missingValueCode', 
            'missingValueCodeExplanation')
      ),
      TRUE
    )
    
    expect_equal(
      nrow(output$template[[attr_names[i]]]$content) > 1,
      TRUE
    )
    
  }
  
  expect_true(
    'custom_units.txt' %in% names(output$template)
  )
  
  # Attempt to import templates when they already exist results in messages
  
  expect_message(
    template_table_attributes(
      data.path = system.file(
        '/examples/data',
        package = 'EMLassemblyline'
      ),
      data.table = c(
        'decomp.csv',
        'nitrogen.csv'
      ),
      x = output,
      write.file = FALSE
    )
  )
  
  # Invalid column names result in error
  
  input <- x
  
  colnames(input$data.table$nitrogen.csv$content)[
    colnames(input$data.table$nitrogen.csv$content) == 'stem_mass_density'
  ] <- 'stem.mass.density'
  
  expect_error(
    suppressMessages(
      template_table_attributes(
        path = tempdir(),
        data.path = system.file(
          '/examples/data',
          package = 'EMLassemblyline'
        ),
        data.table = c(
          'decomp.csv',
          'nitrogen.csv'
        ),
        x = input,
        write.file = FALSE
      ) 
    )
  )
  
  # write.file = TRUE writes files to path
  
  expect_message(
    template_table_attributes(
      path = tempdir(),
      data.path = system.file(
        '/examples/data',
        package = 'EMLassemblyline'
      ),
      data.table = c(
        'decomp.csv',
        'nitrogen.csv'
      ),
      x = x,
      write.file = TRUE
    )
  )
  
})
