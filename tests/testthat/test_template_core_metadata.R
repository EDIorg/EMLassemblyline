context('Create core metadata templates')
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

# File inputs = no data tables ------------------------------------------------

testthat::test_that('File inputs = no data tables', {
  
  # Missing path results in error
  
  expect_error(
    suppressMessages(
      template_core_metadata(
        license = 'CC0',
        write.file = FALSE
      )
    )
  )
  
  # Missing license results in error
  
  expect_error(
    suppressMessages(
      template_core_metadata(
        path = system.file(
          '/examples',
          package = 'EMLassemblyline'
        ),
        write.file = FALSE
      )
    )
  )
  
  # Unsupported license results in error
  
  expect_error(
    suppressMessages(
      template_core_metadata(
        path = system.file(
          '/examples',
          package = 'EMLassemblyline'
        ),
        license = 'CCzero',
        write.file = FALSE
      )
    )
  )
  
  # New imports result in messages
  
  expect_message(
    template_core_metadata(
      path = system.file(
        '/examples',
        package = 'EMLassemblyline'
      ),
      license = 'CC0',
      write.file = FALSE
    )
  )
  
  expect_message(
    template_core_metadata(
      path = system.file(
        '/examples',
        package = 'EMLassemblyline'
      ),
      license = 'CCBY',
      write.file = FALSE
    )
  )
  
  # Attempt to import templates when they already exist results in messages
  
  expect_message(
    template_core_metadata(
      path = system.file(
        '/examples/templates',
        package = 'EMLassemblyline'
      ),
      license = 'CC0',
      write.file = FALSE
    )
  )
  
  # write.file = TRUE writes files to path
  
  expect_message(
    template_core_metadata(
      path = tempdir(),
      license = 'CC0',
      write.file = TRUE
    )
  )
  
})

# x inputs = NULL -------------------------------------------------------------

testthat::test_that('x inputs = NULL', {
  
  # Make function call
  
  x <- make_arguments()
  
  x <- x$x
  
  # Missing path results in messages
  
  expect_message(
    template_core_metadata(
      license = 'CC0',
      x = x,
      write.file = FALSE
    )
  )
  
  # Missing path results in expected content classes with empty values
  
  output <- suppressMessages(
    template_core_metadata(
      license = 'CC0',
      x = x,
      write.file = FALSE
    )
  )
  
  for (i in 1:length(output$template)){
    
    expect_equal(
      class(output$template[[i]]$content)[1] %in% 
        c('TextType', 'data.frame', 'methods', 'character'),
      TRUE
    )
    
  }
  
  # Missing license results in error
  
  expect_error(
    suppressMessages(
      template_core_metadata(
        path = system.file(
          '/examples',
          package = 'EMLassemblyline'
        ),
        x = x,
        write.file = FALSE
      )
    )
  )
  
  # Unsupported license results in error
  
  expect_error(
    suppressMessages(
      template_core_metadata(
        path = system.file(
          '/examples',
          package = 'EMLassemblyline'
        ),
        x = x,
        license = 'CCzero',
        write.file = FALSE
      )
    )
  )
  
  # CCBY is a supported license
  
  expect_message(
    template_core_metadata(
      path = system.file(
        '/examples',
        package = 'EMLassemblyline'
      ),
      x = x,
      license = 'CCBY',
      write.file = FALSE
    )
  )
  
  # Valid path results in messages
  
  expect_message(
    template_core_metadata(
      path = system.file(
        '/examples',
        package = 'EMLassemblyline'
      ),
      x = x,
      license = 'CC0',
      write.file = FALSE
    )
  )
  
  # Valid path results in expected content classes with empty values
  
  output <- suppressMessages(
    template_core_metadata(
      path = system.file(
        '/examples',
        package = 'EMLassemblyline'
      ),
      license = 'CC0',
      x = x,
      write.file = FALSE
    )
  )
  
  for (i in 1:length(output$template)){
    
    expect_equal(
      class(output$template[[i]]$content)[1] %in% 
        c('TextType', 'data.frame', 'methods', 'character'),
      TRUE
    )
    
  }
  
  # Attempt to import templates when they already exist results in messages
  
  expect_message(
    template_core_metadata(
      path = system.file(
        '/examples/templates',
        package = 'EMLassemblyline'
      ),
      x = output,
      license = 'CC0',
      write.file = FALSE
    )
  )
  
})
