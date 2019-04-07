
context('Define categorical variables')
library(EMLassemblyline)

# Parameterize ----------------------------------------------------------------

path <- system.file('/examples/templates/abstract.txt', package = 'EMLassemblyline')
parent_dir <- substr(path, 1, nchar(path)-23)

# Arguments -------------------------------------------------------------------

testthat::test_that('Test arguments', {
  
  # Error: path is missing
  
  expect_error(
    define_catvars(
      data.path = system.file(
        '/examples/data',
        package = 'EMLassemblyline'
      )
    )
  )

  # Error: Attribute files are missing
  
  expect_error(
    suppressMessages(
      define_catvars(
        path = system.file(
          '/examples/data',
          package = 'EMLassemblyline'
        ),
        data.path = system.file(
          '/examples/data',
          package = 'EMLassemblyline'
        )
      )
    )
  )
  
  # Initialize x
  
  x_list <- read_files(
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
  
  # Error: path is present when using x
  
  expect_error(
    suppressMessages(
      define_catvars(
        path = system.file(
          '/examples/templates',
          package = 'EMLassemblyline'
        ),
        data.path = system.file(
          '/examples/data',
          package = 'EMLassemblyline'
        ),
        x = x_list,
        write.file = FALSE
      )
    )
  )
  
  # Error: data.path is present when using x
  
  expect_error(
    suppressMessages(
      define_catvars(
        data.path = system.file(
          '/examples/data',
          package = 'EMLassemblyline'
        ),
        x = x_list,
        write.file = FALSE
      )
    )
  )
  
  # Error: write.file is TRUE when using x
  
  expect_error(
    suppressMessages(
      define_catvars(
        x = x_list,
        write.file = TRUE
      )
    )
  )
  
  
})

# Default usage (file inputs) -------------------------------------------------

testthat::test_that('Test standard usage', {
  
  # catvars templates exist
  
  expect_message(
    define_catvars(
      path = system.file(
        '/examples/templates',
        package = 'EMLassemblyline'
      ),
      data.path = system.file(
        '/examples/data',
        package = 'EMLassemblyline'
      ),
      write.file = FALSE
    )
  )

})

# Input x (2 data.tables with categorical variables) --------------------------

testthat::test_that('Argument x has 2 data.tables with categorical variables',{
  
  # Initialize x
  
  x_list <- read_files(
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
  
  x_list$template$catvars_decomp.txt <- NULL
  
  x_list$template$catvars_nitrogen.txt <- NULL
  
  # Expect messages
  
  expect_message(
    define_catvars(
      x = x_list,
      write.file = FALSE
    )
  )
  
  # Expect addition of catvar templates
  
  output <- suppressMessages(
    define_catvars(
      x = x_list,
      write.file = FALSE
    )
  )
  
  catvars_files <- names(output$template)[stringr::str_detect(
    string = names(output$template),
    pattern = '^catvars_'
  )]
  
  expect_equal(
    length(catvars_files),
    2
  )
  
  for (i in 1:length(catvars_files)){
    
    expect_equal(
      class(output$template[[catvars_files[i]]]$content),
      'data.frame'
    )
    
    expect_equal(
      all(
        colnames(output$template[[catvars_files[i]]]$content) %in%
          c('attributeName', 'code', 'definition')
      ),
      TRUE
    )
    
  }

  
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
  
})

