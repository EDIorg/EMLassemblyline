
context('Define categorical variables')
library(EMLassemblyline)

# Test usage with file inputs -------------------------------------------------

testthat::test_that('Test usage with file inputs', {
  
  # Correct argument use results in messages
  
  expect_message(
    suppressWarnings(
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
  )
  
  # Correct argument use results in deprecation warning
  
  expect_warning(
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
  
  # Missing path results in error

  expect_error(
    suppressMessages(
      suppressWarnings(
        define_catvars(
          data.path = system.file(
            '/examples/data',
            package = 'EMLassemblyline'
          ),
          write.file = FALSE
        ) 
      )
    )
  )

  # Missing attribute files from path
  
  expect_error(
    suppressMessages(
      suppressWarnings(
        define_catvars(
          path = system.file(
            '/examples/data',
            package = 'EMLassemblyline'
          ),
          data.path = system.file(
            '/examples/data',
            package = 'EMLassemblyline'
          ),
          write.file = FALSE
        ) 
      )
    )
  )

})

# Test usage with x inputs ----------------------------------------------------

testthat::test_that('Test usage with x inputs',{

  # Create x_list and x_no_catvars with and without catvars_*.txt, respectivly.
  
  x_list <- make_arguments(
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
  
  x_list <- x_list$x
  
  x_no_catvars <- x_list
  
  x_no_catvars$template$catvars_decomp.txt <- NULL
  
  x_no_catvars$template$catvars_nitrogen.txt <- NULL
  
  # All arguments are supported:
  # - /x/templates/catvars_*.txt is created with expected content
  # - path is not added /x/templates/catvars_*.txt/path
  # - data.path is not added to x
  
  expect_message(
    suppressWarnings(
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
  
  output <- suppressMessages(
    suppressWarnings(
      define_catvars(
        path = '/some/path',
        data.path = system.file(
          '/examples/data',
          package = 'EMLassemblyline'
        ),
        x = x_no_catvars,
        write.file = FALSE
      ) 
    )
  )
  
  expect_equal(
    sum(
      stringr::str_detect(
        string = names(output$template),
        pattern = 'catvars_[:graph:]*.txt'
      )
    ),
    2
  )
  
  expect_equal(
    class(output$template$catvars_decomp.txt$content),
    'data.frame'
  )
  
  expect_equal(
    all(
      colnames(output$template$catvars_decomp.txt$content) %in%
        c('attributeName', 'code', 'definition')
    ),
    TRUE
  )
  
  # Correct argument use results in deprecation warning
  
  expect_warning(
    define_catvars(
      path = system.file(
        '/examples/templates',
        package = 'EMLassemblyline'
      ), 
      data.path = system.file(
        '/examples/data',
        package = 'EMLassemblyline'
      ),
      x = x_no_catvars,
      write.file = FALSE
    ) 
  )
  
  # Missing path adds NA to /x/templates/catvars_*.txt/path
  
  expect_message(
    suppressWarnings(
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
  
  output <- suppressMessages(
    suppressWarnings(
      define_catvars(
        data.path = system.file(
          '/examples/data',
          package = 'EMLassemblyline'
        ),
        x = x_no_catvars,
        write.file = FALSE
      ) 
    )
  )
  
  # Missing path has no effect
  
  expect_message(
    suppressWarnings(
      define_catvars(
        data.path = system.file(
          '/examples/data',
          package = 'EMLassemblyline'
        ),
        x = x_no_catvars,
        write.file = FALSE
      ) 
    )
  )
  
  # Missing path and data.path has no effect
  # - /x/templates/catvars_*.txt is created with expected content
  
  expect_error(
    suppressMessages(
      suppressWarnings(
        define_catvars(
          x = x_no_catvars,
          write.file = FALSE
        ) 
      )
    )
  )
  
})
