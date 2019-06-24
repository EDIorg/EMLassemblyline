context('Create taxonomic coverage template')
library(EMLassemblyline)

# File inputs -----------------------------------------------------------------

testthat::test_that('File inputs', {
  
  # Standard usage results in messages, taxonomic_coverage.txt written to path,
  # and expected table attributes.
  
  expect_message(
    template_taxonomic_coverage(
      path = tempdir(),
      data.path = system.file(
        '/examples/data',
        package = 'EMLassemblyline'
      ),
      taxa.table = 'decomp.csv',
      taxa.col = 'taxa',
      taxa.name.type = 'both',
      taxa.authority = c(3, 11),
      write.file = TRUE
    ) 
  )
  
  input <- utils::read.table(
    paste0(
      tempdir(), 
      '/taxonomic_coverage.txt'
    ),
    header = T,
    sep="\t",
    quote="\"",
    as.is=TRUE,
    comment.char = "",
    fill = T,
    na.strings = "NA",
    fileEncoding = "UTF-8"
  )
  
  expect_equal(
    class(input),
    'data.frame'
  )
  
  expect_true(
    all(
      colnames(input) %in% 
        c('name', 
          'name_type', 
          'name_resolved',
          'authority_system',
          'authority_id')
    )
  )
  
  unlink(
    paste0(
      tempdir(),
      '/taxonomic_coverage.txt'
    ),
    force = TRUE
  )
  
  # Missing path results in error
  
  expect_error(
    suppressMessages(
      template_taxonomic_coverage(
        data.path = system.file(
          '/examples/data',
          package = 'EMLassemblyline'
        ),
        taxa.table = 'decomp.csv',
        taxa.col = 'taxa',
        taxa.name.type = 'scientific',
        taxa.authority = c(3, 11),
        write.file = FALSE
      ) 
    ) 
  )

  # Missing data.path results in error
  
  expect_error(
    suppressMessages(
      template_taxonomic_coverage(
        path = tempdir(),
        taxa.table = 'decomp.csv',
        taxa.col = 'taxa',
        taxa.name.type = 'scientific',
        taxa.authority = c(3, 11),
        write.file = FALSE
      ) 
    ) 
  )
  
  # Missing taxa.table results in error
  
  expect_error(
    suppressMessages(
      template_taxonomic_coverage(
        path = tempdir(),
        data.path = system.file(
          '/examples/data',
          package = 'EMLassemblyline'
        ),
        taxa.col = 'taxa',
        taxa.name.type = 'scientific',
        taxa.authority = c(3, 11),
        write.file = FALSE
      ) 
    ) 
  )
  
  # Incorrectly spelled taxa.table results in error
  
  expect_error(
    suppressMessages(
      template_taxonomic_coverage(
        path = tempdir(),
        data.path = system.file(
          '/examples/data',
          package = 'EMLassemblyline'
        ),
        taxa.table = 'decomppp.csv',
        taxa.col = 'taxa',
        taxa.name.type = 'scientific',
        taxa.authority = c(3, 11),
        write.file = FALSE
      ) 
    ) 
  )
  
  # Missing taxa.col results in error
  
  expect_error(
    suppressMessages(
      template_taxonomic_coverage(
        path = tempdir(),
        data.path = system.file(
          '/examples/data',
          package = 'EMLassemblyline'
        ),
        taxa.table = 'decomp.csv',
        taxa.name.type = 'scientific',
        taxa.authority = c(3, 11),
        write.file = FALSE
      ) 
    ) 
  )
  
  # Incorrectly spelled taxa.col results in error
  
  expect_error(
    suppressMessages(
      template_taxonomic_coverage(
        path = tempdir(),
        data.path = system.file(
          '/examples/data',
          package = 'EMLassemblyline'
        ),
        taxa.table = 'decomp.csv',
        taxa.col = 'taxaaa',
        taxa.name.type = 'scientific',
        taxa.authority = c(3, 11),
        write.file = FALSE
      ) 
    ) 
  )
  
  # Missing taxa.name.type results in error
  
  expect_error(
    suppressMessages(
      template_taxonomic_coverage(
        path = tempdir(),
        data.path = system.file(
          '/examples/data',
          package = 'EMLassemblyline'
        ),
        taxa.table = 'decomp.csv',
        taxa.col = 'taxa',
        taxa.authority = c(3, 11),
        write.file = FALSE
      ) 
    ) 
  )
  
  # Missing invalid taxa.name.type results in error
  
  expect_error(
    suppressMessages(
      template_taxonomic_coverage(
        path = tempdir(),
        data.path = system.file(
          '/examples/data',
          package = 'EMLassemblyline'
        ),
        taxa.table = 'decomp.csv',
        taxa.col = 'taxa',
        taxa.name.type = 'commontific',
        taxa.authority = c(3, 11),
        write.file = FALSE
      ) 
    ) 
  )
  
  # Invalid taxa.authority results in error
  
  expect_error(
    suppressMessages(
      template_taxonomic_coverage(
        path = tempdir(),
        data.path = system.file(
          '/examples/data',
          package = 'EMLassemblyline'
        ),
        taxa.table = 'decomp.csv',
        taxa.col = 'taxa',
        taxa.name.type = 'scientific',
        taxa.authority = 2112,
        write.file = FALSE
      ) 
    ) 
  )
  
  # Missing file extensions are supported
  
  unlink(
    paste0(
      tempdir(),
      '/taxonomic_coverage.txt'
    ),
    force = TRUE
  )
  
  expect_message(
    template_taxonomic_coverage(
      path = tempdir(),
      data.path = system.file(
        '/examples/data',
        package = 'EMLassemblyline'
      ),
      taxa.table = 'decomp',
      taxa.col = 'taxa',
      taxa.name.type = 'scientific',
      taxa.authority = c(3, 11),
      write.file = FALSE
    ) 
  )
  
})

# x inputs --------------------------------------------------------------------

testthat::test_that('x inputs', {
  
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
  
  # Standard usage results in messages, taxonomic_coverage.txt written to x,
  # and expected table attributes.

  output <- suppressMessages(
    template_taxonomic_coverage(
      taxa.table = 'decomp.csv',
      taxa.col = 'taxa',
      taxa.name.type = 'scientific',
      taxa.authority = c(3, 11),
      x = x
    ) 
  )
  
  expect_equal(
    class(output$template$taxonomic_coverage.txt$content),
    'data.frame'
  )
  
  expect_true(
    all(
      colnames(output$template$taxonomic_coverage.txt) %in% 
        c('name', 
          'name_type', 
          'name_resolved',
          'authority_system',
          'authority_id')
    )
  )

  # Missing taxa.table results in error
  
  expect_error(
    suppressMessages(
      template_taxonomic_coverage(
        path = tempdir(),
        data.path = system.file(
          '/examples/data',
          package = 'EMLassemblyline'
        ),
        taxa.col = 'taxa',
        taxa.name.type = 'scientific',
        taxa.authority = c(3, 11),
        x = output
      ) 
    ) 
  )
  
  # Incorrectly spelled taxa.table results in error
  
  expect_error(
    suppressMessages(
      template_taxonomic_coverage(
        path = tempdir(),
        data.path = system.file(
          '/examples/data',
          package = 'EMLassemblyline'
        ),
        taxa.table = 'decomppp.csv',
        taxa.col = 'taxa',
        taxa.name.type = 'scientific',
        taxa.authority = c(3, 11),
        x = output
      ) 
    ) 
  )
  
  # Missing taxa.col results in error
  
  expect_error(
    suppressMessages(
      template_taxonomic_coverage(
        path = tempdir(),
        data.path = system.file(
          '/examples/data',
          package = 'EMLassemblyline'
        ),
        taxa.table = 'decomp.csv',
        taxa.name.type = 'scientific',
        taxa.authority = c(3, 11),
        x = output
      ) 
    ) 
  )
  
  # Incorrectly spelled taxa.col results in error
  
  expect_error(
    suppressMessages(
      template_taxonomic_coverage(
        path = tempdir(),
        data.path = system.file(
          '/examples/data',
          package = 'EMLassemblyline'
        ),
        taxa.table = 'decomp.csv',
        taxa.col = 'taxaaa',
        taxa.name.type = 'scientific',
        taxa.authority = c(3, 11),
        x = output
      ) 
    ) 
  )
  
  # Missing taxa.name.type results in error
  
  expect_error(
    suppressMessages(
      template_taxonomic_coverage(
        path = tempdir(),
        data.path = system.file(
          '/examples/data',
          package = 'EMLassemblyline'
        ),
        taxa.table = 'decomp.csv',
        taxa.col = 'taxa',
        taxa.authority = c(3, 11),
        x = output
      ) 
    ) 
  )
  
  # Missing invalid taxa.name.type results in error
  
  expect_error(
    suppressMessages(
      template_taxonomic_coverage(
        path = tempdir(),
        data.path = system.file(
          '/examples/data',
          package = 'EMLassemblyline'
        ),
        taxa.table = 'decomp.csv',
        taxa.col = 'taxa',
        taxa.name.type = 'commontific',
        taxa.authority = c(3, 11),
        x = output
      ) 
    ) 
  )
  
  # Invalid taxa.authority results in error
  
  expect_error(
    suppressMessages(
      template_taxonomic_coverage(
        path = tempdir(),
        data.path = system.file(
          '/examples/data',
          package = 'EMLassemblyline'
        ),
        taxa.table = 'decomp.csv',
        taxa.col = 'taxa',
        taxa.name.type = 'scientific',
        taxa.authority = 2112,
        x = output
      ) 
    ) 
  )
  
})

# Multiple table inputs -------------------------------------------------------

testthat::test_that('Multiple table inputs', {
  
  # Create temporary directory for test
  
  dir.create(
    paste0(tempdir(), '/pkg')
  )
  
  # Add 2 tables containing taxonomic information (2 copies of the same table)
  
  file.copy(
    from = system.file('/examples/data/decomp.csv', package = 'EMLassemblyline'),
    to = paste0(tempdir(), '/pkg')
  )
  
  file.copy(
    from = system.file('/examples/data/decomp.csv', package = 'EMLassemblyline'),
    to = paste0(tempdir(), '/pkg/decomp2.csv')
  )
  
  # Call template_taxonomic_coverage()
  # Error if length(taxa.col) != length(taxa.table)
  
  expect_error(
    suppressMessages(
      template_taxonomic_coverage(
        path = paste0(tempdir(), '/pkg'),
        data.path = paste0(tempdir(), '/pkg'),
        taxa.table = c('decomp.csv', 'decomp2.csv'),
        taxa.col = 'taxa',
        taxa.name.type = 'scientific',
        taxa.authority = 3
      ) 
    )
  )
  
  expect_error(
    suppressMessages(
      template_taxonomic_coverage(
        path = paste0(tempdir(), '/pkg'),
        data.path = paste0(tempdir(), '/pkg'),
        taxa.table = 'decomp.csv',
        taxa.col = c('taxa', 'taxa'),
        taxa.name.type = 'scientific',
        taxa.authority = 3
      ) 
    )
  )
  
  # Call template_taxonomic_coverage()
  # Correct usage outputs a valid taxonomic_coverage.txt template
  
  expect_message(
    template_taxonomic_coverage(
      path = paste0(tempdir(), '/pkg'),
      data.path = paste0(tempdir(), '/pkg'),
      taxa.table = c('decomp.csv', 'decomp2.csv'),
      taxa.col = c('taxa', 'taxa'),
      taxa.name.type = 'scientific',
      taxa.authority = 3
    )
  )
  
  expect_true(
    file.exists(paste0(tempdir(), '/pkg/taxonomic_coverage.txt'))
  )
  
  output <- utils::read.table(
    file = paste0(tempdir(), '/pkg/taxonomic_coverage.txt'),
    header = T,
    sep="\t",
    quote="\"",
    as.is=TRUE,
    comment.char = "",
    fill = T,
    na.strings = "NA",
    fileEncoding = "UTF-8"
  )
  
  expect_equal(
    class(output),
    'data.frame'
  )
  
  # Clean up
  
  unlink(
    paste0(tempdir(), '/pkg'),
    recursive = TRUE,
    force = TRUE
  )
  
})