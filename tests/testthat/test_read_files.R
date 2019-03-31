
context('Read files into list')
library(EMLassemblyline)

# When only templates are present ---------------------------------------------

testthat::test_that('Expect standard structure', {
  
  # Parameterize
  
  output <- read_files(
    path = system.file(
      '/examples/templates',
      package = 'EMLassemblyline'
    )
  )
  
  path_files <- list.files(
    system.file(
      '/examples/templates',
      package = 'EMLassemblyline'
    )
  )
  
  # Is list
  
  expect_equal(
    class(output), 
    'list'
  )
  
  # Has level-1 names
  
  expect_equal(
    all(
      c('template', 'data.table', 'other.entity') %in% names(output)
    ),
    TRUE
  )
  
  # Has level-2 names
  
  expect_equal(
    all(names(output$template) %in% path_files),
    TRUE
  )
  
  expect_equal(
    output$data.table,
    NULL
  )
  
  expect_equal(
    output$other.entity,
    NULL
  )
  
})

# When only templates and data tables are present -----------------------------

testthat::test_that('Expect standard structure', {
  
  # Parameterize
  
  output <- read_files(
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
  
  path_files <- list.files(
    system.file(
      '/examples/templates',
      package = 'EMLassemblyline'
    )
  )
  
  data_path_files <- list.files(
    system.file(
      '/examples/data',
      package = 'EMLassemblyline'
    )
  )
  
  # Is list
  
  expect_equal(
    class(output), 
    'list'
  )
  
  # Has level-1 names
  
  expect_equal(
    all(
      c('template', 'data.table', 'other.entity') %in% names(output)
    ),
    TRUE
  )
  
  # Has level-2 names
  
  expect_equal(
    all(names(output$template) %in% path_files),
    TRUE
  )
  
  expect_equal(
    all(names(output$data.table) %in% data_path_files),
    TRUE
  )
  
  expect_equal(
    names(output$other.entity),
    NULL
  )
  
})

# When only templates and other entities are present --------------------------

testthat::test_that('Expect standard structure', {
  
  # Parameterize
  
  output <- read_files(
    path = system.file(
      '/examples/templates',
      package = 'EMLassemblyline'
    ),
    data.path = system.file(
      '/examples/data',
      package = 'EMLassemblyline'
    ),
    other.entity = 'ancillary_data.zip'
  )
  
  path_files <- list.files(
    system.file(
      '/examples/templates',
      package = 'EMLassemblyline'
    )
  )
  
  data_path_files <- list.files(
    system.file(
      '/examples/data',
      package = 'EMLassemblyline'
    )
  )
  
  # Is list
  
  expect_equal(
    class(output), 
    'list'
  )
  
  # Has level-1 names
  
  expect_equal(
    all(
      c('template', 'data.table', 'other.entity') %in% names(output)
    ),
    TRUE
  )
  
  # Has level-2 names
  
  expect_equal(
    all(names(output$template) %in% path_files),
    TRUE
  )
  
  expect_equal(
    names(output$data.table),
    NULL
  )
  
  expect_equal(
    all(names(output$other.entity) %in% data_path_files),
    TRUE
  )
  
})

# When templates, data tables, and other entities are present -----------------

testthat::test_that('Expect standard structure', {
  
  # Parameterize
  
  output <- read_files(
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
    other.entity = 'ancillary_data.zip'
  )
  
  path_files <- list.files(
    system.file(
      '/examples/templates',
      package = 'EMLassemblyline'
    )
  )
  
  data_path_files <- list.files(
    system.file(
      '/examples/data',
      package = 'EMLassemblyline'
    )
  )
    
  # Is list
  
  expect_equal(
    class(output), 
    'list'
  )
  
  # Has level-1 names
  
  expect_equal(
    all(
      c('template', 'data.table', 'other.entity') %in% names(output)
    ),
    TRUE
  )
  
  # Has level-2 names
  
  expect_equal(
    all(names(output$template) %in% path_files),
    TRUE
  )
  
  expect_equal(
    all(names(output$data.table) %in% data_path_files),
    TRUE
  )
  
  expect_equal(
    all(names(output$other.entity) %in% data_path_files),
    TRUE
  )
  
  # Has level-3 names
  
  for (i in 1:length(names(output))){
    
    for (j in 1:length(names(output[[i]]))){
      
      expect_equal(
        all(names(output[[i]][[j]]) %in% c('content', 'path')),
        TRUE
      )
      
    }

  }
  
})
