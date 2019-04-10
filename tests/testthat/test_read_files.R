
context('Read files into list')
library(EMLassemblyline)

# When no templates or files are present --------------------------------------

testthat::test_that('Expect standard structure', {
  
  # Parameterize
  
  output <- read_files(
    path = system.file(
      '/inst',
      package = 'EMLassemblyline'
    )
  )
  
  core_templates <- utils::read.table(
    file = system.file(
      '/templates/template_characteristics.txt',
      package = 'EMLassemblyline'
    ), 
    header = T,
    sep = '\t',
    as.is = T
  )
  
  core_templates <- core_templates[core_templates$core_template == TRUE, ]
  
  attr.args <- utils::read.table(
    file = system.file(
      '/templates/arguments.txt',
      package = 'EMLassemblyline'
    ), 
    header = T,
    sep = '\t',
    as.is = T
  )
  
  # Is list
  
  expect_equal(
    class(output), 
    'list'
  )
  
  # Has level-1 names
  
  expect_equal(
    all(
      c('template', 'data.table', 'other.entity', 'argument') %in% names(output)
    ),
    TRUE
  )
  
  # Has level-2 names
  
  expect_equal(
    all(
      names(output$template) %in% 
        core_templates$regexpr),
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
  
  expect_equal(
    all(
      names(output$argument) %in% 
        attr.args$argument_name),
    TRUE
  )
  
  # Has level-3 names
  
  for (i in 1:length(output$template)){
    
    expect_equal(
      all(
        names(output$template[[i]]) %in% 
          c('content', 'path')
      ),
      TRUE
    )
    
  }
  
  # Has level-3 values
  
  for (i in 1:length(output$template)){
    
    expect_equal(
      all(
        is.na(
          unname(
            unlist(
              output$template[[i]]
            )
          )
        )
      ),
      TRUE
    )
    
  }
  
  for (i in 1:length(output$argument)){
    
    expect_equal(
      is.null(
        output$argument[[i]]
      ),
      TRUE
    )
    
  }
  
})

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
      c('template', 'data.table', 'other.entity', 'argument') %in% 
        names(output)
    ),
    TRUE
  )
  
  # Has level-2 names
  
  expect_equal(
    all(
      names(output$template) %in% 
        path_files
    ),
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
  
  expect_equal(
    all(
      names(output$argument) %in% 
        attr.args$argument_name
    ),
    TRUE
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
      c('template', 'data.table', 'other.entity', 'argument') %in% 
        names(output)
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
      c('template', 'data.table', 'other.entity', 'argument') %in% names(output)
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
      c('template', 'data.table', 'other.entity', 'argument') %in% names(output)
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
  
  # data.table has data.path
  
  # For each data.table ...
  
  for (i in 1:length(names(output$data.table))){
    
    # path exists
    
    expect_equal(
      !is.na(output$data.table[[i]]$path),
      TRUE
    )

  }
  
  # other.entity has data.path
  
  # For each other.entity ...
  
  for (i in 1:length(names(output$other.entity))){
    
    # path exists
    
    expect_equal(
      !is.na(output$other.entity[[i]]$path),
      TRUE
    )
    
  }

})
