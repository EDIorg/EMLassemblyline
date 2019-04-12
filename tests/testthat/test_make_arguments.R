
context('Make arguments')
library(EMLassemblyline)

# Parameterize ----------------------------------------------------------------

# Read argument attributes

attr_args <- utils::read.table(
  file = system.file(
    '/templates/arguments.txt',
    package = 'EMLassemblyline'
  ), 
  header = T,
  sep = '\t',
  as.is = T
)

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

# NULL inputs -------------------------------------------------------------

testthat::test_that('NULL inputs', {
  
  # Make function call
  
  output <- make_arguments()
  
  # Class is list
  
  expect_equal(
    class(output), 
    'list'
  )
  
  # Level-1 has argument names
  
  expect_equal(
    all(
      names(output) %in% attr_args$argument_name
    ),
    TRUE
  )
  
  # Level-2 has templates, data tables, and other entities
  
  expect_equal(
    all(
      names(output$x) %in% c('template', 'data.table', 'other.entity')
    ),
    TRUE
  )
  
  expect_equal(
    output$x$data.table,
    NULL
  )
  
  expect_equal(
    output$x$other.entity,
    NULL
  )
  
  # Level-3 has core templates
  
  expect_equal(
    all(
      names(output$x$template) %in% attr_templates$regexpr
    ),
    TRUE
  )
  
  # Level-4 has content

  for (i in 1:length(output$x$template)){
    
    expect_equal(
      all(
        names(output$x$template[[i]]) %in% 'content'
      ),
      TRUE
    )
    
  }
  
  for (i in 1:length(output$x$template)){
    
    expect_equal(
      is.na(output$x$template[[i]]$content),
      TRUE
    )
    
  }
  
})

# Template inputs -------------------------------------------------------------

testthat::test_that('Template inputs', {
  
  # Make function call
  
  output <- make_arguments(
    path = system.file(
      '/examples/templates',
      package = 'EMLassemblyline'
    )
  )
  
  # Class is list
  
  expect_equal(
    class(output), 
    'list'
  )
  
  # Level-1 has argument names
  
  expect_equal(
    all(
      names(output) %in% attr_args$argument_name
    ),
    TRUE
  )
  
  # Level-2 has templates, data tables, and other entities
  
  expect_equal(
    all(
      names(output$x) %in% c('template', 'data.table', 'other.entity')
    ),
    TRUE
  )
  
  expect_equal(
    output$x$data.table,
    NULL
  )
  
  expect_equal(
    output$x$other.entity,
    NULL
  )
  
  # Level-3 has all template possiblities
  
  for (i in 1:length(attr_templates$regexpr)){
    
    expect_equal(
      any(
        stringr::str_detect(
          string = names(output$x$template),
          pattern = attr_templates$regexpr[i]
        )
      ),
      TRUE
    )

  }
  
  # Level-4 has content
  
  for (i in 1:length(output$x$template)){
    
    expect_equal(
      all(
        names(output$x$template[[i]]) %in% 'content'
      ),
      TRUE
    )
    
  }
  
  for (i in 1:length(output$x$template)){

    expect_equal(
      class(output$x$template[[i]]$content)[1] %in% attr_templates$class,
      TRUE
    )
    
  }
  
})

# When only templates and data tables are present -----------------------------

testthat::test_that('Expect standard structure', {
  
  # Parameterize
  
  output <- make_arguments(
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
  
  output <- make_arguments(
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
  
  output <- make_arguments(
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
