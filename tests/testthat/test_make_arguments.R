
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

# List files at path

path_files <- list.files(
  system.file(
    '/examples/templates',
    package = 'EMLassemblyline'
  )
)

# List files at data.path

data_path_files <- list.files(
  system.file(
    '/examples/data',
    package = 'EMLassemblyline'
  )
)

# Inputs = NULL ---------------------------------------------------------------

testthat::test_that('Inputs = NULL', {
  
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

# Inputs = templates ----------------------------------------------------------

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

# Inputs = templates and data tables ------------------------------------------

testthat::test_that('Inputs = templates and data tables', {
  
  # Make function call
  
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
    !is.null(output$x$data.table),
    TRUE
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
  
  # Level-3 has data table names found at data.path
  
  expect_equal(
    all(
      names(output$x$data.table) %in% data_path_files
    ),
    TRUE
  )
  
  # Level-4 has template content
  
  for (i in 1:length(output$x$template)){
    
    expect_equal(
      all(
        names(output$x$template[[i]]) %in% 'content'
      ),
      TRUE
    )
    
    expect_equal(
      class(output$x$template[[i]]$content)[1] %in% attr_templates$class,
      TRUE
    )
    
  }
  
  # Level-4 has data table content
  
  for (i in 1:length(output$x$data.table)){
    
    expect_equal(
      all(
        names(output$x$data.table[[i]]) %in% 'content'
      ),
      TRUE
    )
    
    expect_equal(
      class(
        output$x$data.table[[i]]$content
      ),
      'data.frame'
    )
    
  }
  
  # Use of the sep argument helps read in data tables
  
  expect_equal(
    class(
      make_arguments(
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
        sep = ','
      ) 
    ),
    'list'
  )
  
})

# Inputs = templates and other entities ---------------------------------------

testthat::test_that('Inputs = templates and other entities', {
  
  # Make function call
  
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
    is.null(output$x$data.table),
    TRUE
  )
  
  expect_equal(
    !is.null(output$x$other.entity),
    TRUE
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
  
  # Level-3 has other entity names found at data.path
  
  expect_equal(
    all(
      names(output$x$other.entity) %in% data_path_files
    ),
    TRUE
  )
  
  # Level-4 has template content
  
  for (i in 1:length(output$x$template)){
    
    expect_equal(
      all(
        names(output$x$template[[i]]) %in% 'content'
      ),
      TRUE
    )
    
    expect_equal(
      class(output$x$template[[i]]$content)[1] %in% attr_templates$class,
      TRUE
    )
    
  }
  
  # Level-4 has other entity content
  
  for (i in 1:length(output$x$other.entity)){
    
    expect_equal(
      all(
        names(output$x$other.entity[[i]]) %in% 'content'
      ),
      TRUE
    )
    
    expect_equal(
      is.na(
        output$x$other.entity[[i]]$content
      ),
      TRUE
    )
    
  }
  
})

# Inputs = templates, data tables, and other entities -------------------------

testthat::test_that('Inputs = templates, data tables, and other entities', {
  
  # Make function call
  
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
    !is.null(output$x$data.table),
    TRUE
  )
  
  expect_equal(
    !is.null(output$x$other.entity),
    TRUE
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
  
  # Level-3 has data table names found at data.path
  
  expect_equal(
    all(
      names(output$x$data.table) %in% data_path_files
    ),
    TRUE
  )
  
  # Level-3 has other entity names found at data.path
  
  expect_equal(
    all(
      names(output$x$other.entity) %in% data_path_files
    ),
    TRUE
  )
  
  # Level-4 has template content
  
  for (i in 1:length(output$x$template)){
    
    expect_equal(
      all(
        names(output$x$template[[i]]) %in% 'content'
      ),
      TRUE
    )
    
    expect_equal(
      class(output$x$template[[i]]$content)[1] %in% attr_templates$class,
      TRUE
    )
    
  }
  
  # Level-4 has data table content
  
  for (i in 1:length(output$x$data.table)){
    
    expect_equal(
      all(
        names(output$x$data.table[[i]]) %in% 'content'
      ),
      TRUE
    )
    
    expect_equal(
      class(
        output$x$data.table[[i]]$content
      ),
      'data.frame'
    )
    
  }
  
  # Level-4 has other entity content
  
  for (i in 1:length(output$x$other.entity)){
    
    expect_equal(
      all(
        names(output$x$other.entity[[i]]) %in% 'content'
      ),
      TRUE
    )
    
    expect_equal(
      is.na(
        output$x$other.entity[[i]]$content
      ),
      TRUE
    )
    
  }
  
})
