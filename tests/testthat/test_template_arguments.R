
context('Template arguments')
library(EMLassemblyline)

# Parameterize ----------------------------------------------------------------

# Read attributes of EMLassemblyline function arguments and templates

attr_args <- data.table::fread(
  file = system.file(
    '/templates/arguments.txt',
    package = 'EMLassemblyline'),
  fill = TRUE,
  blank.lines.skip = TRUE)

attr_templates <- data.table::fread(
  system.file(
    '/templates/template_characteristics.txt',
    package = 'EMLassemblyline'), 
  fill = TRUE,
  blank.lines.skip = TRUE)

# List files at path and data.path

path_files <- list.files(
  system.file(
    '/examples/templates',
    package = 'EMLassemblyline'))

data_path_files <- list.files(
  system.file(
    '/examples/data',
    package = 'EMLassemblyline'))

# Inputs = NULL ---------------------------------------------------------------
# NULL inputs should assign NULL values to arguments names and the template 
# node.

testthat::test_that("Inputs = NULL", {
  
  output <- template_arguments()
  expect_true(class(output) == "list")
  expect_true(all(names(output) %in% attr_args$argument_name))
  expect_true(
    all(names(output$x) %in% c('template', 'data.table', 'other.entity')))
  expect_true(is.null(output$x$template))
  expect_true(is.null(output$x$data.table))
  expect_true(is.null(output$x$other.entity))

})

# Inputs = missing templates --------------------------------------------------
# A path without templates results in error.

testthat::test_that("Inputs = missing templates", {
  
  dir.create(paste0(tempdir(), "/empty"))
  file.create(paste0(tempdir(), "/empty/my.csv"))
  expect_error(template_arguments(path = paste0(tempdir(), "/empty")))
  unlink(paste0(tempdir(), "/empty/my.csv"))
  expect_error(template_arguments(path = paste0(tempdir(), "/empty")))
  unlink(paste0(tempdir(), "/empty"), force = T, recursive = T)

})

# Inputs = empty templates ----------------------------------------------------
# Empty templates should be read into x

testthat::test_that("Inputs = empty templates", {
  
  file.copy(
    from  = system.file('/templates', package = 'EMLassemblyline'),
    to = tempdir(),
    recursive = T)
  output <- template_arguments(path = paste0(tempdir(), "/templates"))
  expect_true(class(output) == "list")
  expect_true(all(names(output) %in% attr_args$argument_name))
  expect_true(
    all(names(output$x) %in% c('template', 'data.table', 'other.entity')))
  # FIXME: Continue here ...
  for (i in 1:length(names(output$x$template))) {
    if (stringr::str_detect(names(output$x$template[i]), "abstract|methods|additional_info|intellectual_rights")) {
      expect_true(is.list(output$x$template[[i]]$content))
    } else if (stringr::str_detect(names(output$x$template[i]), "attributes, catvars")) {
      
    }
    
  }
  
  # expect_true(is.null(output$x$template))
  expect_true(is.null(output$x$data.table))
  expect_true(is.null(output$x$other.entity))
  
  
  
  unlink(paste0(tempdir(), "/templates"), force = T, recursive = T)
  
  
  
  
})

# Inputs = templates (.txt) ---------------------------------------------------

testthat::test_that('Template inputs (.txt)', {
  
  # Make function call
  
  output <- template_arguments(
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
  
  # New geographic_coverage.txt is supported
  
  expect_equal(
    class(
      template_arguments(
        path = system.file(
          '/examples/templates_new_geocoverage',
          package = 'EMLassemblyline'
        )
      )
    ),
    'list'
  )
  
})

# Inputs = templates (docx) ---------------------------------------------------

testthat::test_that('Template inputs (docx)', {
  
  # Make function call
  
  output <- template_arguments(
    path = system.file(
      '/examples/templates_docx',
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
      class(output$x$template[[i]]$content)[1] %in% c(attr_templates$class, 'character'),
      TRUE
    )
    
  }
  
  # New geographic_coverage.txt is supported
  
  expect_equal(
    class(
      template_arguments(
        path = system.file(
          '/examples/templates_new_geocoverage',
          package = 'EMLassemblyline'
        )
      )
    ),
    'list'
  )
  
})

# Inputs = templates (md) -----------------------------------------------------

testthat::test_that('Template inputs (md)', {
  
  # Make function call
  
  output <- template_arguments(
    path = system.file(
      '/examples/templates_md',
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
      class(output$x$template[[i]]$content)[1] %in% c(attr_templates$class, 'character'),
      TRUE
    )
    
  }
  
  # New geographic_coverage.txt is supported
  
  expect_equal(
    class(
      template_arguments(
        path = system.file(
          '/examples/templates_new_geocoverage',
          package = 'EMLassemblyline'
        )
      )
    ),
    'list'
  )
  
})

# Inputs = templates (.txt) and data tables -----------------------------------

testthat::test_that('Inputs = templates (.txt) and data tables', {
  
  # Make function call
  
  output <- template_arguments(
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
      template_arguments(
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

# Inputs = templates (docx) and data tables -----------------------------------

testthat::test_that('Inputs = templates (docx) and data tables', {
  
  # Make function call
  
  output <- template_arguments(
    path = system.file(
      '/examples/templates_docx',
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
      class(output$x$template[[i]]$content)[1] %in% c(attr_templates$class, 'character'),
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
      template_arguments(
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

# Inputs = templates (md) and data tables -------------------------------------

testthat::test_that('Inputs = templates (md) and data tables', {
  
  # Make function call
  
  output <- template_arguments(
    path = system.file(
      '/examples/templates_md',
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
      class(output$x$template[[i]]$content)[1] %in% c(attr_templates$class, 'character'),
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
      template_arguments(
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

# Inputs = templates (.txt) and other entities --------------------------------

testthat::test_that('Inputs = templates (.txt) and other entities', {
  
  # Make function call
  
  output <- template_arguments(
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

# Inputs = templates (docx) and other entities --------------------------------

testthat::test_that('Inputs = templates (docx) and other entities', {
  
  # Make function call
  
  output <- template_arguments(
    path = system.file(
      '/examples/templates_docx',
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
      class(output$x$template[[i]]$content)[1] %in% c(attr_templates$class, 'character'),
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

# Inputs = templates (md) and other entities ---------------------------------

testthat::test_that('Inputs = templates (md) and other entities', {
  
  # Make function call
  
  output <- template_arguments(
    path = system.file(
      '/examples/templates_md',
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
      class(output$x$template[[i]]$content)[1] %in% c(attr_templates$class, 'character'),
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

# Inputs = templates (.txt), data tables, and other entities ------------------

testthat::test_that('Inputs = templates (.txt), data tables, and other entities', {
  
  # Make function call
  
  output <- template_arguments(
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

# Inputs = templates (docx), data tables, and other entities -----------------

testthat::test_that('Inputs = templates (docx), data tables, and other entities', {
  
  # Make function call
  
  output <- template_arguments(
    path = system.file(
      '/examples/templates_docx',
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
  
  expect_true(
    !is.null(output$x$other.entity)
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
      class(output$x$template[[i]]$content)[1] %in% c(attr_templates$class, 'character'),
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
  
})

# Inputs = templates (md), data tables, and other entities --------------------

testthat::test_that('Inputs = templates (md), data tables, and other entities', {
  
  expect_equal(
    class(
      template_arguments(
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

# Inputs = templates (md) and data tables -------------------------------------

testthat::test_that('Inputs = templates (md) and data tables', {
  
  # Make function call
  
  output <- template_arguments(
    path = system.file(
      '/examples/templates_md',
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
      class(output$x$template[[i]]$content)[1] %in% c(attr_templates$class, 'character'),
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
      template_arguments(
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

# Inputs = templates (.txt) and other entities --------------------------------

testthat::test_that('Inputs = templates (.txt) and other entities', {
  
  # Make function call
  
  output <- template_arguments(
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

# Inputs = templates (docx) and other entities --------------------------------

testthat::test_that('Inputs = templates (docx) and other entities', {
  
  # Make function call
  
  output <- template_arguments(
    path = system.file(
      '/examples/templates_docx',
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
      class(output$x$template[[i]]$content)[1] %in% c(attr_templates$class, 'character'),
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

# Inputs = templates (md) and other entities ---------------------------------

testthat::test_that('Inputs = templates (md) and other entities', {
  
  # Make function call
  
  output <- template_arguments(
    path = system.file(
      '/examples/templates_md',
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
      class(output$x$template[[i]]$content)[1] %in% c(attr_templates$class, 'character'),
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

# Inputs = templates (.txt), data tables, and other entities ------------------

testthat::test_that('Inputs = templates (.txt), data tables, and other entities', {
  
  # Make function call
  
  output <- template_arguments(
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

# Inputs = templates (docx), data tables, and other entities -----------------

testthat::test_that('Inputs = templates (docx), data tables, and other entities', {
  
  # Make function call
  
  output <- template_arguments(
    path = system.file(
      '/examples/templates_docx',
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
      class(output$x$template[[i]]$content)[1] %in% c(attr_templates$class, 'character'),
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
