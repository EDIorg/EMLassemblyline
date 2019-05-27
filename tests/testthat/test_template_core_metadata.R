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

# Write to path ---------------------------------------------------------------

testthat::test_that('Write to path', {
  
  # Missing path results in error
  
  expect_error(
    suppressMessages(
      template_core_metadata(
        license = 'CC0'
      )
    )
  )
  
  # Missing license results in error
  
  expect_error(
    suppressMessages(
      template_core_metadata(
        path = tempdir(),
        write.file = FALSE
      )
    )
  )
  
  # Invalid file type results in error
  
  expect_error(
    suppressMessages(
      template_core_metadata(
        path = system.file(
          '/examples',
          package = 'EMLassemblyline'
        ),
        license = 'CC0',
        file.type = '.txtdocx'
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
        license = 'CCzero'
      )
    )
  )
  
  # New imports result in messages
  
  expect_message(
    template_core_metadata(
      path = tempdir(),
      license = 'CC0'
    )
  )
  
  # CCBY is supported
  
  file.remove(paste0(tempdir(), '/intellectual_rights.txt'))

  expect_message(
    template_core_metadata(
      path = tempdir(),
      license = 'CCBY'
    )
  )
  
  # Attempt to import templates when they already exist results in messages
  
  expect_message(
    template_core_metadata(
      path = tempdir(),
      license = 'CC0'
    )
  )
  
  # write.file = FALSE, does not write files to path
  
  expect_message(
    template_core_metadata(
      path = tempdir(),
      license = 'CC0',
      write.file = FALSE
    )
  )

  # file.type = '.docx' writes MS Word files to path
  
  file.remove(paste0(tempdir(), 
                     c('/abstract.txt',
                       '/methods.txt',
                       '/additional_info.txt')))
  
  expect_message(
    template_core_metadata(
      path = tempdir(),
      license = 'CC0',
      file.type = '.docx',
      write.file = TRUE
    )
  )

  expect_true(
    all(
      c('abstract.docx', 'additional_info.docx', 'methods.docx') %in% 
        list.files(tempdir())
    )
  )
  
  # file.type = '.md' writes Markdown files to path

  file.remove(paste0(tempdir(), 
                     c('/abstract.docx',
                       '/methods.docx',
                       '/additional_info.docx')))
  
  expect_message(
    template_core_metadata(
      path = tempdir(),
      license = 'CC0',
      file.type = '.md',
      write.file = TRUE
    )
  )

  
  expect_true(
    all(
      c('abstract.md', 'additional_info.md', 'methods.md') %in% 
        list.files(tempdir())
    )
  )
  
})

# Write to x ------------------------------------------------------------------

testthat::test_that('Write to x', {
  
  # Make function call
  
  x <- template_arguments()
  x <- x$x
  
  # Missing path results in messages
  
  expect_message(
    template_core_metadata(
      license = 'CC0',
      x = x
    )
  )
  
  # Missing path results in expected content classes with empty values
  
  output <- suppressMessages(
    template_core_metadata(
      license = 'CC0',
      x = x
    )
  )
  
  for (i in 1:length(output$template)){
    
    expect_equal(
      class(output$template[[i]]$content)[1] %in% 
        c('list', 'data.frame', 'list', 'character'),
      TRUE
    )
    
  }
  
  # Missing license results in error
  
  expect_error(
    suppressMessages(
      template_core_metadata(
        x = x
      )
    )
  )
  
  # Unsupported license results in error
  
  expect_error(
    suppressMessages(
      template_core_metadata(
        x = x,
        license = 'CCzero'
      )
    )
  )
  
  # CCBY is a supported license
  
  expect_message(
    template_core_metadata(
      x = x,
      license = 'CCBY'
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
      license = 'CC0'
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
      x = x
    )
  )
  
  for (i in 1:length(output$template)){
    
    expect_equal(
      class(output$template[[i]]$content)[1] %in% 
        c('list', 'data.frame', 'list', 'character'),
      TRUE
    )
    
  }
  
  # Attempt to import templates when they already exist results in messages
  
  expect_message(
    template_core_metadata(
      x = output,
      license = 'CC0'
    )
  )
  
  # CCBY is supported
  
  output$template$intellectual_rights.txt$content <- NA_character_
  
  expect_message(
    template_core_metadata(
      x = output,
      license = 'CCBY'
    )
  )
  
  # file.type = '.docx' writes MS Word files to path --------------------------
  
  x <- template_arguments()
  x <- x$x

  output <- template_core_metadata(
    license = 'CC0',
    file.type = '.docx',
    x = x
  )

  for (i in 1:length(output$template)){
    expect_equal(
      class(output$template[[i]]$content)[1] %in% 
        c('list', 'data.frame', 'character'),
      TRUE
    )
  }
  
  # file.type = '.md' writes Markdown files to path ---------------------------
  
  x <- template_arguments()
  x <- x$x
  
  output <- template_core_metadata(
    license = 'CC0',
    file.type = '.md',
    x = x
  )
  
  for (i in 1:length(output$template)){
    expect_equal(
      class(output$template[[i]]$content)[1] %in% 
        c('list', 'data.frame', 'character'),
      TRUE
    )
  }
  
})
