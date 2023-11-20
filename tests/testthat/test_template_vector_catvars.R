context('Create categorical variables template for vector files')
library(EMLassemblyline)

# Test usage with file inputs -------------------------------------------------

testthat::test_that('Test usage with file inputs', {
  
  # write.file = TRUE writes files to path
  
  file.copy(
    from = system.file(
      '/examples/templates',
      package = 'EMLassemblyline'
    ),
    to = tempdir(),
    recursive = TRUE
  )
  
  expect_message(
    suppressWarnings(
      template_vector_catvars(
        path = paste0(
          tempdir(),
          '/templates'
        ), 
        data.path = system.file(
          '/examples/data',
          package = 'EMLassemblyline'
        ),
        vector.file = setNames(
          c('geojson_test_file.GeoJSON'), 
          c('attributes_geojson_test_file.txt')
        ),
        write.file = TRUE
      ) 
    )
  )
  
  # Correct argument use results in messages
  
  expect_message(
    suppressWarnings(
      template_vector_catvars(
        path = system.file(
          '/examples/templates',
          package = 'EMLassemblyline'
        ), 
        data.path = system.file(
          '/examples/data',
          package = 'EMLassemblyline'
        ),
        vector.file = setNames(
          c('geojson_test_file.GeoJSON'), 
          c('attributes_geojson_test_file.txt')
        ),
        write.file = FALSE
      )
    )
  )
  
  # Missing path results in error
  
  expect_error(
    suppressMessages(
      template_vector_catvars(
        data.path = system.file(
          '/examples/data',
          package = 'EMLassemblyline'
        ),
        vector.file = setNames(
          c('geojson_test_file.GeoJSON'), 
          c('attributes_geojson_test_file.txt')
        ),
        write.file = FALSE
      )
    )
  )
  
  # Missing attribute files from path
  
  expect_error(
    suppressMessages(
      template_vector_catvars(
        path = system.file(
          '/examples/data',
          package = 'EMLassemblyline'
        ),
        data.path = system.file(
          '/examples/data',
          package = 'EMLassemblyline'
        ),
        vector.file = setNames(
          c('geojson_test_file.GeoJSON'), 
          c('attributes_geojson_test_file.txt')
        ),
        write.file = FALSE
      )
    )
  )
  
})
