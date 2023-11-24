context('Create vector attributes template')
library(EMLassemblyline)

# File inputs -----------------------------------------------------------------

testthat::test_that('Test usage with file inputs', {
  
  # Missing path results in error
  
  expect_error(
    suppressMessages(
      template_vector_attributes(
        data.path = system.file(
          '/examples/data',
          package = 'EMLassemblyline'
        ),
        vector.file = c(  # TODO check filename
          'shapefile_test', 
          'geojson_test_file.GeoJSON'
        ),
        write.file = FALSE
      )
    )
  )
  
  # Invalid data path results in error
  
  expect_error(
    suppressMessages(
      template_vector_attributes(
        path = system.file(
          '/examples',
          package = 'EMLassemblyline'
        ),
        vector.file = c(  # TODO check filename
          'shapefile_test', 
          'geojson_test_file.GeoJSON'
        ),
        write.file = FALSE
      )
    )
  )
  
  # Invalid data tables result in error
  
  expect_error(
    suppressMessages(
      template_vector_attributes(
        path = system.file(
          '/examples',
          package = 'EMLassemblyline'
        ),
        data.path = system.file(
          '/examples/data',
          package = 'EMLassemblyline'
        ),
        vector.file = c(  # TODO check filename
          'shapefile_testttt', 
          'geojson_test_fiiiile.GeoJSON'
        ),
        write.file = FALSE
      )
    )
  )
  
  
  # New imports result in messages
  
  expect_message(
    template_vector_attributes(
      path = tempdir(),
      data.path = system.file(
        '/examples/data',
        package = 'EMLassemblyline'
      ),
      vector.file = c(  # TODO check filename
        'shapefile_test', 
        'geojson_test_file.GeoJSON'
      ),
      write.file = TRUE
    )
  )
  
  # Attempt to import templates when they already exist results in messages
  
  expect_message(
    template_vector_attributes(
      path = tempdir(),
      data.path = system.file(
        '/examples/data',
        package = 'EMLassemblyline'
      ),
      vector.file = c(  # TODO check filename
        'shapefile_test', 
        'geojson_test_file.GeoJSON'
      ),
      write.file = TRUE
    )
  )
  
  # write.file = TRUE writes files to path
  
  file.remove(
    paste0(
      tempdir(),
      '/attributes_shapefile_test.txt'
    )
  )
  
  file.remove(
    paste0(
      tempdir(),
      '/attributes_geojson_test_file.txt'
    )
  )
  
  file.remove(
    paste0(
      tempdir(),
      '/custom_units.txt'
    )
  )
  
  expect_message(
    template_vector_attributes(
      path = tempdir(),
      data.path = system.file(
        '/examples/data',
        package = 'EMLassemblyline'
      ),
      vector.file = c(  # TODO check filename
        'shapefile_test', 
        'geojson_test_file.GeoJSON'
      ),
      write.file = TRUE
    )
  )
  
  expect_true(
    'custom_units.txt' %in% list.files(tempdir())
  )
  
})
