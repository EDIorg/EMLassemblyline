context('Create vector information template')
library(EMLassemblyline)

# File inputs -----------------------------------------------------------------

testthat::test_that('Test usage with file inputs', {
  
  # Missing path results in error
  
  expect_error(
    suppressMessages(
      template_vector_information(
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
      template_vector_information(
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
  
  # Invalid vector file result in error
  
  expect_error(
    suppressMessages(
      template_vector_information(
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
    template_vector_information(
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
    template_vector_information(
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
      '/information_shapefile_test.txt'
    )
  )
  
  file.remove(
    paste0(
      tempdir(),
      '/information_geojson_test_file.txt'
    )
  )

  expect_message(
    template_vector_information(
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
    'information_geojson_test_file.txt' %in% list.files(tempdir())
  )
  
})
