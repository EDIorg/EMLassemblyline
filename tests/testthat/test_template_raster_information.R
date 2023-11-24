context('Create raster information template')
library(EMLassemblyline)

# File inputs -----------------------------------------------------------------

testthat::test_that('Test usage with file inputs', {
  
  # Missing path results in error
  
  expect_error(
    suppressMessages(
      template_raster_information(
        data.path = system.file(
          '/examples/data',
          package = 'EMLassemblyline'
        ),
        raster.file = 'geotiff_test_file.tif',  # TODO check filename
        write.file = FALSE
      )
    )
  )
  
  # Invalid data path results in error
  
  expect_error(
    suppressMessages(
      template_raster_information(
        path = system.file(
          '/examples',
          package = 'EMLassemblyline'
        ),
        raster.file = 'geotiff_test_file.tif',  # TODO check filename
        write.file = FALSE
      )
    )
  )
  
  # Invalid raster files result in error
  
  expect_error(
    suppressMessages(
      template_raster_information(
        path = system.file(
          '/examples',
          package = 'EMLassemblyline'
        ),
        data.path = system.file(
          '/examples/data',
          package = 'EMLassemblyline'
        ),
        raster.file = 'geotiff_test_fiiiile.tif',  # TODO check filename
        write.file = FALSE
      )
    )
  )
  
  
  # New imports result in messages
  
  expect_message(
    template_raster_information(
      path = tempdir(),
      data.path = system.file(
        '/examples/data',
        package = 'EMLassemblyline'
      ),
      raster.file = 'geotiff_test_file.tif',  # TODO check filename
      write.file = TRUE
    )
  )
  
  # Attempt to import templates when they already exist results in messages
  
  expect_message(
    template_raster_information(
      path = tempdir(),
      data.path = system.file(
        '/examples/data',
        package = 'EMLassemblyline'
      ),
      raster.file = 'geotiff_test_file.tif',  # TODO check filename
      write.file = TRUE
    )
  )
  
  # write.file = TRUE writes files to path
  
  file.remove(
    paste0(
      tempdir(),
      '/information_geotiff_test_file.txt'
    )
  )
  
  expect_message(
    template_raster_information(
      path = tempdir(),
      data.path = system.file(
        '/examples/data',
        package = 'EMLassemblyline'
      ),
      raster.file = 'geotiff_test_file.tif',  # TODO check filename
      write.file = TRUE
    )
  )
  
  expect_true(
    'information_geotiff_test_file.txt' %in% list.files(tempdir())
  )
  
})
