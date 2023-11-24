context('Create netcdf attributes template')
library(EMLassemblyline)

# File inputs -----------------------------------------------------------------

testthat::test_that('Test usage with file inputs', {
  
  # Missing path results in error
  
  expect_error(
    suppressMessages(
      template_netcdf_attributes(
        data.path = system.file(
          '/examples/data',
          package = 'EMLassemblyline'
        ),
        data.table = 'netcdf_test_file.nc',
        write.file = FALSE
      )
    )
  )
  
  # Invalid data path results in error
  
  expect_error(
    suppressMessages(
      template_netcdf_attributes(
        path = system.file(
          '/examples',
          package = 'EMLassemblyline'
        ),
        data.table = 'netcdf_test_file.nc',
        write.file = FALSE
      )
    )
  )
  
  # Invalid data tables result in error
  
  expect_error(
    suppressMessages(
      template_netcdf_attributes(
        path = system.file(
          '/examples',
          package = 'EMLassemblyline'
        ),
        data.path = system.file(
          '/examples/data',
          package = 'EMLassemblyline'
        ),
        netcdf = 'netcdf_testtttt_file.nc',
        write.file = FALSE
      )
    )
  )
  
  
  # New imports result in messages
  
  expect_message(
    template_netcdf_attributes(
      path = tempdir(),
      data.path = system.file(
        '/examples/data',
        package = 'EMLassemblyline'
      ),
      netcdf = 'netcdf_test_file.nc',
      write.file = TRUE
    )
  )
  
  # Attempt to import templates when they already exist results in messages
  
  expect_message(
    template_netcdf_attributes(
      path = tempdir(),
      data.path = system.file(
        '/examples/data',
        package = 'EMLassemblyline'
      ),
      netcdf = 'netcdf_test_file.nc',
      write.file = TRUE
    )
  )
  
  # write.file = TRUE writes files to path
  
  file.remove(
    paste0(
      tempdir(),
      '/attributes_netcdf_test_file.txt'
    )
  )
  
  file.remove(
    paste0(
      tempdir(),
      '/custom_units.txt'
    )
  )
  
  expect_message(
    template_netcdf_attributes(
      path = tempdir(),
      data.path = system.file(
        '/examples/data',
        package = 'EMLassemblyline'
      ),
      netcdf = 'netcdf_test_file.nc',
      write.file = TRUE
    )
  )
  
  expect_true(
    'custom_units.txt' %in% list.files(tempdir())
  )
  
})
