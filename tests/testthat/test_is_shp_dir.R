context('Create vector attributes template')
library(EMLassemblyline)

testthat::test_that('Test is_shp_dir function', {
  
  # Return message and FALSE if dir_path does not exists
  
  expect_message(
    is_shp_dir(dir_path = "not/existing/path")
  )
  
  expect_false(
    suppressMessages(
      is_shp_dir(dir_path = "not/existing/path")
    )
  )
  
  # Return FALSE if dir_path exists but is not a shapefile directory
  
  expect_false(
    is_shp_dir(
      system.file(
        '/examples/data/decomp.csv',
        package = 'EMLassemblyline'
      )
    )
  )
  
  # # Return message and FALSE if dir_path is a zip archive
  # 
  # expect_message(
  #   is_shp_dir(
  #     dir_path = system.file(
  #       '/examples/data/shapefile_test.zip',
  #       package = 'EMLassemblyline'
  #     )
  #   )
  # )
  # 
  # expect_false(
  #   suppressMessages(
  #     is_shp_dir(
  #       dir_path = system.file(
  #         '/examples/data/shapefile_test.zip',
  #         package = 'EMLassemblyline'
  #       )
  #     )
  #   )
  # )
  
  
  # Return TRUE if dir_path is a shapefile directory
  
  expect_true(
    is_shp_dir(
      dir_path = system.file(
        '/examples/data/shapefile_test',
        package = 'EMLassemblyline'
      )
    )
  )
  
})
