
context('Extract geographical coverage')
library(EMLassemblyline)

# Parameterize ----------------------------------------------------------------

path <- system.file('/examples/templates/abstract.txt', 
                    package = 'EMLassemblyline')
parent_dir <- substr(path, 1, nchar(path)-32)

# Expect errors ---------------------------------------------------------------

testthat::test_that('Expect errors', {
  
  expect_error(
    extract_geocoverage(data.path = paste0(parent_dir, '/data'), 
                        data.table = 'nitrogen.csv', 
                        site.col = 'site_name',
                        lat.col = 'site_lat', 
                        lon.col = 'site_lon')
  )
  
  expect_error(
    extract_geocoverage(path = paste0(parent_dir, '/metadata_templates'), 
                        data.table = 'nitrogen.csv', 
                        site.col = 'site_name',
                        lat.col = 'site_lat', 
                        lon.col = 'site_lon')
  )
  
  expect_error(
    extract_geocoverage(path = paste0(parent_dir, '/metadata_templates'), 
                        data.path = paste0(parent_dir, '/data'), 
                        site.col = 'site_name',
                        lat.col = 'site_lat', 
                        lon.col = 'site_lon')
  )
  
  expect_error(
    extract_geocoverage(path = paste0(parent_dir, '/metadata_templates'), 
                        data.path = paste0(parent_dir, '/data'), 
                        data.table = 'nitrogen.csv', 
                        lat.col = 'site_lat', 
                        lon.col = 'site_lon')
  )
  
  expect_error(
    extract_geocoverage(path = paste0(parent_dir, '/metadata_templates'), 
                        data.path = paste0(parent_dir, '/data'), 
                        data.table = 'nitrogen.csv', 
                        site.col = 'site_name',
                        lon.col = 'site_lon')
  )
  
  expect_error(
    extract_geocoverage(path = paste0(parent_dir, '/metadata_templates'), 
                        data.path = paste0(parent_dir, '/data'), 
                        data.table = 'nitrogen.csv', 
                        site.col = 'site_name',
                        lat.col = 'site_lat')
  )
  
  expect_error(
    extract_geocoverage(
      path = paste0(parent_dir, '/examples/templates'), 
      data.path = paste0(parent_dir, '/examples/data'), 
      data.table = 'nitrogen', 
      site.col = 'site_nameeeee',
      lat.col = 'site_lat',
      lon.col = 'site_lon'
    )
  )
  

  
})

# Expect equal ----------------------------------------------------------------

testthat::test_that('Expect data frame', {
  output <- suppressMessages(
    extract_geocoverage(
      path = paste0(parent_dir, '/examples/templates'), 
      data.path = paste0(parent_dir, '/examples/data'), 
      data.table = 'nitrogen', 
      site.col = 'site_name',
      lat.col = 'site_lat',
      lon.col = 'site_lon', 
      return.obj = TRUE)
    )
  expect_equal(class(output), 'data.frame')
  expect_equal(colnames(output), c('latitude', 'longitude', 'site'))
})

