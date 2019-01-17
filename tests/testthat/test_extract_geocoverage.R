
context('Extract geographical coverage')
library(EMLassemblyline)

# Parameterize ----------------------------------------------------------------

path <- system.file('/example_dataset/metadata_templates/abstract.txt', 
                    package = 'EMLassemblyline')
parent_dir <- substr(path, 1, nchar(path)-32)

# Expect errors ---------------------------------------------------------------

testthat::test_that('Expect errors', {
  
  expect_error(
    extract_geocoverage(data.path = paste0(parent_dir, '/data'), 
                        data.file = 'Sphagnum_fuscum_growth_and_N_data.csv', 
                        site.col = 'site_name',
                        lat.col = 'site_lat', 
                        lon.col = 'site_lon')
  )
  
  expect_error(
    extract_geocoverage(path = paste0(parent_dir, '/metadata_templates'), 
                        data.file = 'Sphagnum_fuscum_growth_and_N_data.csv', 
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
                        data.file = 'Sphagnum_fuscum_growth_and_N_data.csv', 
                        lat.col = 'site_lat', 
                        lon.col = 'site_lon')
  )
  
  expect_error(
    extract_geocoverage(path = paste0(parent_dir, '/metadata_templates'), 
                        data.path = paste0(parent_dir, '/data'), 
                        data.file = 'Sphagnum_fuscum_growth_and_N_data.csv', 
                        site.col = 'site_name',
                        lon.col = 'site_lon')
  )
  
  expect_error(
    extract_geocoverage(path = paste0(parent_dir, '/metadata_templates'), 
                        data.path = paste0(parent_dir, '/data'), 
                        data.file = 'Sphagnum_fuscum_growth_and_N_data.csv', 
                        site.col = 'site_name',
                        lat.col = 'site_lat')
  )
  
})

# Expect equal ----------------------------------------------------------------

testthat::test_that('Expect data frame', {
  output <- suppressMessages(
    extract_geocoverage(
      path = paste0(parent_dir, '/metadata_templates'), 
      data.path = paste0(parent_dir, '/data'), 
      data.file = 'Sphagnum_fuscum_growth_and_N_data.csv', 
      site.col = 'site_name',
      lat.col = 'site_lat',
      lon.col = 'site_lon')
    )
  expect_equal(class(output), 'data.frame')
  expect_equal(colnames(output), c('latitude', 'longitude', 'site'))
})

