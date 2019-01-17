
context('Extract geographical coverage')
library(EMLassemblyline)

# Parameterize ----------------------------------------------------------------

path <- system.file('/example_dataset/metadata_templates/abstract.txt', 
                    package = 'EMLassemblyline')
parent_dir <- substr(path, 1, nchar(path)-32)

# Tests -----------------------------------------------------------------------

extract_geocoverage(path = paste0(parent_dir, '/metadata_templates'), 
                    data.path = paste0(parent_dir, '/data'), 
                    data.file = 'Sphagnum_fuscum_growth_and_N_data.csv', 
                    site.col = 'site_name',
                    lat.col = 'site_lat', 
                    lon.col = 'site_lon')

#

testthat::test_that('Error when attribute files are missing', {
  expect_error(
    define_catvars(path = paste0(parent_dir, '/data'),
                   data.path = paste0(parent_dir, '/data'))
  )
})


testthat::test_that('Return message when catvars file already exists', {
  # expect_message(
  #   define_catvars(path = paste0(parent_dir, '/metadata_templates'),
  #                  data.path = paste0(parent_dir, '/data'))
  # )
  expect_equal(
    define_catvars(path = paste0(parent_dir, '/metadata_templates'),
                   data.path = paste0(parent_dir, '/data')),
    NULL
  )

})


testthat::test_that('Return data frame when files have not yet been created', {
  
  expect_equal(
    colnames(
      define_catvars(
        path = paste0(parent_dir, '/metadata_templates_missing_catvars'),
        data.path = paste0(parent_dir, '/data'))
    ),
    c('attributeName', 'code', 'definition')
  )
  
  expect_equal(
    class(
      define_catvars(
        path = paste0(parent_dir, '/metadata_templates_missing_catvars'),
        data.path = paste0(parent_dir, '/data'))
    ),
    'data.frame'
  )

})

