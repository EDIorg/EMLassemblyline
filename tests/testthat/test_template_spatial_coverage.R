context('Extract spatial coverage')
library(EMLassemblyline)

# Test usage with file inputs -------------------------------------------------

testthat::test_that('Test usage with file inputs', {
  
  # Clean tempdir() of .spatial_coverage.txt
  
  unlink(
    paste0(
      tempdir(),
      '/.spatial_coverage.txt'
    ),
    force = TRUE
  )
  
  # Missing path results in error
  
  expect_error(
    suppressMessages(
      template_spatial_coverage(
        # path = system.file(
        #   '/examples/pkg_260/data_objects/',
        #   package = 'EMLassemblyline'
        # ), 
        data.path = system.file(
          '/examples/pkg_260/data_objects/',
          package = 'EMLassemblyline'
        ),
        site.col = 'nom', # FIXME when example data is set
        write.file = FALSE,
        overwrite = TRUE
      ) 
    )
  )
  
  # Missing data.path results in error
  
  expect_error(
    suppressMessages(
      template_spatial_coverage(
        path = system.file(
          '/examples/pkg_260/data_objects/',
          package = 'EMLassemblyline'
        ),
        # data.path = system.file(
        #   '/examples/pkg_260/data_objects/',
        #   package = 'EMLassemblyline'
        # ),
        site.col = 'nom', # FIXME when example data is set
        write.file = FALSE,
        overwrite = TRUE
      ) 
    )
  )
  
  # Missing site.col results in error
  
  expect_error(
    suppressMessages(
      template_spatial_coverage(
        path = system.file(
          '/examples/pkg_260/data_objects/',
          package = 'EMLassemblyline'
        ),
        data.path = system.file(
          '/examples/pkg_260/data_objects/',
          package = 'EMLassemblyline'
        ),
        # site.col = 'nom', # FIXME when example data is set
        write.file = FALSE,
        overwrite = TRUE
      ) 
    )
  )
  
  # Misspelled site.col results in error
  
  expect_error(
    suppressMessages(
      template_spatial_coverage(
        path = system.file(
          '/examples/pkg_260/data_objects/',
          package = 'EMLassemblyline'
        ),
        data.path = system.file(
          '/examples/pkg_260/data_objects/',
          package = 'EMLassemblyline'
        ),
        site.col = 'noooooom', # FIXME when example data is set
        write.file = FALSE,
        overwrite = TRUE
      ) 
    )
  )
  
  # Valid arguments result in messages
  
  expect_message(
    template_spatial_coverage(
      path = system.file(
        '/examples/pkg_260/data_objects/',
        package = 'EMLassemblyline'
      ),
      data.path = system.file(
        '/examples/pkg_260/data_objects/',
        package = 'EMLassemblyline'
      ),
      site.col = 'nom', # FIXME when example data is set
      write.file = FALSE,
      overwrite = TRUE
    ) 
  )
  
  # Writing to file results in messages
  
  expect_message(
    template_spatial_coverage(
      path = system.file(
        '/examples/pkg_260/data_objects/',
        package = 'EMLassemblyline'
      ),
      data.path = system.file(
        '/examples/pkg_260/data_objects/',
        package = 'EMLassemblyline'
      ),
      site.col = 'nom', # FIXME when example data is set
      write.file = TRUE,
      overwrite = TRUE
    ) 
  )
  
  unlink(
    paste0(
      tempdir(),
      '/.spatial_coverage.txt'
    ),
    force = TRUE
  )
  
})

# Test written coverage ---------------------------------------------------

testthat::test_that('Test written coverage', {
  x <- template_spatial_coverage(
    path = system.file(
      '/examples/pkg_260/data_objects/',
      package = 'EMLassemblyline'
    ),
    data.path = system.file(
      '/examples/pkg_260/data_objects/',
      package = 'EMLassemblyline'
    ),
    site.col = 'nom', # FIXME when example data is set
    write.file = TRUE,
    overwrite = TRUE
  )
  
  y <- utils::read.csv(
    system.file(
      '/examples/pkg_260/metadata_templates/.spatial_coverage.txt', # FIXME when example data is set
      package = 'EMLassemblyline'
    ),
    header = TRUE,
    sep = "\t"
  )
  
  expect_equal(x$shp_coverage, y)
})

# Test overwriting coverage ---------------------------------------------------

testthat::test_that('Test overwriting coverage', {
  
  # write surely false spatial coverage
  random <- data.frame(
    file = LETTERS[1:5],
    site_name = LETTERS[1:5],
    wkt = LETTERS[1:5]
  )
  utils::write.csv(
    random,
    system.file(
      '/examples/pkg_260/data_objects/.spatial_coverage.txt',
      package = 'EMLassemblyline'
    ),
    sep = "\t"
  )
  
  # try to overwrite it
  template_spatial_coverage(
    path = system.file(
      '/examples/pkg_260/data_objects/',
      package = 'EMLassemblyline'
    ),
    data.path = system.file(
      '/examples/pkg_260/data_objects/',
      package = 'EMLassemblyline'
    ),
    site.col = 'nom', # FIXME when example data is set
    write.file = TRUE,
    overwrite = TRUE
  )
  
  # check it has changed -- shall have, shouldn't be identical
  expect_false(identical(
    utils::read(system.file(
      '/examples/pkg_260/data_objects/.spatial_coverage.txt',
      package = 'EMLassemblyline'
    )),
    random
  ))
  
})



# Test usage with x inputs ----------------------------------------------------

# testthat::test_that('Test usage with x inputs', {
#   
#   # Create x_list and x_no_coverage with and without geographic_coverage.txt, 
#   # respectivly
#   
#   x_list <- template_arguments(
#     path = system.file(
#       '/examples/templates', 
#       package = 'EMLassemblyline'
#     ),
#     data.path = system.file(
#       '/examples/data',
#       package = 'EMLassemblyline'
#     ),
#     data.table = c(
#       'decomp.csv',
#       'nitrogen.csv'
#     )
#   )
#   
#   x_list <- x_list$x
#   
#   x_no_coverage <- x_list
#   
#   x_no_coverage$template$geographic_coverage.txt <- NULL
#   
#   # All arguments are supported:
#   # - /x/templates/geographic_coverage.txt is created with expected content
#   
#   expect_message(
#     template_geographic_coverage(
#       path = '/some/path',
#       data.path = '/some/data.path', 
#       data.table = 'nitrogen.csv', 
#       site.col = 'site_name',
#       lat.col = 'site_lat', 
#       lon.col = 'site_lon',
#       x = x_no_coverage,
#       write.file = FALSE
#     )
#   )
#   
#   output <- suppressMessages(
#     template_geographic_coverage(
#       path = '/some/path',
#       data.path = '/some/data.path', 
#       data.table = 'nitrogen.csv', 
#       site.col = 'site_name',
#       lat.col = 'site_lat', 
#       lon.col = 'site_lon',
#       x = x_no_coverage,
#       write.file = FALSE
#     )
#   )
#   
#   expect_equal(
#     class(output$template$geographic_coverage.txt$content),
#     'data.frame'
#   )
#   
#   expect_equal(
#     all(
#       colnames(output$template$geographic_coverage.txt$content) %in%
#         c('northBoundingCoordinate', 'southBoundingCoordinate', 
#           'eastBoundingCoordinate', 'westBoundingCoordinate', 'geographicDescription')
#     ),
#     TRUE
#   )
#   
#   # Missing path adds NA to /x/templates/geographic_coverage.txt/path
#   
#   expect_message(
#     template_geographic_coverage(
#       data.path = system.file(
#         '/examples/data',
#         package = 'EMLassemblyline'
#       ), 
#       data.table = 'nitrogen.csv', 
#       site.col = 'site_name',
#       lat.col = 'site_lat', 
#       lon.col = 'site_lon',
#       x = x_no_coverage,
#       write.file = FALSE
#     )
#   )
#   
#   output <- suppressMessages(
#     template_geographic_coverage(
#       data.path = system.file(
#         '/examples/data',
#         package = 'EMLassemblyline'
#       ), 
#       data.table = 'nitrogen.csv', 
#       site.col = 'site_name',
#       lat.col = 'site_lat', 
#       lon.col = 'site_lon',
#       x = x_no_coverage,
#       write.file = FALSE
#     )
#   )
#   
#   # Missing data.path has no effect
#   
#   expect_message(
#     template_geographic_coverage(
#       data.table = 'nitrogen.csv', 
#       site.col = 'site_name',
#       lat.col = 'site_lat', 
#       lon.col = 'site_lon',
#       x = x_no_coverage,
#       write.file = FALSE
#     )
#   )
#   
#   # Missing data.table results in error
#   
#   expect_error(
#     suppressMessages(
#       template_geographic_coverage(
#         site.col = 'site_name',
#         lat.col = 'site_lat', 
#         lon.col = 'site_lon',
#         x = x_no_coverage,
#         write.file = FALSE
#       )
#     )
#   )
#   
#   # Missing site.col results in error
#   
#   expect_error(
#     suppressMessages(
#       template_geographic_coverage(
#         data.table = 'nitrogen.csv', 
#         lat.col = 'site_lat', 
#         lon.col = 'site_lon',
#         x = x_no_coverage,
#         write.file = FALSE
#       )
#     )
#   )
#   
#   # Missing lat.col results in error
#   
#   expect_error(
#     suppressMessages(
#       template_geographic_coverage(
#         data.table = 'nitrogen.csv', 
#         site.col = 'site_name', 
#         lon.col = 'site_lon',
#         x = x_no_coverage,
#         write.file = FALSE
#       )
#     )
#   )
#   
#   # Missing lon.col results in error
#   
#   expect_error(
#     suppressMessages(
#       template_geographic_coverage(
#         data.table = 'nitrogen.csv', 
#         site.col = 'site_name', 
#         lat.col = 'site_lat',
#         x = x_no_coverage,
#         write.file = FALSE
#       )
#     )
#   )
#   
#   # Misspelled site.col results in error
#   
#   expect_error(
#     suppressMessages(
#       template_geographic_coverage(
#         data.table = 'nitrogen.csv', 
#         site.col = 'site_nameeee', 
#         lat.col = 'site_lat',
#         lon.col = 'site_lon',
#         x = x_no_coverage,
#         write.file = FALSE
#       )
#     )
#   )
#   
#   # Misspelled lat.col results in error
#   
#   expect_error(
#     suppressMessages(
#       template_geographic_coverage(
#         data.table = 'nitrogen.csv', 
#         site.col = 'site_name', 
#         lat.col = 'site_lattt',
#         lon.col = 'site_lon',
#         x = x_no_coverage,
#         write.file = FALSE
#       )
#     )
#   )
#   
#   # Misspelled lon.col results in error
#   
#   expect_error(
#     suppressMessages(
#       template_geographic_coverage(
#         data.table = 'nitrogen.csv', 
#         site.col = 'site_name', 
#         lat.col = 'site_lat',
#         lon.col = 'site_lonnn',
#         x = x_no_coverage,
#         write.file = FALSE
#       )
#     )
#   )
#   
#   # write.file = TRUE result in error
#   
#   expect_error(
#     suppressMessages(
#       template_geographic_coverage(
#         data.table = 'nitrogen.csv', 
#         site.col = 'site_name', 
#         lat.col = 'site_lat',
#         lon.col = 'site_lon',
#         x = x_no_coverage,
#         write.file = TRUE
#       )
#     )
#   )
#   
#   # Existing geographic_coverage.txt results in messages
#   
#   expect_message(
#     suppressWarnings(
#       template_geographic_coverage(
#         data.table = 'nitrogen.csv', 
#         site.col = 'site_name', 
#         lat.col = 'site_lat',
#         lon.col = 'site_lon',
#         x = x_list,
#         write.file = FALSE
#       ) 
#     )
#   )
# 
# })

# Test usage with 'empty = TRUE' ----------------------------------------------

# testthat::test_that('Test usage with "empty = TRUE"', {
#   
#   # Clean tempdir() of geographic_coverage.txt
#   
#   unlink(
#     paste0(
#       tempdir(),
#       '/geographic_coverage.txt'
#     ),
#     force = TRUE
#   )
#   
#   # Writing to file results in messages
#   
#   expect_message(
#     template_geographic_coverage(
#       path = tempdir(), 
#       empty = TRUE
#     )
#   )
#   
#   # File has column names and no rows
#   
#   input <- utils::read.table(
#     paste0(
#       tempdir(), 
#       '/geographic_coverage.txt'
#     ),
#     header = T,
#     sep="\t",
#     quote="\"",
#     as.is=TRUE,
#     comment.char = "",
#     fill = T,
#     na.strings = "NA",
#     fileEncoding = "UTF-8"
#   )
#   
#   expect_true(
#     all(colnames(input) %in% 
#           c('geographicDescription', 'northBoundingCoordinate', 
#             'southBoundingCoordinate', 'eastBoundingCoordinate',
#             'westBoundingCoordinate'))
#   )
#   
#   expect_equal(
#     nrow(input),
#     0
#   )
# 
# })

# Coerce lat.col & lon.col to numeric -----------------------------------------

# testthat::test_that("Coerce lat.col & lon.col to numeric", {
#   
#   # Add character strings to numeric columns of the example data and expect 
#   # these rows to be missing from the resultant template.
#   
#   unlink(
#     paste0(tempdir(), c("/nitrogen.csv", "/geographic_coverage.txt")),
#     force = TRUE)
#   
#   # Add character strings to numeric columns of example data
#   
#   d <- data.table::fread(
#     system.file(
#       '/examples/pkg_260/data_objects/nitrogen.csv', 
#       package = 'EMLassemblyline'))
#   d$site_lat[1] <- "a_character_string"
#   d$site_lon[2] <- "a_character_string"
#   
#   data.table::fwrite(d, paste0(tempdir(), '/nitrogen.csv'))
#   
#   template_geographic_coverage(
#     path = tempdir(),
#     data.table = 'nitrogen.csv', 
#     site.col = 'site_name',
#     lat.col = 'site_lat', 
#     lon.col = 'site_lon')
#   
#   template <- data.table::fread(paste0(tempdir(), "/geographic_coverage.txt"))
#   
#   expect_true(
#     all(!(d$site_name[1:2] %in% template$geographicDescription)))
#   
# })





