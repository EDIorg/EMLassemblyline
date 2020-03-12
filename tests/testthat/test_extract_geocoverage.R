context('Extract geographical coverage')
library(EMLassemblyline)

# Test usage with file inputs -------------------------------------------------

testthat::test_that('Test usage with file inputs', {
  
  # Missing path results in error
  
  expect_error(
    suppressMessages(
      suppressWarnings(
        extract_geocoverage(
          data.path = system.file(
            '/examples/data',
            package = 'EMLassemblyline'
          ), 
          data.table = 'nitrogen.csv', 
          site.col = 'site_name',
          lat.col = 'site_lat', 
          lon.col = 'site_lon',
          write.file = FALSE
        ) 
      )
    )
  )
  
  # Missing data.path results in error
  
  expect_error(
    suppressMessages(
      suppressWarnings(
        extract_geocoverage(
          path = system.file(
            '/examples/templates'
          ), 
          data.table = 'nitrogen.csv', 
          site.col = 'site_name',
          lat.col = 'site_lat', 
          lon.col = 'site_lon',
          write.file = FALSE
        ) 
      )
    )
  )
  
  # Missing data.table results in error
  
  expect_error(
    suppressMessages(
      suppressWarnings(
        extract_geocoverage(
          path = system.file(
            '/examples/templates',
            package = 'EMLassemblyline'
          ), 
          data.path = system.file(
            '/examples/data',
            package = 'EMLassemblyline'
          ), 
          site.col = 'site_name',
          lat.col = 'site_lat', 
          lon.col = 'site_lon',
          write.file = FALSE
        ) 
      )
    )
  )
  
  # Missing site.col results in error
  
  expect_error(
    suppressMessages(
      suppressWarnings(
        extract_geocoverage(
          path = system.file(
            '/examples/templates',
            package = 'EMLassemblyline'
          ), 
          data.path = system.file(
            '/examples/data',
            package = 'EMLassemblyline'
          ), 
          data.table = 'nitrogen.csv', 
          lat.col = 'site_lat', 
          lon.col = 'site_lon',
          write.file = FALSE
        ) 
      )
    )
  )
  
  # Missing lat.col results in error
  
  expect_error(
    suppressMessages(
      suppressWarnings(
        extract_geocoverage(
          path = system.file(
            '/examples/templates',
            package = 'EMLassemblyline'
          ), 
          data.path = system.file(
            '/examples/data',
            package = 'EMLassemblyline'
          ), 
          data.table = 'nitrogen.csv', 
          site.col = 'site_name', 
          lon.col = 'site_lon',
          write.file = FALSE
        ) 
      )
    )
  )
  
  # Missing lon.col results in error
  
  expect_error(
    suppressMessages(
      suppressWarnings(
        extract_geocoverage(
          path = system.file(
            '/examples/templates',
            package = 'EMLassemblyline'
          ), 
          data.path = system.file(
            '/examples/data',
            package = 'EMLassemblyline'
          ), 
          data.table = 'nitrogen.csv', 
          site.col = 'site_name', 
          lat.col = 'site_lat',
          write.file = FALSE
        ) 
      )
    )
  )
  
  # Misspelled site.col results in error
  
  expect_error(
    suppressMessages(
      suppressWarnings(
        extract_geocoverage(
          path = system.file(
            '/examples',
            package = 'EMLassemblyline'
          ), 
          data.path = system.file(
            '/examples/data',
            package = 'EMLassemblyline'
          ), 
          data.table = 'nitrogen.csv', 
          site.col = 'site_nameeee', 
          lat.col = 'site_lat',
          lon.col = 'site_lon',
          write.file = FALSE
        ) 
      )
    )
  )
  
  # Misspelled lat.col results in error
  
  expect_error(
    suppressMessages(
      suppressWarnings(
        extract_geocoverage(
          path = system.file(
            '/examples',
            package = 'EMLassemblyline'
          ), 
          data.path = system.file(
            '/examples/data',
            package = 'EMLassemblyline'
          ), 
          data.table = 'nitrogen.csv', 
          site.col = 'site_name', 
          lat.col = 'site_latt',
          lon.col = 'site_lon',
          write.file = FALSE
        ) 
      )
    )
  )
  
  # Misspelled lon.col results in error
  
  expect_error(
    suppressMessages(
      suppressWarnings(
        extract_geocoverage(
          path = system.file(
            '/examples',
            package = 'EMLassemblyline'
          ), 
          data.path = system.file(
            '/examples/data',
            package = 'EMLassemblyline'
          ), 
          data.table = 'nitrogen.csv', 
          site.col = 'site_name', 
          lat.col = 'site_lat',
          lon.col = 'site_lonnnnn',
          write.file = FALSE
        ) 
      )
    )
  )
  
  # Valid arguments result in messages
  
  expect_message(
    suppressWarnings(
      extract_geocoverage(
        path = system.file(
          '/examples',
          package = 'EMLassemblyline'
        ), 
        data.path = system.file(
          '/examples/data',
          package = 'EMLassemblyline'
        ), 
        data.table = 'nitrogen.csv', 
        site.col = 'site_name', 
        lat.col = 'site_lat',
        lon.col = 'site_lon',
        write.file = FALSE
      ) 
    )
  )
  
  # Deprecated arguments result in warning
  
  expect_warning(
    extract_geocoverage(
      path = system.file(
        '/examples',
        package = 'EMLassemblyline'
      ), 
      data.path = system.file(
        '/examples/data',
        package = 'EMLassemblyline'
      ), 
      data.file = 'nitrogen.csv', 
      site.col = 'site_name', 
      lat.col = 'site_lat',
      lon.col = 'site_lon',
      write.file = FALSE
    )
  )

})

# # Test usage with x inputs ----------------------------------------------------
# 
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
#     suppressWarnings(
#       extract_geocoverage(
#         path = '/some/path',
#         data.path = '/some/data.path', 
#         data.table = 'nitrogen.csv', 
#         site.col = 'site_name',
#         lat.col = 'site_lat', 
#         lon.col = 'site_lon',
#         x = x_no_coverage,
#         write.file = FALSE
#       ) 
#     )
#   )
#   
#   output <- suppressMessages(
#     suppressWarnings(
#       extract_geocoverage(
#         path = '/some/path',
#         data.path = '/some/data.path', 
#         data.table = 'nitrogen.csv', 
#         site.col = 'site_name',
#         lat.col = 'site_lat', 
#         lon.col = 'site_lon',
#         x = x_no_coverage,
#         write.file = FALSE
#       ) 
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
#       c('latitude', 'longitude', 'site')
#     ),
#     TRUE
#   )
#   
#   # Missing path adds NA to /x/templates/geographic_coverage.txt/path
#   
#   expect_message(
#     suppressWarnings(
#       extract_geocoverage(
#         data.path = system.file(
#           '/examples/data',
#           package = 'EMLassemblyline'
#         ), 
#         data.table = 'nitrogen.csv', 
#         site.col = 'site_name',
#         lat.col = 'site_lat', 
#         lon.col = 'site_lon',
#         x = x_no_coverage,
#         write.file = FALSE
#       ) 
#     )
#   )
#   
#   output <- suppressMessages(
#     suppressWarnings(
#       extract_geocoverage(
#         data.path = system.file(
#           '/examples/data',
#           package = 'EMLassemblyline'
#         ), 
#         data.table = 'nitrogen.csv', 
#         site.col = 'site_name',
#         lat.col = 'site_lat', 
#         lon.col = 'site_lon',
#         x = x_no_coverage,
#         write.file = FALSE
#       ) 
#     )
#   )
#   
#   # Missing data.path has no effect
#   
#   expect_message(
#     suppressWarnings(
#       extract_geocoverage(
#         data.table = 'nitrogen.csv', 
#         site.col = 'site_name',
#         lat.col = 'site_lat', 
#         lon.col = 'site_lon',
#         x = x_no_coverage,
#         write.file = FALSE
#       ) 
#     )
#   )
#   
#   # Missing data.table results in error
#   
#   expect_error(
#     suppressMessages(
#       suppressWarnings(
#         extract_geocoverage(
#           site.col = 'site_name',
#           lat.col = 'site_lat', 
#           lon.col = 'site_lon',
#           x = x_no_coverage,
#           write.file = FALSE
#         ) 
#       )
#     )
#   )
#   
#   # Missing site.col results in error
#   
#   expect_error(
#     suppressMessages(
#       suppressWarnings(
#         extract_geocoverage(
#           data.table = 'nitrogen.csv', 
#           lat.col = 'site_lat', 
#           lon.col = 'site_lon',
#           x = x_no_coverage,
#           write.file = FALSE
#         ) 
#       )
#     )
#   )
#   
#   # Missing lat.col results in error
#   
#   expect_error(
#     suppressMessages(
#       suppressWarnings(
#         extract_geocoverage(
#           data.table = 'nitrogen.csv', 
#           site.col = 'site_name', 
#           lon.col = 'site_lon',
#           x = x_no_coverage,
#           write.file = FALSE
#         ) 
#       )
#     )
#   )
#   
#   # Missing lon.col results in error
#   
#   expect_error(
#     suppressMessages(
#       suppressWarnings(
#         extract_geocoverage(
#           data.table = 'nitrogen.csv', 
#           site.col = 'site_name', 
#           lat.col = 'site_lat',
#           x = x_no_coverage,
#           write.file = FALSE
#         ) 
#       )
#     )
#   )
#   
#   # Misspelled site.col results in error
#   
#   expect_error(
#     suppressMessages(
#       suppressWarnings(
#         extract_geocoverage(
#           data.table = 'nitrogen.csv', 
#           site.col = 'site_nameeee', 
#           lat.col = 'site_lat',
#           lon.col = 'site_lon',
#           x = x_no_coverage,
#           write.file = FALSE
#         ) 
#       )
#     )
#   )
#   
#   # Misspelled lat.col results in error
#   
#   expect_error(
#     suppressMessages(
#       suppressWarnings(
#         extract_geocoverage(
#           data.table = 'nitrogen.csv', 
#           site.col = 'site_name', 
#           lat.col = 'site_lattt',
#           lon.col = 'site_lon',
#           x = x_no_coverage,
#           write.file = FALSE
#         ) 
#       )
#     )
#   )
#   
#   # Misspelled lon.col results in error
#   
#   expect_error(
#     suppressMessages(
#       suppressWarnings(
#         extract_geocoverage(
#           data.table = 'nitrogen.csv', 
#           site.col = 'site_name', 
#           lat.col = 'site_lat',
#           lon.col = 'site_lonnn',
#           x = x_no_coverage,
#           write.file = FALSE
#         ) 
#       )
#     )
#   )
#   
#   # write.file = TRUE result in error
#   
#   expect_error(
#     suppressMessages(
#       suppressWarnings(
#         extract_geocoverage(
#           data.table = 'nitrogen.csv', 
#           site.col = 'site_name', 
#           lat.col = 'site_lat',
#           lon.col = 'site_lon',
#           x = x_no_coverage,
#           write.file = TRUE
#         ) 
#       )
#     )
#   )
#   
#   # Deprecated arguments result in warning
#   
#   expect_warning(
#     suppressMessages(
#       extract_geocoverage(
#         data.file = 'nitrogen.csv', 
#         site.col = 'site_name', 
#         lat.col = 'site_lat',
#         lon.col = 'site_lon',
#         x = x_no_coverage,
#         write.file = FALSE
#       )
#     )
#   )
#   
#   # Deprecated arguments result in valid outputs
#   
#   output <- suppressWarnings(
#     suppressMessages(
#       extract_geocoverage(
#         data.file = 'nitrogen.csv', 
#         site.col = 'site_name', 
#         lat.col = 'site_lat',
#         lon.col = 'site_lon',
#         x = x_no_coverage,
#         write.file = FALSE
#       )
#     )
#   )
#   
#   expect_equal(
#     'geographic_coverage.txt' %in% names(output$template),
#     TRUE
#   )
#   
#   expect_equal(
#     class(
#       output$template$geographic_coverage.txt$content
#     ),
#     'data.frame'
#   )
#   
#   expect_equal(
#     all(
#       colnames(output$template$geographic_coverage.txt$content) %in%
#         c('latitude', 'longitude', 'site')
#     ),
#     TRUE
#   )
#   
#   expect_equal(
#     nrow(output$template$geographic_coverage.txt$content) > 1,
#     TRUE
#   )
#   
#   # Existing geographic_coverage.txt results in messages
#   
#   expect_message(
#     suppressWarnings(
#       extract_geocoverage(
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
#   # Non-numeric characters in latitude column result in error
#   
#   input <- x_list
#   
#   input$template$geographic_coverage.txt <- NULL
#   
#   input$data.table$nitrogen.csv$content$site_lat[1:5] <- 'Ooops'
#   
#   expect_error(
#     suppressWarnings(
#       suppressMessages(
#         extract_geocoverage(
#           data.table = 'nitrogen.csv', 
#           site.col = 'site_name', 
#           lat.col = 'site_lat',
#           lon.col = 'site_lon',
#           x = input,
#           write.file = FALSE
#         ) 
#       )
#     )
#   )
#   
#   # Non-numeric characters in longitude column result in error
#   
#   input <- x_list
#   
#   input$template$geographic_coverage.txt <- NULL
#   
#   input$data.table$nitrogen.csv$content$site_lon[1:5] <- 'Ooops'
#   
#   expect_error(
#     suppressWarnings(
#       suppressMessages(
#         extract_geocoverage(
#           data.table = 'nitrogen.csv', 
#           site.col = 'site_name', 
#           lat.col = 'site_lat',
#           lon.col = 'site_lon',
#           x = input,
#           write.file = FALSE
#         ) 
#       )
#     )
#   )
#   
#   # Writing to file results in messages
#   
#   expect_message(
#     suppressWarnings(
#       extract_geocoverage(
#         path = tempdir(), 
#         data.path = system.file(
#           '/examples/data',
#           package = 'EMLassemblyline'
#         ), 
#         data.table = 'nitrogen.csv', 
#         site.col = 'site_name', 
#         lat.col = 'site_lat',
#         lon.col = 'site_lon',
#         write.file = TRUE
#       ) 
#     )
#   )
# 
# })
# 
