context('Extract geographical coverage')
library(EMLassemblyline)

# Test usage with file inputs -------------------------------------------------

testthat::test_that('Test usage with file inputs', {
  
  # Clean tempdir() of geographic_coverage.txt
  
  unlink(
    paste0(
      tempdir(),
      '/geographic_coverage.txt'
    ),
    force = TRUE
  )
  
  # Missing path results in error
  
  expect_error(
    suppressMessages(
      template_geographic_coverage(
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
  
  # Missing data.path results in error
  
  expect_error(
    suppressMessages(
      template_geographic_coverage(
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
  
  # Missing data.table results in error
  
  expect_error(
    suppressMessages(
      template_geographic_coverage(
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
  
  # Missing site.col results in error
  
  expect_error(
    suppressMessages(
      template_geographic_coverage(
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
  
  # Missing lat.col results in error
  
  expect_error(
    suppressMessages(
      template_geographic_coverage(
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
  
  # Missing lon.col results in error
  
  expect_error(
    suppressMessages(
      template_geographic_coverage(
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
  
  # Misspelled site.col results in error
  
  expect_error(
    suppressMessages(
      template_geographic_coverage(
        path = tempdir(), 
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
  
  # Misspelled lat.col results in error
  
  expect_error(
    suppressMessages(
      template_geographic_coverage(
        path = tempdir(), 
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
  
  # Misspelled lon.col results in error
  
  expect_error(
    suppressMessages(
      template_geographic_coverage(
        path = tempdir(), 
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
  
  # Mismatched data.table and site.col length results in error
  
  expect_error(
    suppressMessages(
      template_geographic_coverage(
        path = tempdir(), 
        data.path = system.file(
          '/examples/data',
          package = 'EMLassemblyline'
        ), 
        data.table = 'nitrogen.csv', 
        site.col = c('site_name', 'site_name'), 
        lat.col = 'site_lat',
        lon.col = 'site_lon',
        write.file = FALSE
      )
    )
  )
  
  # Mismatched data.table and lat.col length results in error
  
  expect_error(
    suppressMessages(
      template_geographic_coverage(
        path = tempdir(), 
        data.path = system.file(
          '/examples/data',
          package = 'EMLassemblyline'
        ), 
        data.table = 'nitrogen.csv', 
        site.col = 'site_name', 
        lat.col = c('site_lat', 'site_lat'),
        lon.col = 'site_lon',
        write.file = FALSE
      ) 
    )
  )
  
  # Mismatched data.table and lon.col length results in error
  
  expect_error(
    suppressMessages(
      template_geographic_coverage(
        path = tempdir(), 
        data.path = system.file(
          '/examples/data',
          package = 'EMLassemblyline'
        ), 
        data.table = 'nitrogen.csv', 
        site.col = 'site_name', 
        lat.col = 'site_lat',
        lon.col = c('site_lon', 'site_lon'),
        write.file = FALSE
      )
    )
  )
  
  # Valid arguments result in messages
  
  expect_message(
    template_geographic_coverage(
      path = tempdir(), 
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
  
  # Writing to file results in messages

  expect_message(
    template_geographic_coverage(
      path = tempdir(), 
      data.path = system.file(
        '/examples/data',
        package = 'EMLassemblyline'
      ), 
      data.table = 'nitrogen.csv', 
      site.col = 'site_name', 
      lat.col = 'site_lat',
      lon.col = 'site_lon',
      write.file = TRUE
    )
  )
  
  unlink(
    paste0(
      tempdir(),
      '/geographic_coverage.txt'
    ),
    force = TRUE
  )
  
  # Multiple geography files with duplicate sites results in warnings
  
  expect_warning(
    template_geographic_coverage(
      path = tempdir(), 
      data.path =  testthat::test_path("fixtures"), 
      data.table = c('sites.csv', 'subsites.csv'), 
      site.col = c('siteID', 'subsiteID'), 
      lat.col = c('decimalLatitude', 'decimalLatitude'),
      lon.col = c('decimalLongitude', 'decimalLongitude'),
      write.file = FALSE
    )
  )
  
  # Multiple geography files with no duplicate sites results in no warnings
  
  expect_no_warning(
    template_geographic_coverage(
      path = tempdir(), 
      data.path =  testthat::test_path("fixtures"), 
      data.table = c('sites.csv', 'subsites_no_duplicates.csv'), 
      site.col = c('siteID', 'subsiteID'), 
      lat.col = c('decimalLatitude', 'decimalLatitude'),
      lon.col = c('decimalLongitude', 'decimalLongitude'),
      write.file = FALSE
    )
  )
  
})

# Test usage with x inputs ----------------------------------------------------

testthat::test_that('Test usage with x inputs', {
  
  # Create x_list and x_no_coverage with and without geographic_coverage.txt, 
  # respectivly
  
  x_list <- template_arguments(
    path = system.file(
      '/examples/templates', 
      package = 'EMLassemblyline'
    ),
    data.path = system.file(
      '/examples/data',
      package = 'EMLassemblyline'
    ),
    data.table = c(
      'decomp.csv',
      'nitrogen.csv'
    )
  )
  
  x_list <- x_list$x
  
  x_no_coverage <- x_list
  
  x_no_coverage$template$geographic_coverage.txt <- NULL
  
  # All arguments are supported:
  # - /x/templates/geographic_coverage.txt is created with expected content
  
  expect_message(
    template_geographic_coverage(
      path = '/some/path',
      data.path = '/some/data.path', 
      data.table = 'nitrogen.csv', 
      site.col = 'site_name',
      lat.col = 'site_lat', 
      lon.col = 'site_lon',
      x = x_no_coverage,
      write.file = FALSE
    )
  )
  
  output <- suppressMessages(
    template_geographic_coverage(
      path = '/some/path',
      data.path = '/some/data.path', 
      data.table = 'nitrogen.csv', 
      site.col = 'site_name',
      lat.col = 'site_lat', 
      lon.col = 'site_lon',
      x = x_no_coverage,
      write.file = FALSE
    )
  )
  
  expect_equal(
    class(output$template$geographic_coverage.txt$content),
    'data.frame'
  )
  
  expect_equal(
    all(
      colnames(output$template$geographic_coverage.txt$content) %in%
        c('northBoundingCoordinate', 'southBoundingCoordinate', 
          'eastBoundingCoordinate', 'westBoundingCoordinate', 'geographicDescription')
    ),
    TRUE
  )
  
  # Missing path adds NA to /x/templates/geographic_coverage.txt/path
  
  expect_message(
    template_geographic_coverage(
      data.path = system.file(
        '/examples/data',
        package = 'EMLassemblyline'
      ), 
      data.table = 'nitrogen.csv', 
      site.col = 'site_name',
      lat.col = 'site_lat', 
      lon.col = 'site_lon',
      x = x_no_coverage,
      write.file = FALSE
    )
  )
  
  output <- suppressMessages(
    template_geographic_coverage(
      data.path = system.file(
        '/examples/data',
        package = 'EMLassemblyline'
      ), 
      data.table = 'nitrogen.csv', 
      site.col = 'site_name',
      lat.col = 'site_lat', 
      lon.col = 'site_lon',
      x = x_no_coverage,
      write.file = FALSE
    )
  )
  
  # Missing data.path has no effect
  
  expect_message(
    template_geographic_coverage(
      data.table = 'nitrogen.csv', 
      site.col = 'site_name',
      lat.col = 'site_lat', 
      lon.col = 'site_lon',
      x = x_no_coverage,
      write.file = FALSE
    )
  )
  
  # Missing data.table results in error
  
  expect_error(
    suppressMessages(
      template_geographic_coverage(
        site.col = 'site_name',
        lat.col = 'site_lat', 
        lon.col = 'site_lon',
        x = x_no_coverage,
        write.file = FALSE
      )
    )
  )
  
  # Missing site.col results in error
  
  expect_error(
    suppressMessages(
      template_geographic_coverage(
        data.table = 'nitrogen.csv', 
        lat.col = 'site_lat', 
        lon.col = 'site_lon',
        x = x_no_coverage,
        write.file = FALSE
      )
    )
  )
  
  # Missing lat.col results in error
  
  expect_error(
    suppressMessages(
      template_geographic_coverage(
        data.table = 'nitrogen.csv', 
        site.col = 'site_name', 
        lon.col = 'site_lon',
        x = x_no_coverage,
        write.file = FALSE
      )
    )
  )
  
  # Missing lon.col results in error
  
  expect_error(
    suppressMessages(
      template_geographic_coverage(
        data.table = 'nitrogen.csv', 
        site.col = 'site_name', 
        lat.col = 'site_lat',
        x = x_no_coverage,
        write.file = FALSE
      )
    )
  )
  
  # Misspelled site.col results in error
  
  expect_error(
    suppressMessages(
      template_geographic_coverage(
        data.table = 'nitrogen.csv', 
        site.col = 'site_nameeee', 
        lat.col = 'site_lat',
        lon.col = 'site_lon',
        x = x_no_coverage,
        write.file = FALSE
      )
    )
  )
  
  # Misspelled lat.col results in error
  
  expect_error(
    suppressMessages(
      template_geographic_coverage(
        data.table = 'nitrogen.csv', 
        site.col = 'site_name', 
        lat.col = 'site_lattt',
        lon.col = 'site_lon',
        x = x_no_coverage,
        write.file = FALSE
      )
    )
  )
  
  # Misspelled lon.col results in error
  
  expect_error(
    suppressMessages(
      template_geographic_coverage(
        data.table = 'nitrogen.csv', 
        site.col = 'site_name', 
        lat.col = 'site_lat',
        lon.col = 'site_lonnn',
        x = x_no_coverage,
        write.file = FALSE
      )
    )
  )
  
  # write.file = TRUE result in error
  
  expect_error(
    suppressMessages(
      template_geographic_coverage(
        data.table = 'nitrogen.csv', 
        site.col = 'site_name', 
        lat.col = 'site_lat',
        lon.col = 'site_lon',
        x = x_no_coverage,
        write.file = TRUE
      )
    )
  )
  
  # Existing geographic_coverage.txt results in messages
  
  expect_message(
    suppressWarnings(
      template_geographic_coverage(
        data.table = 'nitrogen.csv', 
        site.col = 'site_name', 
        lat.col = 'site_lat',
        lon.col = 'site_lon',
        x = x_list,
        write.file = FALSE
      ) 
    )
  )

})

# Test usage with 'empty = TRUE' ----------------------------------------------

testthat::test_that('Test usage with "empty = TRUE"', {
  
  # Clean tempdir() of geographic_coverage.txt
  
  unlink(
    paste0(
      tempdir(),
      '/geographic_coverage.txt'
    ),
    force = TRUE
  )
  
  # Writing to file results in messages
  
  expect_message(
    template_geographic_coverage(
      path = tempdir(), 
      empty = TRUE
    )
  )
  
  # File has column names and no rows
  
  input <- utils::read.table(
    paste0(
      tempdir(), 
      '/geographic_coverage.txt'
    ),
    header = T,
    sep="\t",
    quote="\"",
    as.is=TRUE,
    comment.char = "",
    fill = T,
    na.strings = "NA",
    fileEncoding = "UTF-8"
  )
  
  expect_true(
    all(colnames(input) %in% 
          c('geographicDescription', 'northBoundingCoordinate', 
            'southBoundingCoordinate', 'eastBoundingCoordinate',
            'westBoundingCoordinate'))
  )
  
  expect_equal(
    nrow(input),
    0
  )

})

# Coerce lat.col & lon.col to numeric -----------------------------------------

testthat::test_that("Coerce lat.col & lon.col to numeric", {
  
  # Add character strings to numeric columns of the example data and expect 
  # these rows to be missing from the resultant template.
  
  unlink(
    paste0(tempdir(), c("/nitrogen.csv", "/geographic_coverage.txt")),
    force = TRUE)
  
  # Add character strings to numeric columns of example data
  
  d <- data.table::fread(
    system.file(
      '/examples/pkg_260/data_objects/nitrogen.csv', 
      package = 'EMLassemblyline'))
  d$site_lat[1] <- "a_character_string"
  d$site_lon[2] <- "a_character_string"
  
  data.table::fwrite(d, paste0(tempdir(), '/nitrogen.csv'))
  
  template_geographic_coverage(
    path = tempdir(),
    data.table = 'nitrogen.csv', 
    site.col = 'site_name',
    lat.col = 'site_lat', 
    lon.col = 'site_lon')
  
  template <- data.table::fread(paste0(tempdir(), "/geographic_coverage.txt"))
  
  expect_true(
    all(!(d$site_name[1:2] %in% template$geographicDescription)))
  
})





