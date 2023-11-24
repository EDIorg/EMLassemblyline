context('Create categorical variables template for raster files')
library(EMLassemblyline)

# Test usage with file inputs -------------------------------------------------

testthat::test_that('Test usage with file inputs', {
  
  # write.file = TRUE writes files to path
  
  # Allow the creation of the catvars_geotiff_test_file.txt file by changing 
  # the class of Present.Surface.pH from 'numeric' to 'categorical' in the 
  # attributes_geotiff_test_file.txt file
  
  temptest <- paste0(tempdir(), '/catvars_raster_test')
  
  dir.create(temptest)
  
  file.copy(
    from = system.file(
      '/examples/templates',
      package = 'EMLassemblyline'
    ),
    to = temptest,
    recursive = TRUE
  )
  
  attribute.template <- as.data.frame(
    data.table::fread(
      file = paste0(temptest, '/templates/attributes_geotiff_test_file.txt'),
      fill = TRUE,
      blank.lines.skip = TRUE,
      sep = "\t",
      quote = "",
      colClasses = list(
        character = 1:utils::count.fields(paste0(temptest, '/templates/attributes_geotiff_test_file.txt'), sep = "\t")[1]
      )
    )
  )
  
  attribute.template$class[attribute.template$attributeName == "geotiff_test_file"] <- "categorical"
  
  data.table::fwrite(
    x = attribute.template,
    file = paste0(temptest, '/templates/attributes_geotiff_test_file.txt'),
    sep = "\t",
    quote = FALSE
  )
  
  expect_message(
    suppressWarnings(
      template_raster_catvars(
        path = paste0(
          temptest,
          '/templates'
        ), 
        data.path = system.file(
          '/examples/data',
          package = 'EMLassemblyline'
        ),
        raster.file = setNames(
          c('geotiff_test_file.tif'), 
          c('attributes_geotiff_test_file.txt')
        ),
        write.file = TRUE
      ) 
    )
  )
  
  # Correct argument use results in messages
  
  expect_message(
    suppressWarnings(
      template_raster_catvars(
        path = system.file(
          '/examples/templates',
          package = 'EMLassemblyline'
        ), 
        data.path = system.file(
          '/examples/data',
          package = 'EMLassemblyline'
        ),
        raster.file = setNames(
          c('geotiff_test_file.tif'), 
          c('attributes_geotiff_test_file.txt')
        ),
        write.file = FALSE
      )
    )
  )
  
  # Missing path results in error
  
  expect_error(
    suppressMessages(
      template_raster_catvars(
        data.path = system.file(
          '/examples/data',
          package = 'EMLassemblyline'
        ),
        raster.file = setNames(
          c('geotiff_test_file.tif'), 
          c('attributes_geotiff_test_file.txt')
        ),
        write.file = FALSE
      )
    )
  )
  
  # Missing attribute files from path
  
  expect_error(
    suppressMessages(
      template_raster_catvars(
        path = system.file(
          '/examples/data',
          package = 'EMLassemblyline'
        ),
        data.path = system.file(
          '/examples/data',
          package = 'EMLassemblyline'
        ),
        raster.file = setNames(
          c('geotiff_test_file.tif'), 
          c('attributes_geotiff_test_file.txt')
        ),
        write.file = FALSE
      )
    )
  )
  
  unlink(temptest, recursive = T, force = T)
  
})
