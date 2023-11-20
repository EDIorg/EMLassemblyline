
context('Create categorical variables template for NetCDF files')
library(EMLassemblyline)

# Test usage with file inputs -------------------------------------------------

testthat::test_that('Test usage with file inputs', {

  # write.file = TRUE writes files to path

  # Allow the creation of the catvars_netcdf_test_file.txt file by changing 
  # the class of Var0Dim0 from 'character' to 'categorical' in the 
  # attributes_netcdf_test_file.txt file
  
  temptest <- paste0(tempdir(), '/catvars_netcdf_test')
  
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
      file = paste0(temptest, '/templates/attributes_netcdf_test_file.txt'),
      fill = TRUE,
      blank.lines.skip = TRUE,
      sep = "\t",
      quote = "",
      colClasses = list(
        character = 1:utils::count.fields(paste0(temptest, '/templates/attributes_netcdf_test_file.txt'), sep = "\t")[1]
      )
    )
  )
  
  attribute.template$class[attribute.template$attributeName == "Var0Dim0"] <- "categorical"
  
  data.table::fwrite(
    x = attribute.template,
    file = paste0(temptest, '/templates/attributes_netcdf_test_file.txt'),
    sep = "\t",
    quote = FALSE
  )
  
  expect_message(
    suppressWarnings(
      template_netcdf_catvars(
        path = paste0(
          temptest,
          '/templates'
        ),
        data.path = system.file(
          '/examples/data',
          package = 'EMLassemblyline'
        ),
        netcdf = setNames(
          c('netcdf_test_file.nc'),
          c('attributes_netcdf_test_file.txt')
        ),
        write.file = TRUE
      )
    )
  )

  # Correct argument use results in messages

  expect_message(
    suppressWarnings(
      template_netcdf_catvars(
        path = system.file(
          '/examples/templates',
          package = 'EMLassemblyline'
        ),
        data.path = system.file(
          '/examples/data',
          package = 'EMLassemblyline'
        ),
        netcdf = setNames(
          c('netcdf_test_file.nc'),
          c('attributes_netcdf_test_file.txt')
        ),
        write.file = FALSE
      )
    )
  )

  # Missing path results in error

  expect_error(
    suppressMessages(
      template_netcdf_catvars(
        data.path = system.file(
          '/examples/data',
          package = 'EMLassemblyline'
        ),
        netcdf = setNames(
          c('netcdf_test_file.nc'),
          c('attributes_netcdf_test_file.txt')
        ),
        write.file = FALSE
      )
    )
  )

  # Missing attribute files from path

  expect_error(
    suppressMessages(
      template_netcdf_catvars(
        path = system.file(
          '/examples/data',
          package = 'EMLassemblyline'
        ),
        data.path = system.file(
          '/examples/data',
          package = 'EMLassemblyline'
        ),
        netcdf = setNames(
          c('netcdf_test_file.nc'),
          c('attributes_netcdf_test_file.txt')
        ),
        write.file = FALSE
      )
    )
  )

  unlink(temptest, recursive = T, force = T)
  
})
