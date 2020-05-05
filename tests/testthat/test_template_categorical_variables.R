
context('Create categorical variables template')
library(EMLassemblyline)

# Test usage with file inputs -------------------------------------------------

testthat::test_that('Test usage with file inputs', {
  
  # write.file = TRUE writes files to path
  
  file.copy(
    from = system.file(
      '/examples/templates',
      package = 'EMLassemblyline'
    ),
    to = tempdir(),
    recursive = TRUE
  )
  
  file.remove(
    paste0(
      tempdir(),
      '/templates/catvars_decomp.txt'
    )
  )
  
  file.remove(
    paste0(
      tempdir(),
      '/templates/catvars_nitrogen.txt'
    )
  )
  
  expect_message(
    suppressWarnings(
      template_categorical_variables(
        path = paste0(
          tempdir(),
          '/templates'
        ), 
        data.path = system.file(
          '/examples/data',
          package = 'EMLassemblyline'
        ),
        write.file = TRUE
      ) 
    )
  )
  
  # Correct argument use results in messages
  
  expect_message(
    template_categorical_variables(
      path = system.file(
        '/examples/templates',
        package = 'EMLassemblyline'
      ), 
      data.path = system.file(
        '/examples/data',
        package = 'EMLassemblyline'
      ),
      write.file = FALSE
    )
  )
  
  # Missing path results in error
  
  expect_error(
    suppressMessages(
      template_categorical_variables(
        data.path = system.file(
          '/examples/data',
          package = 'EMLassemblyline'
        ),
        write.file = FALSE
      )
    )
  )
  
  # Missing attribute files from path
  
  expect_error(
    suppressMessages(
      template_categorical_variables(
        path = system.file(
          '/examples/data',
          package = 'EMLassemblyline'
        ),
        data.path = system.file(
          '/examples/data',
          package = 'EMLassemblyline'
        ),
        write.file = FALSE
      )
    )
  )
  
})

# Test usage with x inputs ----------------------------------------------------

testthat::test_that('Test usage with x inputs',{

  # Create x_list and x_no_catvars with and without catvars_*.txt, respectivly.

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

  x_no_catvars <- x_list

  x_no_catvars$template$catvars_decomp.txt <- NULL

  x_no_catvars$template$catvars_nitrogen.txt <- NULL

  # All arguments are supported:
  # - /x/templates/catvars_*.txt is created with expected content
  # - path is not added /x/templates/catvars_*.txt/path
  # - data.path is not added to x

  expect_message(
    template_categorical_variables(
      path = system.file(
        '/examples/templates',
        package = 'EMLassemblyline'
      ),
      data.path = system.file(
        '/examples/data',
        package = 'EMLassemblyline'
      ),
      x = x_list,
      write.file = FALSE
    )
  )

  output <- suppressMessages(
    template_categorical_variables(
      path = '/some/path',
      data.path = system.file(
        '/examples/data',
        package = 'EMLassemblyline'
      ),
      x = x_no_catvars,
      write.file = FALSE
    )
  )

  expect_equal(
    sum(
      stringr::str_detect(
        string = names(output$template),
        pattern = 'catvars_.*.txt'
      )
    ),
    2
  )

  expect_equal(
    class(output$template$catvars_decomp.txt$content),
    'data.frame'
  )

  expect_equal(
    all(
      colnames(output$template$catvars_decomp.txt$content) %in%
        c('attributeName', 'code', 'definition')
    ),
    TRUE
  )

  # Missing path adds NA to /x/templates/catvars_*.txt/path

  expect_message(
    template_categorical_variables(
      data.path = system.file(
        '/examples/data',
        package = 'EMLassemblyline'
      ),
      x = x_list,
      write.file = FALSE
    )
  )

  output <- suppressMessages(
    template_categorical_variables(
      data.path = system.file(
        '/examples/data',
        package = 'EMLassemblyline'
      ),
      x = x_no_catvars,
      write.file = FALSE
    )
  )

  # Missing path has no effect

  expect_message(
    template_categorical_variables(
      data.path = system.file(
        '/examples/data',
        package = 'EMLassemblyline'
      ),
      x = x_no_catvars,
      write.file = FALSE
    )
  )

  # Missing path and data.path has no effect
  # - /x/templates/catvars_*.txt is created with expected content

  expect_error(
    suppressMessages(
      template_categorical_variables(
        x = x_no_catvars,
        write.file = FALSE
      )
    )
  )

  # Blank rows of catvars*_.txt are removed from output

  input <- x_no_catvars

  input$data.table$decomp.csv$content$ntrt[
    input$data.table$decomp.csv$content$ntrt == 25
    ] <- ''

  output <- suppressWarnings(
    suppressMessages(
      template_categorical_variables(
        data.path = system.file(
          '/examples/data',
          package = 'EMLassemblyline'
        ),
        x = input,
        write.file = FALSE
      )
    )
  )

  expect_equal(
    any(output$template$catvars_decomp.txt$content$code == '', na.rm = T),
    FALSE
  )

})

# Missing value codes ---------------------------------------------------------
# Missing value codes should not be listed as categorical variables.

testthat::test_that('Missing value codes', {
  
  x <- template_arguments(
    path = system.file(
      '/examples/templates', 
      package = 'EMLassemblyline'),
    data.path = system.file(
      '/examples/data',
      package = 'EMLassemblyline'),
    data.table = c(
      'decomp.csv',
      'nitrogen.csv'))$x
  
  # Helper function for testing this feature - Change missing value code in 
  # attributes.txt, add missing value code to data, write to file, run 
  # template_categorical variables(), check missing value codes are not 
  # included in categorical_variables.txt
  
  test_missing_value_code <- function(data, attribute.template, missing.value.code) {
    
    if (class(missing.value.code) == "numeric") {
      
      attribute.template$missingValueCode[
        attribute.template$class == "categorical"] <- as.character(missing.value.code)
      data[1:5, colnames(data)[
        attribute.template$class == "categorical"]] <- missing.value.code
      dir.create(paste0(tempdir(), '/catvars_test'))
      write.table(
        attribute.template,
        paste0(tempdir(), '/catvars_test/attributes_decomp.txt'),
        sep = '\t',
        row.names = FALSE)
      write.csv(
        data,
        paste0(tempdir(), '/catvars_test/decomp.csv'),
        row.names = FALSE)
      template_categorical_variables(
        path = paste0(tempdir(), '/catvars_test'))
      t <- read.table(
        paste0(tempdir(), '/catvars_test/catvars_decomp.txt'),
        sep = '\t', 
        header = TRUE,
        as.is = TRUE)
      expect_true(!any(missing.value.code %in% t$code))
      
    } else if (class(missing.value.code) == "character") {
      
      attribute.template$missingValueCode[
        attribute.template$class == "categorical"] <- missing.value.code
      data[1:5, colnames(data)[
        attribute.template$class == "categorical"]] <- missing.value.code
      dir.create(paste0(tempdir(), '/catvars_test'))
      write.table(
        attribute.template,
        paste0(tempdir(), '/catvars_test/attributes_decomp.txt'),
        sep = '\t',
        row.names = FALSE)
      write.csv(
        data,
        paste0(tempdir(), '/catvars_test/decomp.csv'),
        row.names = FALSE)
      template_categorical_variables(
        path = paste0(tempdir(), '/catvars_test'))
      t <- read.table(
        paste0(tempdir(), '/catvars_test/catvars_decomp.txt'),
        sep = '\t', 
        header = TRUE,
        as.is = TRUE)
      expect_true(!any(missing.value.code %in% t$code))
      
    } else if (missing.value.code == "NA") {
      
      attribute.template$missingValueCode[
        attribute.template$class == "categorical"] <- missing.value.code
      data[1:5, colnames(data)[
        attribute.template$class == "categorical"]] <- NA
      dir.create(paste0(tempdir(), '/catvars_test'))
      write.table(
        attribute.template,
        paste0(tempdir(), '/catvars_test/attributes_decomp.txt'),
        sep = '\t',
        row.names = FALSE)
      write.csv(
        data,
        paste0(tempdir(), '/catvars_test/decomp.csv'),
        row.names = FALSE)
      template_categorical_variables(
        path = paste0(tempdir(), '/catvars_test'))
      t <- read.table(
        paste0(tempdir(), '/catvars_test/catvars_decomp.txt'),
        sep = '\t', 
        header = TRUE,
        as.is = TRUE)
      expect_true(!any(missing.value.code %in% t$code))
      
    }
    unlink(paste0(tempdir(), '/catvars_test'), recursive = T, force = T)
  }
  
  # Missing value code = -999 (interpreted as numeric within the data and 
  # character in attributes.txt)
  
  test_missing_value_code(
    data = x$data.table$decomp.csv$content,
    attribute.template = x$template$attributes_decomp.txt$content,
    missing.value.code = -999)
  
  # Missing value code = "No data"
  
  test_missing_value_code(
    data = x$data.table$decomp.csv$content,
    attribute.template = x$template$attributes_decomp.txt$content,
    missing.value.code = "No data")
  
  # Missing value code = "NA" (interpreted as NA in the data)
  
  test_missing_value_code(
    data = x$data.table$decomp.csv$content,
    attribute.template = x$template$attributes_decomp.txt$content,
    missing.value.code = "NA")

})
