context('Validate templates')
library(EMLassemblyline)

# Units -----------------------------------------------------------------------

testthat::test_that('Units of table attributes', {
  
  # Units - Numeric attributes require units ----------------------------------
  
  # Template arguments for a valid set of files
  x1 <- template_arguments(
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
  
  # Remove a unit from a numeric attribute
  x1$x$template$attributes_decomp.txt$content$unit[6] <- ""
  
  # This violation should throw an error
  expect_error(
    validate_templates(
      fun.name = "make_eml",
      x = x1$x
    )
  )
  
  # Units - Units require definition ------------------------------------------
  
  # Template arguments for a valid set of files
  x1 <- template_arguments(
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
  
  # Replace valid units with invalid ones
  x1$x$template$attributes_nitrogen.txt$content$unit[5] <- "an_undefined_unit"
  x1$x$template$attributes_nitrogen.txt$content$unit[6] <- "another_undefined_unit"
  
  # This violation should throw an error
  expect_error(
    validate_templates(
      fun.name = "make_eml",
      x = x1$x
    )
  )

})