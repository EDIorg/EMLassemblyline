context('Validate templates')
library(EMLassemblyline)

# Units -----------------------------------------------------------------------

testthat::test_that('Units of table attributes', {
  
  # Units - missing -----------------------------------------------------------
  # Numeric attributes require units
  
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
  
  # Units - definition --------------------------------------------------------
  
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
  
  # Invalid units should throw an error
  expect_error(
    validate_templates(
      fun.name = "make_eml",
      x = x1$x
    )
  )
  
  # Non-conventional units must be defined in the custom_units.txt template.
  # Adding the above units to custom_units.txt fixes the issue.
  x1$x$template$custom_units.txt$content[nrow(x1$x$template$custom_units.txt$content)+1, ] <- c(
    "an_undefined_unit", 
    "of some type",
    "with some parent SI",
    "a multiplier",
    "and a description"
  )
  
  x1$x$template$custom_units.txt$content[nrow(x1$x$template$custom_units.txt$content)+1, ] <- c(
    "another_undefined_unit", 
    "of some type",
    "with some parent SI",
    "a multiplier",
    "and a description"
  )

  expect_null(
    validate_templates(
      fun.name = "make_eml",
      x = x1$x
    )
  )

})