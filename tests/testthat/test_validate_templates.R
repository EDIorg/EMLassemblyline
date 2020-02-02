context('Validate templates')
library(EMLassemblyline)

# annotations -----------------------------------------------------------------

testthat::test_that("annotations.txt", {
  
  # Parameterize
  
  x <- template_arguments(
    path = system.file(
      "/examples/pkg_260/metadata_templates", 
      package = "EMLassemblyline"
    ),
    data.path = system.file(
      "/examples/pkg_260/data_objects",
      package = "EMLassemblyline"
    ),
    data.table = c(
      "decomp.csv",
      "nitrogen.csv"
    ),
    other.entity = c(
      "ancillary_data",
      "processing_and_analysis"
    )
  )$x
  
  defs <- as.data.frame(
    data.table::fread(
      file = system.file(
        '/templates/annotation_characteristics.txt',
        package = 'EMLassemblyline'
      ),
      colClasses = rep(
        "character",
        max(
          utils::count.fields(
            system.file(
              '/templates/annotation_characteristics.txt',
              package = 'EMLassemblyline'
            ),
            sep = "\t"
          )
        )
      ),
      fill = TRUE,
      blank.lines.skip = TRUE
    )
  )
  
  # annotations - missing template --------------------------------------------
  
  expect_warning(
    validate_templates(
      fun.name = "make_eml",
      x = as.null(x$template$annotations.txt)
    )
  )
  
  # annotations - incomplete annotations --------------------------------------
  
  x1 <- x
  x1$template$annotations.txt$content$object_uri[c(1,2)] <- NA_character_
  
  expect_warning(
    validate_templates(
      fun.name = "make_eml",
      x = x1
    )
  )
  
  rm(x1)
  
  # annotations - resolvable URIs ---------------------------------------------
  
  # x1 <- x
  # x1$template$annotations.txt$content$predicate_uri[1] <- "Not a uri"
  # 
  # expect_warning(
  #   validate_templates(
  #     fun.name = "make_eml",
  #     x = x1
  #   )
  # )
  # 
  # x1$template$annotations.txt$content$object_uri[1] <- "Not a uri"
  # x1$template$annotations.txt$content$predicate_uri[1] <- 
  #   x1$template$annotations.txt$content$predicate_uri[2]
  # 
  # expect_warning(
  #   validate_templates(
  #     fun.name = "make_eml",
  #     x = x1
  #   )
  # )
  # 
  # rm(x1)
  
  # annotations - elements ----------------------------------------------------
  # Current list of supported elements 
  # (see /inst/templates/annotation_characteristics.txt)
  
  test_elements <- function(element, x) {
    
    if (element == "dataset") {
      
      x1 <- x
      x1$template$annotations.txt$content$element[
        x1$template$annotations.txt$content$element == element
        ] <- "dataTable"
      expect_warning(
        validate_templates(
          fun.name = "make_eml",
          x = x1
        )
      )
      rm(x1)
      
    } else {
      
      x1 <- x
      x1$template$annotations.txt$content$element[
        x1$template$annotations.txt$content$element == element
        ] <- "dataset"
      expect_warning(
        validate_templates(
          fun.name = "make_eml",
          x = x1
        )
      )
      rm(x1)
      
    }
    
  }
  
  x1 <- lapply(
    unique(defs$element),
    test_elements,
    x = x
  )
  
  rm(x1)

  
})


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