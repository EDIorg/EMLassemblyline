context('Validate templates')
library(EMLassemblyline)


# Required templates ----------------------------------------------------------

testthat::test_that("Required templates", {
  
  # Parameterize
  
  attr_tmp <- read_template_attributes()
  r_tmp <- attr_tmp[attr_tmp$required_template, ]
  
  # When some required templates are missing
  
  x <- template_arguments(
    path = system.file(
      '/examples/templates',
      package = 'EMLassemblyline'))$x
  for (i in 1:length(x$template)) {
    x1 <- x
    n <- names(x1$template[i])
    if (stringr::str_detect(n, paste(r_tmp$regexpr, collapse = "|"))) {
      x1$template[i] <- NULL
      expect_error(
        validate_templates("make_eml", x1))
    }
  }
  
  # When x$templates = NULL
  
  x <- template_arguments()$x
  expect_error(
    validate_templates("make_eml", x))
  
})

# attributes.txt --------------------------------------------------------------

testthat::test_that("attributes.txt", {
  
  # Parameterize
  
  attr_tmp <- read_template_attributes()
  x <- template_arguments(
    path = system.file(
      '/examples/pkg_260/metadata_templates',
      package = 'EMLassemblyline'),
    data.path = system.file(
      '/examples/pkg_260/data_objects',
      package = 'EMLassemblyline'),
    data.table = c("decomp.csv", "nitrogen.csv"),
    other.entity = c("ancillary_data.zip", "processing_and_analysis.R"))$x
  x1 <- x
  expect_null(validate_templates("make_eml", x1))
  
  # attributes.txt - attributes.txt should be present for each data table
  
  x1 <- x
  x1$template$attributes_decomp.txt <- NULL
  x1$template$attributes_nitrogen.txt <- NULL
  expect_error(validate_templates("make_eml", x1))
  
  # attributeName - All table columns are listed as attributeName
  
  x1 <- x
  x1$template$attributes_decomp.txt$content <- x1$template$attributes_decomp.txt$content[1:2, ]
  x1$template$attributes_nitrogen.txt$content <- x1$template$attributes_nitrogen.txt$content[1:2, ]
  expect_error(validate_templates("make_eml", x1))
  
  # attributeName - Names follow best practices
  
  x1 <- x
  n <- stringr::str_replace(names(x1$data.table$decomp.csv$content), "_", " ")
  n <- stringr::str_replace(n, "t", "%")
  names(x1$data.table$decomp.csv$content) <- n
  x1$template$attributes_decomp.txt$content$attributeName <- n
  expect_warning(validate_templates("make_eml", x1))
  
  # definition- Each attribute has a definition
  
  x1 <- x
  x1$template$attributes_decomp.txt$content$attributeDefinition[1] <- ""
  x1$template$attributes_nitrogen.txt$content$attributeDefinition[1] <- ""
  expect_error(validate_templates("make_eml", x1))
  
  # class - Each attribute has a class
  
  x1 <- x
  x1$template$attributes_decomp.txt$content$class[1] <- ""
  x1$template$attributes_nitrogen.txt$content$class[1] <- ""
  expect_error(validate_templates("make_eml", x1))
  
  # class - Each class is numeric, Date, character, or categorical
  
  x1 <- x
  x1$template$attributes_decomp.txt$content$class[1] <- "dateagorical"
  x1$template$attributes_nitrogen.txt$content$class[1] <- "numerecter"
  expect_error(validate_templates("make_eml", x1))
  
  # class - Each Date class has a dateTimeformatString
  
  x1 <- x
  x1$template$attributes_decomp.txt$content$dateTimeFormatString[
    tolower(x1$template$attributes_decomp.txt$content$class) == "date"
    ] <- ""
  expect_error(validate_templates("make_eml", x1))
  
  # unit - Numeric classed attributes have units
  
  x1 <- x
  x1$template$attributes_decomp.txt$content$unit[6] <- ""
  expect_error(validate_templates("make_eml", x1))
  
  # unit - Units should be from the dictionary or defined in custom_units.txt
  
  x1 <- x
  x1$template$attributes_nitrogen.txt$content$unit[5] <- "an_undefined_unit"
  x1$template$attributes_nitrogen.txt$content$unit[6] <- "another_undefined_unit"
  expect_error(validate_templates("make_eml", x1))
  x1 <- x
  x1$template$custom_units.txt$content[nrow(x1$template$custom_units.txt$content)+1, ] <- c(
    "an_undefined_unit", "of some type", "with some parent SI", "a multiplier",
    "and a description")
  x1$template$custom_units.txt$content[nrow(x1$template$custom_units.txt$content)+1, ] <- c(
    "another_undefined_unit", "of some type", "with some parent SI", 
    "a multiplier", "and a description")
  expect_null(validate_templates("make_eml", x1))
  
  # dateTimeFormatString- Remaining dateTimeFormatString prompts have been removed
  
  x1 <- x
  x1$template$attributes_decomp.txt$content$dateTimeFormatString[1] <- 
    "!Add datetime specifier here!"
  x1$template$attributes_nitrogen.txt$content$dateTimeFormatString[1] <- 
    "!Add datetime specifier here!"
  expect_error(validate_templates("make_eml", x1))
  
  # missingValueCode - Each missingValueCode has a missingValueCodeExplanation
  
  x1 <- x
  x1$template$attributes_decomp.txt$content$missingValueCodeExplanation[1] <- ""
  x1$template$attributes_nitrogen.txt$content$missingValueCodeExplanation[1] <- ""
  expect_error(validate_templates("make_eml", x1))
  
  # missingValueCode - Each missingValueCode only has 1 entry per column
  
  x1 <- x
  x1$template$attributes_decomp.txt$content$missingValueCode[1] <- "NA, -99999"
  x1$template$attributes_nitrogen.txt$content$missingValueCode[1] <- "NA -99999"
  expect_error(validate_templates("make_eml", x1))
  
  # missingValueCodeExplanation - Each missingValueCodeExplanation has a 
  # non-blank missingValueCode
  
  x1 <- x
  x1$template$attributes_decomp.txt$content$missingValueCode[1] <- ""
  x1$template$attributes_nitrogen.txt$content$missingValueCode[1] <- ""
  expect_error(validate_templates("make_eml", x1))
  
})

# catvars.txt -----------------------------------------------------------------

testthat::test_that("Categorical variables", {
  
  # Parameterize
  
  attr_tmp <- read_template_attributes()
  x <- template_arguments(
    path = system.file(
      '/examples/pkg_260/metadata_templates',
      package = 'EMLassemblyline'),
    data.path = system.file(
      '/examples/pkg_260/data_objects',
      package = 'EMLassemblyline'),
    data.table = c("decomp.csv", "nitrogen.csv"),
    other.entity = c("ancillary_data.zip", "processing_and_analysis.R"))$x
  x1 <- x
  expect_null(validate_templates("make_eml", x1))
  
  # catvars.txt - Required when table attributes are listed as 
  # "categorical"
  
  x1 <- x
  x1$template$attributes_decomp.txt$content$class[1] <- "categorical"
  x1$template$attributes_nitrogen.txt$content$class[1] <- "categorical"
  x1$template$catvars_decomp.txt <- NULL
  x1$template$catvars_nitrogen.txt <- NULL
  expect_error(validate_templates("make_eml", x1))
  
  # codes - All codes require definition
  
  use_i <- seq(
    length(names(x$template)))[
      stringr::str_detect(
        names(x$template), 
        attr_tmp$regexpr[attr_tmp$template_name == "catvars"])]
  x1 <- x
  for (i in use_i) {
    x1$template[[i]]$content$definition[round(runif(2, 1, nrow(x1$template[[i]]$content)))] <- ""
  }
  expect_error(validate_templates("make_eml", x1))
  
})

# geographic_coverage ---------------------------------------------------------

testthat::test_that("geographic_coverage", {
  
  # Parameterize
  
  attr_tmp <- read_template_attributes()
  x <- template_arguments(
    system.file(
      '/examples/pkg_260/metadata_templates',
      package = 'EMLassemblyline'))$x
  x1 <- x
  expect_null(validate_templates("make_eml", x1))
  
  # geographicDescription - Each entry requires a north, south, east, and west 
  # bounding coordinate
  
  x1 <- x
  x1$template$geographic_coverage.txt$content$northBoundingCoordinate[1] <- ""
  x1$template$geographic_coverage.txt$content$southBoundingCoordinate[2] <- ""
  expect_error(validate_templates("make_eml", x1))
  
  # coordinates - Decimal degree is expected
  
  x1 <- x
  x1$template$geographic_coverage.txt$content$northBoundingCoordinate[1] <- "45 23'"
  x1$template$geographic_coverage.txt$content$southBoundingCoordinate[2] <- "23 degrees 23 minutes"
  expect_error(validate_templates("make_eml", x1))

})

# personnel -------------------------------------------------------------------

testthat::test_that("personnel", {
  
  # Parameterize
  
  attr_tmp <- read_template_attributes()
  x <- template_arguments(
    system.file(
      '/examples/pkg_260/metadata_templates',
      package = 'EMLassemblyline'))$x
  x1 <- x
  expect_null(validate_templates("make_eml", x1))
  
  # role - At least one creator and contact is listed
  
  x1 <- x
  x1$template$personnel.txt$content$role[
    stringr::str_detect(
      x1$template$personnel.txt$content$role, 
      "contact")] <- "creontact"
  expect_error(validate_templates("make_eml", x1))
  x1 <- x
  x1$template$personnel.txt$content$role[
    stringr::str_detect(
      x1$template$personnel.txt$content$role, 
      "creator")] <- "creontact"
  expect_error(validate_templates("make_eml", x1))
  
  # role - All personnel have roles
  
  x1 <- x
  x1$template$personnel.txt$content$role[
    stringr::str_detect(
      x1$template$personnel.txt$content$role, 
      "PI|pi")] <- ""
  expect_error(validate_templates("make_eml", x1))

})

# remove_empty_templates() ----------------------------------------------------

testthat::test_that("remove_empty_templates()", {
  
  x <- template_arguments(
    path = system.file(
      '/examples/templates', 
      package = 'EMLassemblyline'))$x
  for (i in 1:length(x$template)) {
    x1 <- x
    n <- names(x1$template[i])
    x1$template[[i]]$content <- NULL
    x1 <- remove_empty_templates(x1)
    expect_true(!any(stringr::str_detect(names(x1$template), n)))
  }
  
  x <- template_arguments(empty = T)$x
  x <- remove_empty_templates(x)
  expect_true(is.null(x$template))

})
