context('Validate templates')
library(EMLassemblyline)

# validate_templates() --------------------------------------------------------

testthat::test_that("validate_templates()", {
  
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
  
  # Valid inputs result in equivalent outputs
  
  r <- validate_templates("make_eml", x1)
  
  # The validation report is a compilation of template issues. A few issues
  # are created to demonstrate this.
  
  x1 <- x
  # Missing abstract
  x1$template$abstract.txt <- NULL
  # Missing attribute names
  x1$template$attributes_decomp.txt$content <- 
    x1$template$attributes_decomp.txt$content[1:3, ]
  # Missing creator
  x1$template$personnel.txt$content$role[
    stringr::str_detect(
      x1$template$personnel.txt$content$role, 
      "creator")] <- "creontact"
  
  # Call validate_templates()
  r <- suppressWarnings(
    validate_templates("make_eml", x1))
  
  # Messages are compiled into an "issues" object in the global environment
  expect_true(
    any(
      stringr::str_detect(
        template_issues,
        "Missing abstract. An abstract describing the data is recommended ")))
  expect_true(
    any(
      stringr::str_detect(
        template_issues,
        "Mising attribute names. These attributes are listed in the data but ")))
  expect_true(
    any(
      stringr::str_detect(
        template_issues,
        "Missing creator. At least one creator is required.")))
  
  # Required issues result in dropped objects
  expect_null(r$x$template$personnel.txt)
  expect_null(r$x$template$abstract.txt)
  expect_null(r$x$template$attributes_decomp.txt)

})

# abstract --------------------------------------------------------------------

testthat::test_that("abstract", {
  
  # Parameterize

  x <- template_arguments(
    path = system.file(
      '/examples/pkg_260/metadata_templates',
      package = 'EMLassemblyline'),
    data.path = system.file(
      '/examples/pkg_260/data_objects',
      package = 'EMLassemblyline'),
    data.table = c("decomp.csv", "nitrogen.csv"),
    other.entity = c("ancillary_data.zip", "processing_and_analysis.R"))$x
  
  # # Valid inputs result in equivalent outputs
  # 
  # r <- validate_abstract(x1)
  # expect_null(r$issues)
  # expect_identical(r$x, x1)
  
  # Message if missing
  
  x1 <- x
  x1$template$abstract.txt <- NULL
  expect_true(
    stringr::str_detect(
      validate_abstract(x1),
      "Missing abstract. An abstract describing the data is recommended "))
  
})

# annotations -----------------------------------------------------------------

testthat::test_that("annotations.txt", {
  
  # Parameterize
  
  unlink(
    paste0(tempdir(), "/pkg_260"), 
    recursive = TRUE, 
    force = TRUE)
  
  file.copy(
    from = system.file(
      "/examples/pkg_260", 
      package = "EMLassemblyline"),
    to = tempdir(),
    recursive = TRUE)
  
  file.copy(
    from = system.file(
      "/examples/pkg_260/metadata_templates_overflow/annotations.txt", 
      package = "EMLassemblyline"),
    to = paste0(tempdir(), "/pkg_260/metadata_templates"),
    recursive = TRUE)

  x <- template_arguments(
    path = paste0(tempdir(), "/pkg_260/metadata_templates"), 
    data.path = paste0(tempdir(), "/pkg_260/data_objects"),
    data.table = c("decomp.csv", "nitrogen.csv"),
    other.entity = c("ancillary_data.zip", "processing_and_analysis.R"))$x
  
  defs <- as.data.frame(
    data.table::fread(
      file = system.file(
        '/templates/annotation_defaults.txt',
        package = 'EMLassemblyline'),
      colClasses = rep(
        "character",
        max(
          utils::count.fields(
            system.file(
              '/templates/annotation_defaults.txt',
              package = 'EMLassemblyline'),
            sep = "\t"))),
      fill = TRUE,
      blank.lines.skip = TRUE))
  
  # Template column names are correct
  
  x1 <- x
  colnames(x1$template$annotations.txt$content) <- c(
    colnames(x1$template$annotations.txt$content)[1:7], "Not a column name")

  expect_error(
    validate_annotation_column_names(x1),
    regexp = "Unexpected column names in the annotations template. ")
  
  # Each annotation has a subject, predicate, and object, with corresponding 
  # labels and URIs
  
  x1 <- x
  x1$template$annotations.txt$content$id[1] <- NA_character_
  x1$template$annotations.txt$content$predicate_label[2] <- NA_character_
  x1$template$annotations.txt$content$predicate_uri[3] <- NA_character_
  x1$template$annotations.txt$content$object_label[4] <- NA_character_
  x1$template$annotations.txt$content$object_uri[5] <- NA_character_
  
  expect_true(
    stringr::str_detect(
      validate_annotation_completeness(x1),
      "Incomplete annotations. A complete annotation requires an ID, subject"))
  
  # URIs are resolvable
  
  x1 <- x
  x1$template$annotations.txt$content$predicate_uri[1] <- "not_a_uri"
  x1$template$annotations.txt$content$object_uri[2] <- "not_a_uri"
  
  expect_true(
    stringr::str_detect(
      validate_annotation_uri(x1),
      "Unresolvable URIs. Resolvable URIs are required. These URIs "))
  
  # Clean up
  
  unlink(
    paste0(tempdir(), "/pkg_260"), 
    recursive = TRUE, 
    force = TRUE)
  
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
  
  # Valid inputs result in equivalent outputs
  
  r <- validate_table_attributes(x1)
  expect_null(r$issues)
  expect_identical(r$x, x1)
  
  # attributes.txt - attributes metadata are required for each data table
  
  x1 <- x
  x1$template$attributes_decomp.txt <- NULL
  
  expect_true(
    stringr::str_detect(
      validate_table_attribute_template_presence("attributes_decomp.txt", x1),
      "Missing attributes metadata. Attributes metadata describe important "))
  
  r <- validate_table_attributes(x1)
  expect_true(
    stringr::str_detect(
      r$issues,
      "Missing attributes metadata. Attributes metadata describe important "))
  
  # Template column names - Expected column names must be present
  
  x1 <- x
  x1$template$attributes_decomp.txt$content <- 
    x1$template$attributes_decomp.txt$content[, 1:2]
  
  expect_error(
    validate_table_attribute_template_column_names(
      "attributes_decomp.txt", x1),
    "Unexpected column names in .+. Expected columns ")
  
  expect_error(
    validate_table_attributes(x1),
    "Unexpected column names in .+. Expected columns ")
  
  # attributeName - All columns of a table should be listed
  
  x1 <- x
  x1$template$attributes_decomp.txt$content <- 
    x1$template$attributes_decomp.txt$content[1:3, ]
  
  expect_true(
    stringr::str_detect(
      validate_table_attribute_name_presence(
        "attributes_decomp.txt", "decomp.csv", x1),
      "Mising attribute names. These attributes are listed in the data but "))
  
  r <- validate_table_attributes(x1)
  expect_true(
    stringr::str_detect(
      r$issues,
      "Mising attribute names. These attributes are listed in the data but "))
  expect_null(r$x$template$attributes_decomp.txt)
  
  # attributeName - All columns of a table should be listed in the correct 
  # order
  
  x1 <- x
  x1$template$attributes_decomp.txt$content$attributeName <- 
    c(x1$template$attributes_decomp.txt$content$attributeName[5:7],
      x1$template$attributes_decomp.txt$content$attributeName[1:4])
  
  expect_true(
    stringr::str_detect(
      validate_table_attribute_name_order(
        "attributes_decomp.txt", "decomp.csv", x1),
      "Mismatched attribute order. Listed attributes are not in the same "))
  
  r <- validate_table_attributes(x1)
  expect_true(
    stringr::str_detect(
      r$issues,
      "Mismatched attribute order. Listed attributes are not in the same "))
  expect_null(r$x$template$attributes_decomp.txt)
  
  # definition- Each attribute has a definition
  
  x1 <- x
  x1$template$attributes_decomp.txt$content$attributeDefinition[1] <- ""
  
  expect_true(
    stringr::str_detect(
      validate_table_attribute_definitions("attributes_decomp.txt", x1),
      "Missing definitions. Attributes are meaningless without definition. "))
  
  r <- validate_table_attributes(x1)
  expect_true(
    stringr::str_detect(
      r$issues,
      "Missing definitions. Attributes are meaningless without definition. "))
  expect_null(r$x$template$attributes_decomp.txt)
  
  # class - Each attribute has a class
  
  x1 <- x
  x1$template$attributes_decomp.txt$content$class[1:2] <- ""
  
  expect_true(
    stringr::str_detect(
      validate_table_attribute_class_presence("attributes_decomp.txt", x1),
      "Missing classes. These attributes have missing classes"))
  
  r <- validate_table_attributes(x1)
  expect_true(
    stringr::str_detect(
      r$issues,
      "Missing classes. These attributes have missing classes"))
  expect_null(r$x$template$attributes_decomp.txt)
  
  # class - Each class is numeric, Date, character, or categorical
  
  x1 <- x
  x1$template$attributes_decomp.txt$content$class[1] <- "dateagorical"
  
  expect_true(
    stringr::str_detect(
      validate_table_attribute_class_type("attributes_decomp.txt", x1),
      "Unsupported classes. Supported classes are: numeric, character, "))
  
  r <- validate_table_attributes(x1)
  expect_true(
    stringr::str_detect(
      r$issues,
      "Unsupported classes. Supported classes are: numeric, character, "))
  expect_null(r$x$template$attributes_decomp.txt)
  
  # unit - Numeric classed attributes have units
  
  x1 <- x
  x1$template$attributes_decomp.txt$content$unit[
    x1$template$attributes_decomp.txt$content$class == "numeric"] <- ""
  
  expect_true(
    stringr::str_detect(
      validate_table_attribute_unit_presence("attributes_decomp.txt", x1),
      "Missing units. Attributes with a numeric class require units. "))
  
  r <- validate_table_attributes(x1)
  expect_true(
    stringr::str_detect(
      r$issues,
      "Missing units. Attributes with a numeric class require units. "))
  expect_null(r$x$template$attributes_decomp.txt)
  
  # unit - Units should be from the dictionary or defined in custom_units.txt
  
  x1 <- x
  x1$template$attributes_nitrogen.txt$content$unit[5] <- "an_undefined_unit"
  x1$template$attributes_nitrogen.txt$content$unit[6] <- "another_undefined_unit"
  
  expect_true(
    stringr::str_detect(
      validate_table_attribute_unit_definition("attributes_nitrogen.txt", x1),
      "Undefined units. Units must be from the EML standard unit dictionary "))
  
  r <- validate_table_attributes(x1)
  expect_true(
    stringr::str_detect(
      r$issues,
      "Undefined units. Units must be from the EML standard unit dictionary "))
  expect_null(r$x$template$attributes_nitrogen.txt)
  
  x1 <- x
  x1$template$attributes_nitrogen.txt$content$unit[5] <- "an_undefined_unit"
  x1$template$attributes_nitrogen.txt$content$unit[6] <- "another_undefined_unit"
  x1$template$custom_units.txt$content[
    nrow(x1$template$custom_units.txt$content)+1, ] <- c(
      "an_undefined_unit", "of some type", "with some parent SI", "a multiplier",
      "and a description")
  x1$template$custom_units.txt$content[
    nrow(x1$template$custom_units.txt$content)+1, ] <- c(
      "another_undefined_unit", "of some type", "with some parent SI", 
      "a multiplier", "and a description")
  
  expect_null(
    validate_table_attribute_unit_definition(
      "attributes_nitrogen.txt", x1))
  
  r <- validate_table_attributes(x1)
  expect_null(r$issues)
  expect_identical(r$x, x1)
  
  # Date - Each Date has a dateTimeformatString

  x1 <- x
  x1$template$attributes_decomp.txt$content$class[1:2] <- "Date"
  x1$template$attributes_decomp.txt$content$dateTimeFormatString[1] <- ""
  x1$template$attributes_decomp.txt$content$dateTimeFormatString[2] <- 
    "!Add datetime specifier here!"

  expect_true(
    stringr::str_detect(
      validate_table_attribute_date_format_presence(
        "attributes_decomp.txt", x1),
      "Missing datetime format strings. Each attribute with a \'Date\' "))
  
  r <- validate_table_attributes(x1)
  expect_true(
    stringr::str_detect(
      r$issues,
      "Missing datetime format strings. Each attribute with a \'Date\' "))
  expect_null(r$x$template$attributes_decomp.txt)
  
  # missingValueCode - Each missingValueCode only has 1 entry per column
  
  x1 <- x
  x1$template$attributes_decomp.txt$content$missingValueCode[1] <- "NA, -99999"
  x1$template$attributes_decomp.txt$content$missingValueCode[2] <- "NA -99999"
  
  expect_true(
    stringr::str_detect(
      validate_table_attribute_missing_value_code_quantity(
        "attributes_decomp.txt", x1),
      "Multiple missing value codes. Only one is allowed. It appears there "))
  
  r <- validate_table_attributes(x1)
  expect_true(
    stringr::str_detect(
      r$issues,
      "Multiple missing value codes. Only one is allowed. It appears there "))
  
  # missingValueCode - Cannot be a blank space
  
  x1 <- x
  x1$template$attributes_decomp.txt$content$missingValueCode[1] <- " "
  x1$template$attributes_decomp.txt$content$missingValueCode[2] <- 
    "valid code with spaces"
  
  expect_true(
    stringr::str_detect(
      validate_table_attribute_missing_value_code_ws(
        "attributes_decomp.txt", x1),
      "Unsupported missing value codes. White space \\(e.g. space, tab\\)"))
  
  r <- validate_table_attributes(x1)
  expect_true(
    stringr::str_detect(
      r$issues,
      "Unsupported missing value codes. White space \\(e.g. space, tab\\)"))
  expect_null(r$x$template$attributes_decomp.txt)

  # Both a missing value codes and a missing value code definition is required 
  # if one or the other is present
  
  x1 <- x
  x1$template$attributes_decomp.txt$content$missingValueCode[1] <- ""
  x1$template$attributes_decomp.txt$content$missingValueCodeExplanation[1] <- 
    "Missing value code"
  x1$template$attributes_decomp.txt$content$missingValueCode[2] <- ""
  x1$template$attributes_decomp.txt$content$missingValueCodeExplanation[2] <- 
    ""
  x1$template$attributes_decomp.txt$content$missingValueCode[3] <- "NA"
  x1$template$attributes_decomp.txt$content$missingValueCodeExplanation[3] <- 
    ""
  
  expect_true(
    stringr::str_detect(
      validate_table_attribute_missing_value_code_definition(
        "attributes_decomp.txt", x1),
      "Incomplete missing value code/definition pairs. Each missing value "))
  
  r <- validate_table_attributes(x1)
  expect_true(
    stringr::str_detect(
      r$issues,
      "Incomplete missing value code/definition pairs. Each missing value "))
  expect_null(r$x$template$attributes_decomp.txt)
  
  # False numeric attributes - Attributes specified by the user as numeric 
  # should contain no characters other than listed under missingValueCode of 
  # the table attributes template.
  
  x1 <- x
  numeric_attributes <- 
    x1$template$attributes_nitrogen.txt$content$attributeName[
      x1$template$attributes_nitrogen.txt$content$class == "numeric"]
  for (i in seq_along(numeric_attributes)) {
    x1$data.table$nitrogen.csv$content[i, numeric_attributes[i]] <- 
      "a non-numeric value that is not a missing value code"
  }
  
  r <- validate_table_attribute_false_numeric(
    "attributes_nitrogen.txt", "nitrogen.csv", x1)
  expect_true(
    stringr::str_detect(
      r$issues,
      "Inaccurate classes. These attributes are classified as 'numeric' but "))
  
  r <- validate_table_attributes(x1)
  expect_true(
    stringr::str_detect(
      r$issues,
      "Inaccurate classes. These attributes are classified as 'numeric' but "))
  expect_false(
    any(r$x$template$attributes_nitrogen.txt$content$class %in% "numeric"))
  
  # If multiple validation issues, then report all issues with a warning and
  # corresponding changes to x (the data and metadata list object).
  
  x1 <- x
  # All columns of a table should be listed
  x1$template$attributes_decomp.txt$content <- 
    x1$template$attributes_decomp.txt$content[-7, ]
  # Each attribute has a definition
  x1$template$attributes_decomp.txt$content$attributeDefinition[1] <- ""
  # Each attribute has a class
  x1$template$attributes_decomp.txt$content$class[2] <- ""
  # Each class is numeric, Date, character, or categorical
  x1$template$attributes_decomp.txt$content$class[3] <- "dateagorical"
  # Numeric classed attributes have units
  x1$template$attributes_decomp.txt$content$unit[6] <- ""
  # Units should be from the dictionary or defined in custom_units.txt
  x1$template$attributes_nitrogen.txt$content$unit[5] <- "an_undefined_unit"
  # Date - Each Date has a dateTimeformatString
  x1$template$attributes_nitrogen.txt$content$class[1] <- "Date"
  x1$template$attributes_nitrogen.txt$content$dateTimeFormatString[1] <- ""
  x1$template$attributes_nitrogen.txt$content$dateTimeFormatString[1] <- 
    "!Add datetime specifier here!"
  # Each missingValueCode only has 1 entry per column
  x1$template$attributes_decomp.txt$content$missingValueCode[1] <- "NA, -99999"
  # missingValueCode - Cannot be a blank space
  x1$template$attributes_decomp.txt$content$missingValueCode[2] <- " "
  # Both a missing value codes and a missing value code definition is required 
  # if one or the other is present
  x1$template$attributes_nitrogen.txt$content$missingValueCode[6] <- ""
  x1$template$attributes_nitrogen.txt$content$missingValueCodeExplanation[6] <- 
    "Missing value code"
  
  # Expected messages
  r <- validate_table_attributes(x1)
  expect_true(
    any(
      stringr::str_detect(
        r$issues,
        "Mising attribute names. These attributes are listed in the data but ")))
  expect_true(
    any(
      stringr::str_detect(
        r$issues,
        "Missing definitions. Attributes are meaningless without definition. ")))
  expect_true(
    any(
      stringr::str_detect(
        r$issues,
        "Missing classes. These attributes have missing classes")))
  expect_true(
    any(
      stringr::str_detect(
        r$issues,
        "Unsupported classes. Supported classes are: numeric, character, ")))
  expect_true(
    any(
      stringr::str_detect(
        r$issues,
        "Missing units. Attributes with a numeric class require units. ")))
  expect_true(
    any(
      stringr::str_detect(
        r$issues,
        "Undefined units. Units must be from the EML standard unit dictionary ")))
  expect_true(
    any(
      stringr::str_detect(
        r$issues,
        "Missing datetime format strings. Each attribute with a \'Date\' ")))
  expect_true(
    any(
      stringr::str_detect(
        r$issues,
        "Multiple missing value codes. Only one is allowed. It appears there ")))
  expect_true(
    any(
      stringr::str_detect(
        r$issues,
        "Unsupported missing value codes. White space \\(e.g. space, tab\\)")))
  expect_true(
    any(
      stringr::str_detect(
        r$issues,
        "Incomplete missing value code/definition pairs. Each missing value ")))
  # Expected dropped objects
  expect_null(r$x$template$attributes_decomp.txt)
  expect_null(r$x$template$attributes_nitrogen.txt)

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
  
  # Valid inputs result in equivalent outputs
  
  r <- validate_categorical_variables(x1)
  expect_null(r$issues)
  expect_identical(r$x, x1)
  
  # Categorical variables template - Required when table attributes are listed 
  # "categorical"
  
  x1 <- x
  x1$template$attributes_decomp.txt$content$class[1] <- "categorical"
  x1$template$catvars_decomp.txt <- NULL
  
  expect_true(
    stringr::str_detect(
      validate_categorical_variable_template_presence(
        "attributes_decomp.txt", x1),
      "Missing categorical variable metadata. Variables are listed as "))
  
  r <- validate_categorical_variables(x1)
  expect_true(
    stringr::str_detect(
      r$issues,
      "Missing categorical variable metadata. Variables are listed as "))
  expect_false(
    "categorical" %in% r$x$template$attributes_decomp.txt$content$class)
  expect_null(
    r$x$template$catvars_decomp.txt)

  # Required columns are present
  
  x1 <- x
  names(x1$template$catvars_decomp.txt$content) <- c(
    "attribute", "attribute_code", 
    names(x1$template$catvars_decomp.txt$content)[3])
  
  expect_error(
    validate_categorical_variable_column_names("catvars_decomp.txt", x1),
    regexp = "Unexpected column names in .+. Expected columns are: ")
  
  expect_error(
    validate_categorical_variables(x1))
  
  # codes - All codes require definition
  
  x1 <- x
  x1$template$catvars_nitrogen.txt$content$definition[1:2] <- ""
  
  expect_true(
    stringr::str_detect(
      validate_categorical_variable_definitions("catvars_nitrogen.txt", x1),
      "Missing code definitions. Codes are meaningless without definition."))
  
  r <- validate_categorical_variables(x1)
  expect_true(
    stringr::str_detect(
      r$issues,
      "Missing code definitions. Codes are meaningless without definition."))
  expect_false(
    "categorical" %in% r$x$template$attributes_nitrogen.txt$content$class)
  expect_null(
    r$x$template$catvars_nitrogen.txt)
  
  # If multiple validation issues, then report all issues with a warning and
  # corresponding changes to x (the data and metadata list object).
  
  x1 <- x
  # Categorical variables template
  x1$template$attributes_decomp.txt$content$class[1] <- "categorical"
  x1$template$catvars_decomp.txt <- NULL
  # All codes require definition
  x1$template$catvars_nitrogen.txt$content$definition[1:2] <- ""
  
  # Expectations
  r <- validate_categorical_variables(x1)
  expect_true(
    any(
      stringr::str_detect(
        r$issues,
        "Missing categorical variable metadata. Variables are listed as ")))
  expect_true(
    any(
      stringr::str_detect(
        r$issues,
        "Missing code definitions. Codes are meaningless without definition")))
  expect_true(
    !isTRUE(
      "categorical" %in% r$x$template$attributes_decomp.txt$content$class))
  expect_true(
    !isTRUE(
      "categorical" %in% r$x$template$attributes_nitrogen.txt$content$class))
  expect_null(
    r$x$template$catvars_decomp.txt)
  expect_null(
    r$x$template$catvars_nitrogen.txt)
  
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
  
  # Valid inputs result in equivalent outputs
  
  r <- validate_geographic_coverage(x1)
  expect_null(r$issues)
  expect_identical(r$x, x1)
  
  # Template is not missing
  
  x1 <- x
  x1$template$geographic_coverage.txt <- NULL
  
  expect_true(
    stringr::str_detect(
      validate_geographic_coverage_presence(x1),
      "Missing geographic coverage metadata. Geographic coverage "))
  
  expect_true(
    stringr::str_detect(
      validate_geographic_coverage(x1)$issues,
      "Missing geographic coverage metadata. Geographic coverage "))
  
  # Required columns are present
  
  x1 <- x
  expected_colnames <- colnames(x1$template$geographic_coverage.txt$content)
  colnames(x1$template$geographic_coverage.txt$content) <- c(
    "description", "north", expected_colnames[-1:-2])
  
  expect_error(
    validate_geographic_coverage_column_names(x1),
    regexp = "Unexpected column names in the geographic coverage template")
  
  expect_error(
    validate_geographic_coverage(x1),
    regexp = "Unexpected column names in the geographic coverage template")

  # Incomplete inputs - Each entry requires a north, south, east, and west 
  # coordinate, as well as a description
  
  x1 <- x
  x1$template$geographic_coverage.txt$content$northBoundingCoordinate[1] <- ""
  x1$template$geographic_coverage.txt$content$southBoundingCoordinate[2] <- ""
  
  expect_true(
    stringr::str_detect(
      validate_geographic_coverage_completeness(x1),
      "Incomplete geographic definitions. Geographic definitions require a "))
  
  r <- validate_geographic_coverage(x1)
  expect_true(
    stringr::str_detect(
      r$issues,
      "Incomplete geographic definitions. Geographic definitions require a "))
  expect_null(r$x$template$geographic_coverage.txt$content)
  
  # coordinates - Decimal degree is expected
  
  x1 <- x
  x1$template$geographic_coverage.txt$content$northBoundingCoordinate[1] <- 
    "45 23'"
  x1$template$geographic_coverage.txt$content$southBoundingCoordinate[2] <- 
    "23 degrees 23 minutes"
  
  expect_true(
    stringr::str_detect(
      validate_geographic_coverage_coordinate_format(x1),
      "Unsupported coordinate formats. Decimal degrees are required. These "))
  
  r <- validate_geographic_coverage(x1)
  expect_true(
    stringr::str_detect(
      r$issues,
      "Unsupported coordinate formats. Decimal degrees are required. These "))
  expect_null(r$x$template$geographic_coverage.txt$content)
  
  # Coordinate range
  
  x1 <- x
  x1$template$geographic_coverage.txt$content$northBoundingCoordinate[1] <- 91
  x1$template$geographic_coverage.txt$content$southBoundingCoordinate[2] <- 91
  x1$template$geographic_coverage.txt$content$eastBoundingCoordinate[3] <- 181
  x1$template$geographic_coverage.txt$content$westBoundingCoordinate[4] <- 181
  
  expect_true(
    stringr::str_detect(
      validate_geographic_coverage_coordinate_range(x1),
      "Unsupported coordinate range. Latitude values should range from "))
  
  r <- validate_geographic_coverage(x1)
  expect_true(
    stringr::str_detect(
      r$issues,
      "Unsupported coordinate range. Latitude values should range from "))
  expect_null(r$x$template$geographic_coverage.txt$content)
  
  # If multiple validation issues, then report all issues with a warning and
  # corresponding changes to x (the data and metadata list object).
  
  x1 <- x
  # Incomplete inputs
  x1$template$geographic_coverage.txt$content$northBoundingCoordinate[1] <- ""
  # Decimal degree is expected
  x1$template$geographic_coverage.txt$content$northBoundingCoordinate[2] <- 
    "45 23'"
  # Coordinate range
  x1$template$geographic_coverage.txt$content$northBoundingCoordinate[3] <- 91
  x1$template$geographic_coverage.txt$content$eastBoundingCoordinate[4] <- 181

  # Expectations
  r <- validate_geographic_coverage(x1)
  expect_true(
    stringr::str_detect(
      r$issues,
      "Incomplete geographic definitions. Geographic definitions require a "))
  expect_true(
    stringr::str_detect(
      r$issues,
      "Unsupported coordinate formats. Decimal degrees are required. These "))
  expect_true(
    stringr::str_detect(
      r$issues,
      "Unsupported coordinate range. Latitude values should range from "))
  expect_null(r$x$template$geographic_coverage.txt)

})

# intellectual_rights ---------------------------------------------------------

testthat::test_that("intellectual_rights", {
  
  # Parameterize

  x <- template_arguments(
    path = system.file(
      '/examples/pkg_260/metadata_templates',
      package = 'EMLassemblyline'),
    data.path = system.file(
      '/examples/pkg_260/data_objects',
      package = 'EMLassemblyline'),
    data.table = c("decomp.csv", "nitrogen.csv"),
    other.entity = c("ancillary_data.zip", "processing_and_analysis.R"))$x
  
  # # Valid inputs result in equivalent outputs
  # 
  # r <- validate_intellectual_rights(x1)
  # expect_null(r$issues)
  # expect_identical(r$x, x1)
  
  # Message if missing
  
  x1 <- x
  x1$template$intellectual_rights.txt <- NULL
  expect_true(
    stringr::str_detect(
      validate_intellectual_rights(x1),
      "Missing intellectual rights. An intellectual rights license is "))
  
})

# keywords --------------------------------------------------------------------

testthat::test_that("keywords", {
  
  # Parameterize
  
  x <- template_arguments(
    path = system.file(
      '/examples/pkg_260/metadata_templates',
      package = 'EMLassemblyline'),
    data.path = system.file(
      '/examples/pkg_260/data_objects',
      package = 'EMLassemblyline'),
    data.table = c("decomp.csv", "nitrogen.csv"),
    other.entity = c("ancillary_data.zip", "processing_and_analysis.R"))$x
  
  # # Valid inputs result in equivalent outputs
  # 
  # r <- validate_keywords(x1)
  # expect_null(r$issues)
  # expect_identical(r$x, x1)
  
  # Message if missing
  
  x1 <- x
  x1$template$keywords.txt <- NULL
  expect_true(
    stringr::str_detect(
      validate_keywords(x1),
      "Missing keywords. Keywords are recommended."))
  
})

# methods ---------------------------------------------------------------------

testthat::test_that("methods", {
  
  # Parameterize
  
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
  
  # Valid inputs result in equivalent outputs

  r <- validate_methods(x1)
  expect_null(r)
  
  # Message if missing
  
  x1 <- x
  x1$template$methods.txt <- NULL
  expect_true(
    stringr::str_detect(
      validate_methods(x1),
      "Missing methods. Methods are recommended and should describe \\(in "))
  
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
  
  # Valid inputs result in equivalent outputs
  
  r <- validate_personnel(x1)
  expect_null(r$issues)
  expect_identical(r$x, x1)
  
  # Missing
  
  x1 <- x
  x1$template$personnel.txt <- NULL
  
  expect_true(
    stringr::str_detect(
      validate_personnel_presence(x1),
      "Missing personnel metadata. Personnel metadata assigns attribution"))
  
  r <- validate_personnel(x1)
  expect_true(
    stringr::str_detect(
      r$issues,
      "Missing personnel metadata. Personnel metadata assigns attribution"))
  expect_null(r$x$template$personnel.txt)
  
  # Required columns are present
  
  x1 <- x
  expected_colnames <- colnames(x1$template$personnel.txt$content)
  colnames(x1$template$personnel.txt$content) <- c(
    "given", "middle", expected_colnames[-1:-2])
  
  expect_error(
    validate_personnel_column_names(x1),
    regexp = "Unexpected column names in the personnel template.")
  
  expect_error(
    validate_personnel(x1),
    regexp = "Unexpected column names in the personnel template.")
  
  # role - At least one creator is listed
  
  x1 <- x
  x1$template$personnel.txt$content$role[
    stringr::str_detect(
      x1$template$personnel.txt$content$role, 
      "creator")] <- "creontact"
  
  expect_true(
    stringr::str_detect(
      validate_personnel_creator(x1),
      "Missing creator. At least one creator is required."))
  
  r <- validate_personnel(x1)
  expect_true(
    stringr::str_detect(
      r$issues,
      "Missing creator. At least one creator is required."))
  expect_null(r$x$template$personnel.txt)
  
  # role - At least one contact is listed
  
  x1 <- x
  x1$template$personnel.txt$content$role[
    stringr::str_detect(
      x1$template$personnel.txt$content$role, 
      "contact")] <- "creontact"
  
  expect_true(
    stringr::str_detect(
      validate_personnel_contact(x1),
      "Missing contact. At least one contact is required."))
  
  r <- validate_personnel(x1)
  expect_true(
    stringr::str_detect(
      r$issues,
      "Missing contact. At least one contact is required."))
  expect_null(r$x$template$personnel.txt)
  
  # role - All personnel have roles
  
  x1 <- x
  x1$template$personnel.txt$content$role[
    stringr::str_detect(
      x1$template$personnel.txt$content$role, 
      "Equipment lead")] <- ""
  
  expect_true(
    stringr::str_detect(
      validate_personnel_roles(x1),
      "Missing role. Each person requires a role."))
  
  r <- validate_personnel(x1)
  expect_true(
    stringr::str_detect(
      r$issues,
      "Missing role. Each person requires a role."))
  expect_null(r$x$template$personnel.txt)
  
  # role - PI is recommended
  
  x1 <- x
  x1$template$personnel.txt$content$role[
    stringr::str_detect(
      x1$template$personnel.txt$content$role, 
      "PI|pi")] <- "pie"
  
  expect_true(
    stringr::str_detect(
      validate_personnel_pi(x1),
      "Missing Principal Investigator. Including the PI of the project from "))
  
  r <- validate_personnel(x1)
  expect_true(
    stringr::str_detect(
      r$issues,
      "Missing Principal Investigator. Including the PI of the project from "))
  expect_true(!is.null(r$x$template$personnel.txt))
  
  # Project info - The projectTitle, fundingAgency, and fundingNumber is 
  # recommended
  
  x1 <- x
  x1$template$personnel.txt$content$projectTitle <- ""
  x1$template$personnel.txt$content$fundingAgency <- ""
  x1$template$personnel.txt$content$fundingNumber <- ""
  
  expect_true(
    stringr::str_detect(
      validate_personnel_project(x1),
      "Missing funding information. Including the project title, "))
  
  r <- validate_personnel(x1)
  expect_true(
    stringr::str_detect(
      r$issues,
      "Missing funding information. Including the project title, "))
  expect_true(!is.null(r$x$template$personnel.txt))
  
  # Publisher - Only one publisher is allowed
  
  x1 <- x
  use_i <- which(x1$template$personnel.txt$content$role == "creator")[1:2]
  x1$template$personnel.txt$content$role[use_i] <- "publisher"
  
  r <- validate_personnel_publisher(x1)
  expect_true(
    stringr::str_detect(
      r$issues,
      "Too many publishers. Only the first will be used."))
  expect_equal(sum(r$x$template$personnel.txt$content$role == "publisher"), 1)
  
  r <- validate_personnel(x1)
  expect_true(
    stringr::str_detect(
      r$issues,
      "Too many publishers. Only the first will be used."))
  expect_equal(sum(r$x$template$personnel.txt$content$role == "publisher"), 1)
  
  # If multiple validation issues, then report all issues with a warning and
  # corresponding changes to x (the data and metadata list object).
  
  x1 <- x
  # Creator
  x1$template$personnel.txt$content$role[
    which(x1$template$personnel.txt$content$role == "creator")] <- "creontact"
  # Contact
  x1$template$personnel.txt$content$role[
    which(x1$template$personnel.txt$content$role == "contact")] <- "creontact"
  # role - All personnel have roles
  x1$template$personnel.txt$content$role[
    which(x1$template$personnel.txt$content$role == "Equipment lead")] <- ""  
  # Project info - The projectTitle, fundingAgency, and fundingNumber is 
  # recommended
  x1$template$personnel.txt$content$projectTitle <- ""
  x1$template$personnel.txt$content$fundingAgency <- ""
  x1$template$personnel.txt$content$fundingNumber <- ""
  # Publisher - Only one publisher is allowed
  use_i <- which(x1$template$personnel.txt$content$role == "creontact")[1:2]
  x1$template$personnel.txt$content$role[use_i] <- "publisher"
  
  # Expectations
  r <- validate_personnel(x1)
  expect_true(
    any(
      stringr::str_detect(
        r$issues,
        "Missing creator. At least one creator is required.")))
  expect_true(
    any(
      stringr::str_detect(
        r$issues,
        "Missing contact. At least one contact is required.")))
  expect_true(
    any(
      stringr::str_detect(
        r$issues,
        "Missing role. Each person requires a role.")))
  expect_true(
    any(
      stringr::str_detect(
        validate_personnel(x1)$issues,
        "Missing funding information. Including the project title, ")))
  expect_true(
    any(
      stringr::str_detect(
        r$issues,
        "Too many publishers. Only the first will be used.")))
  expect_null(r$x$template$personnel.txt)
  
})

# provenance -------------------------------------------------------------------

testthat::test_that("provenance", {
  
  # Parameterize
  
  attr_tmp <- read_template_attributes()
  
  unlink(
    paste0(tempdir(), "/pkg_260"), 
    recursive = TRUE, 
    force = TRUE)
  
  file.copy(
    from = system.file(
      "/examples/pkg_260", 
      package = "EMLassemblyline"),
    to = tempdir(),
    recursive = TRUE)
  
  file.copy(
    from = system.file(
      "/examples/pkg_260/metadata_templates_overflow/provenance.txt", 
      package = "EMLassemblyline"),
    to = paste0(tempdir(), "/pkg_260/metadata_templates"),
    recursive = TRUE)
  
  x <- template_arguments(
    path = paste0(tempdir(), "/pkg_260/metadata_templates"))$x
  x1 <- x
  
  unlink(
    paste0(tempdir(), "/pkg_260"), 
    recursive = TRUE, 
    force = TRUE)
  
  # Valid inputs result don't result in issues
  
  r <- validate_provenance(x1)
  expect_null(r$issues)
  expect_identical(r$x, x1)
  
  # Required columns are present
  
  x1 <- x
  expected_colnames <- colnames(x1$template$provenance.txt$content)
  colnames(x1$template$provenance.txt$content) <- c(
    "given", "middle", expected_colnames[-1:-2])
  
  expect_error(
    validate_provenance_column_names(x1),
    regexp = "Unexpected column names in the provenance template.")
  
  expect_error(
    validate_provenance(x1),
    regexp = "Unexpected column names in the provenance template.")
  
  # systemID - Only some systems are supported
  
  x1 <- x
  x1$template$provenance.txt$content$systemID[
    stringr::str_detect(
      x1$template$provenance.txt$content$systemID, 
      "EDI")] <- "non_supported_system"
  
  expect_true(
    stringr::str_detect(
      validate_provenance_system_id(x1),
      "Unsupported systemID. The only supported system, currently, is 'EDI'."))
  
  r <- validate_provenance(x1)
  expect_true(
    stringr::str_detect(
      r$issues,
      "Unsupported systemID. The only supported system, currently, is 'EDI'."))
  expect_null(r$x$template$provenance.txt)
  
  # dataPackageID + systemID - Valid pairs resolve to provenance metadata
  
  x1 <- x
  x1$template$provenance.txt$content$dataPackageID[
    stringr::str_detect(
      x1$template$provenance.txt$content$systemID, 
      "EDI")] <- "non_supported_package_id"
  
  expect_true(
    stringr::str_detect(
      validate_provenance_data_package_id(x1),
      "Invalid dataPackageID. These dataPackageID cannot be resolved:"))
  
  r <- validate_provenance(x1)
  expect_true(
    stringr::str_detect(
      r$issues,
      "Invalid dataPackageID. These dataPackageID cannot be resolved:"))
  expect_null(r$x$template$provenance.txt)
  
  # A URL is present for external resources
  
  x1 <- x
  external_resources <- as.numeric(
    row.names(
      x1$template$provenance.txt$content[
        !(x1$template$provenance.txt$content$dataPackageID != "" &
            x1$template$provenance.txt$content$systemID != ""), ]))
  x1$template$provenance.txt$content$url[external_resources[1:2]] <- ""
  
  expect_true(
    stringr::str_detect(
      validate_provenance_url_presence(x1),
      "Missing URLs. A URL is required for each resource. These resources"))
  
  r <- validate_provenance(x1)
  expect_true(
    stringr::str_detect(
      r$issues,
      "Missing URLs. A URL is required for each resource. These resources"))
  expect_null(r$x$template$provenance.txt)
  
  # A URL resolves for external resources

  x1 <- x
  external_resources <- as.numeric(
    row.names(
      x1$template$provenance.txt$content[
        !(x1$template$provenance.txt$content$dataPackageID != "" &
            x1$template$provenance.txt$content$systemID != ""), ]))
  x1$template$provenance.txt$content$url[external_resources[1:2]] <- 
    "a_non_resolvable_url"
  
  expect_true(
    stringr::str_detect(
      validate_provenance_url_resolvability(x1),
      "Unresolvable URLs. URLs must be resolvable. These URLs do not "))
  
  r <- validate_provenance(x1)
  expect_true(
    stringr::str_detect(
      r$issues,
      "Unresolvable URLs. URLs must be resolvable. These URLs do not "))
  expect_null(r$x$template$provenance.txt)
  
  # An online description is recommended for external resources
  
  x1 <- x
  external_resources <- as.numeric(
    row.names(
      x1$template$provenance.txt$content[
        !(x1$template$provenance.txt$content$dataPackageID != "" &
            x1$template$provenance.txt$content$systemID != ""), ]))
  x1$template$provenance.txt$content$onlineDescription[
    external_resources[1:2]] <- ""
  
  expect_true(
    stringr::str_detect(
      validate_provenance_online_description(x1),
      "Missing online descriptions. A description of each external resource"))
  
  r <- validate_provenance(x1)
  expect_true(
    stringr::str_detect(
      r$issues,
      "Missing online descriptions. A description of each external resource"))
  expect_true(!is.null(r$x$template$provenance.txt))
  
  # A title is present for external resources
  
  x1 <- x
  external_resources <- as.numeric(
    row.names(
      x1$template$provenance.txt$content[
        !(x1$template$provenance.txt$content$dataPackageID != "" &
            x1$template$provenance.txt$content$systemID != ""), ]))
  x1$template$provenance.txt$content$title[external_resources[1:2]] <- ""
  
  expect_true(
    stringr::str_detect(
      validate_provenance_title(x1),
      "Missing titles. A title is required for each external resource. "))
  
  r <- validate_provenance(x1)
  expect_true(
    stringr::str_detect(
      r$issues,
      "Missing titles. A title is required for each external resource. "))
  expect_null(r$x$template$provenance.txt)
  
  # A persons name, or an organization name, is present for external resources
  
  # Is not an issue if organization name is present
  x1 <- x
  external_resources <- as.numeric(
    row.names(
      x1$template$provenance.txt$content[
        !(x1$template$provenance.txt$content$dataPackageID != "" &
            x1$template$provenance.txt$content$systemID != ""), ]))
  x1$template$provenance.txt$content$givenName[external_resources[1:2]] <- ""
  x1$template$provenance.txt$content$middleInitial[external_resources[1:2]] <- ""
  x1$template$provenance.txt$content$surName[external_resources[1:2]] <- ""
  expect_null(validate_provenance_individual_organization_name(x1))
  
  # Is not an issue if individual name is present
  x1 <- x
  external_resources <- as.numeric(
    row.names(
      x1$template$provenance.txt$content[
        !(x1$template$provenance.txt$content$dataPackageID != "" &
            x1$template$provenance.txt$content$systemID != ""), ]))
  x1$template$provenance.txt$content$organizationName[external_resources[1:2]] <- ""
  expect_null(validate_provenance_individual_organization_name(x1))
  
  # Is an issue only if both are missing
  x1 <- x
  external_resources <- as.numeric(
    row.names(
      x1$template$provenance.txt$content[
        !(x1$template$provenance.txt$content$dataPackageID != "" &
            x1$template$provenance.txt$content$systemID != ""), ]))
  x1$template$provenance.txt$content$givenName[external_resources[1:2]] <- ""
  x1$template$provenance.txt$content$middleInitial[external_resources[1:2]] <- ""
  x1$template$provenance.txt$content$surName[external_resources[1:2]] <- ""
  x1$template$provenance.txt$content$organizationName[external_resources[1:2]] <- ""
  
  expect_true(
    stringr::str_detect(
      validate_provenance_individual_organization_name(x1),
      "Missing individual or organization name. An individual person or "))
  
  r <- validate_provenance(x1)
  expect_true(
    stringr::str_detect(
      r$issues,
      "Missing individual or organization name. An individual person or "))
  expect_null(r$x$template$provenance.txt)
  
  # A creator and contact is listed for each resource, when dataPackageID 
  # and systemID pair is missing
  
  x1 <- x
  external_resources <- as.numeric(
    row.names(
      x1$template$provenance.txt$content[
        !(x1$template$provenance.txt$content$dataPackageID != "" &
            x1$template$provenance.txt$content$systemID != ""), ]))
  x1$template$provenance.txt$content$role[external_resources[1:2]] <- ""
  
  expect_true(
    stringr::str_detect(
      validate_provenance_contact_creator(x1),
      "Missing creator/contact. Each external resources requires both a "))
  
  r <- validate_provenance(x1)
  expect_true(
    stringr::str_detect(
      r$issues,
      "Missing creator/contact. Each external resources requires both a "))
  expect_null(r$x$template$provenance.txt)
  
  # An email contact is recommended for external resources
  
  x1 <- x
  external_resources <- as.numeric(
    row.names(
      x1$template$provenance.txt$content[
        !(x1$template$provenance.txt$content$dataPackageID != "" &
            x1$template$provenance.txt$content$systemID != ""), ]))
  x1$template$provenance.txt$content$email[
    external_resources[1:2]] <- ""
  
  expect_true(
    stringr::str_detect(
      validate_provenance_email(x1),
      "Missing email. An email address for each external resource is "))
  
  r <- validate_provenance(x1)
  expect_true(
    stringr::str_detect(
      r$issues,
      "Missing email. An email address for each external resource is "))
  expect_true(!is.null(r$x$template$provenance.txt))
  
  # If multiple validation issues, then report all issues with a warning and
  # corresponding changes to x (the data and metadata list object).
  
  x1 <- x
  # systemID
  x1$template$provenance.txt$content$systemID[
    stringr::str_detect(
      x1$template$provenance.txt$content$systemID, 
      "EDI")] <- "non_supported_system"
  # email
  external_resources <- as.numeric(
    row.names(
      x1$template$provenance.txt$content[
        !(x1$template$provenance.txt$content$dataPackageID != "" &
            x1$template$provenance.txt$content$systemID != ""), ]))
  x1$template$provenance.txt$content$email[
    external_resources[1:2]] <- ""
  
  # Expectations
  r <- validate_provenance(x1)
  expect_true(
    any(
      stringr::str_detect(
        r$issues,
        "Unsupported systemID. The only supported system, currently, ")))
  expect_true(
    any(
      stringr::str_detect(
        r$issues,
        "Missing email. An email address for each external resource is ")))
  
})








testthat::test_that("compile_provenance()", {
  
  # Test the 3 different function call permutations to ensure the provenance
  # metadata is being added correctly
  
  # Parameterize do.call()
  attr_tmp <- read_template_attributes()
  
  unlink(
    paste0(tempdir(), "/pkg_260"), 
    recursive = TRUE, 
    force = TRUE)
  
  file.copy(
    from = system.file(
      "/examples/pkg_260", 
      package = "EMLassemblyline"),
    to = tempdir(),
    recursive = TRUE)
  
  file.copy(
    from = system.file(
      "/examples/pkg_260/metadata_templates_overflow/provenance.txt", 
      package = "EMLassemblyline"),
    to = paste0(tempdir(), "/pkg_260/metadata_templates"),
    recursive = TRUE)
  
  x <- template_arguments(
    path = paste0(tempdir(), "/pkg_260/metadata_templates"), 
    data.path = paste0(tempdir(), "/pkg_260/data_objects"),
    data.table = c("decomp.csv", "nitrogen.csv"),
    other.entity = c("ancillary_data.zip", "processing_and_analysis.R"))

  x$data.path <- system.file('/examples/pkg_260/data_objects', package = 'EMLassemblyline')
  x$data.table <- c("decomp.csv", "nitrogen.csv")
  x$data.table.name <- c("Decomp file name", "Nitrogen file name")
  x$data.table.description <- c("Decomp file description", "Nitrogen file description")
  x$data.table.quote.character  <- c("\\'", "\\'")
  x$data.table.url <- c("https://url/to/decomp.csv", "https://url/to/nitrogen.csv")
  x$dataset.title <- 'Sphagnum and Vascular Plant Decomposition under Increasing Nitrogen Additions: 2014-2015'
  x$eml.path <- tempdir()
  x$geographic.coordinates <- c('55.895', '112.094','55.895', '112.094')
  x$geographic.description <- 'Alberta, Canada, 100 km south of Fort McMurray, Canada'
  x$maintenance.description <- 'Completed'
  x$other.entity <- c("ancillary_data.zip", "processing_and_analysis.R")
  x$other.entity.name <- c("ancillary_data file name", "processing_and_analysis file name")
  x$other.entity.description <- c("ancillary_data file description", "processing_and_analysis file description")
  x$other.entity.url <- c("https://url/to/ancillary_data.zip", "https://url/to/processing_and_analysis.R")
  x$package.id <- "edi.100.1"
  x$path <- system.file('/examples/pkg_260/metadata_templates', package = 'EMLassemblyline')
  x$return.obj <- T
  x$temporal.coverage <- c('2014-05-01', '2015-10-31')
  x$user.domain <- c("EDI", "LTER")
  x$user.id <- c("userid1", "userid2")
  x$write.file <- F

  x1 <- x
  internal_resources <- unique(x1$x$template$provenance.txt$content$dataPackageID[
    x1$x$template$provenance.txt$content$dataPackageID != ""])
  external_resources <- unique(x1$x$template$provenance.txt$content$title[
    x1$x$template$provenance.txt$content$dataPackageID == ""])
  
  # Called from do.call()

  x1 <- x
  r <- suppressWarnings(
    do.call(make_eml, x1[names(x1) %in% names(formals(make_eml))]))

  expect_true(
    length(r$dataset$methods$methodStep) - 1 ==
      length(c(internal_resources, external_resources)))

  for (i in 2:length(r$dataset$methods$methodStep)) {
    expect_true(
      "dataSource" %in% names(r$dataset$methods$methodStep[[i]]))
  }

  # Called from make_eml() with explicitly stated arguments

  r <- suppressWarnings(
    make_eml(
      path = paste0(tempdir(), "/pkg_260/metadata_templates"),
      data.path = paste0(tempdir(), "/pkg_260/data_objects"),
      eml.path = paste0(tempdir(), "/pkg_260/eml"),
      dataset.title = "Sphagnum and Vascular Plant Decomposition under Increasing Nitrogen Additions: 2014-2015",
      data.table = c("decomp.csv", "nitrogen.csv"),
      data.table.name = c("Decomp file name", "Nitrogen file name"),
      data.table.description = c("Decomposition data description", "Nitrogen data description"),
      other.entity = c("ancillary_data.zip", "processing_and_analysis.R"),
      other.entity.name = c("Ancillary data name", "Script name"),
      other.entity.description = c("Ancillary data description", "Script description"),
      temporal.coverage = c('2014-05-01', '2015-10-31'),
      maintenance.description = 'completed',
      user.id = "someuserid",
      user.domain = "LTER",
      package.id = 'edi.141.1',
      return.obj = TRUE,
      write.file = FALSE))

  expect_true(
    length(r$dataset$methods$methodStep) - 1 ==
      length(c(internal_resources, external_resources)))

  for (i in 2:length(r$dataset$methods$methodStep)) {
    expect_true(
      "dataSource" %in% names(r$dataset$methods$methodStep[[i]]))
  }
  
  # FIXME: This test cannot be run without error within this test_that() 
  # because the prov object is not accessible within sys.call() return of
  # compile_provenance().
  # # Called from make_eml() with named arguments
  # 
  # prov <- "edi.200.1"
  # r <- suppressWarnings(
  #   make_eml(
  #     path = paste0(tempdir(), "/pkg_260/metadata_templates"),
  #     data.path = paste0(tempdir(), "/pkg_260/data_objects"),
  #     eml.path = paste0(tempdir(), "/pkg_260/eml"),
  #     dataset.title = "Sphagnum and Vascular Plant Decomposition under Increasing Nitrogen Additions: 2014-2015",
  #     data.table = c("decomp.csv", "nitrogen.csv"),
  #     data.table.name = c("Decomp file name", "Nitrogen file name"),
  #     data.table.description = c("Decomposition data description", "Nitrogen data description"),
  #     other.entity = c("ancillary_data.zip", "processing_and_analysis.R"),
  #     other.entity.name = c("Ancillary data name", "Script name"),
  #     other.entity.description = c("Ancillary data description", "Script description"),
  #     temporal.coverage = c('2014-05-01', '2015-10-31'),
  #     maintenance.description = 'completed',
  #     user.id = "someuserid",
  #     user.domain = "LTER",
  #     package.id = 'edi.141.1',
  #     provenance = prov,
  #     return.obj = TRUE,
  #     write.file = FALSE))
  # 
  # expect_true(
  #   length(r$dataset$methods$methodStep) - 1 == 
  #     length(c(internal_resources, external_resources)))
  # 
  # for (i in 2:length(r$dataset$methods$methodStep)) {
  #   expect_true(
  #     "dataSource" %in% names(r$dataset$methods$methodStep[[i]]))
  # }
  
  # Clean up
  
  unlink(
    paste0(tempdir(), "/pkg_260"), 
    recursive = TRUE, 
    force = TRUE)
  
})

# taxonomic_coverage ----------------------------------------------------------

testthat::test_that("taxonomic_coverage", {
  
  # Parameterize
  
  attr_tmp <- read_template_attributes()
  
  unlink(
    paste0(tempdir(), "/pkg_260"), 
    recursive = TRUE, 
    force = TRUE)
  
  file.copy(
    from = system.file(
      "/examples/pkg_260", 
      package = "EMLassemblyline"),
    to = tempdir(),
    recursive = TRUE)
  
  file.copy(
    from = system.file(
      "/examples/pkg_260/metadata_templates_overflow/taxonomic_coverage.txt", 
      package = "EMLassemblyline"),
    to = paste0(tempdir(), "/pkg_260/metadata_templates"),
    recursive = TRUE)
  
  x <- template_arguments(
    path = paste0(tempdir(), "/pkg_260/metadata_templates"))$x
  x1 <- x
  
  unlink(
    paste0(tempdir(), "/pkg_260"), 
    recursive = TRUE, 
    force = TRUE)
  
  # Default manipulation - Use raw names when a resolved name is missing
  x1 <- x
  missing <- (x$template$taxonomic_coverage.txt$content$name_resolved == "") | 
    (is.na(x$template$taxonomic_coverage.txt$content$name_resolved)) 
  expect_true(any(missing))
  
  r <- validate_taxonomic_coverage(x1)
  expect_null(r$issues)
  expect_false(identical(r$x, x1))
  missing <- (r$x$template$taxonomic_coverage.txt$content$name_resolved == "") | 
    (is.na(r$x$template$taxonomic_coverage.txt$content$name_resolved)) 
  expect_false(any(missing))

  # column names - Required columns are present
  
  x1 <- x
  expected_colnames <- colnames(x1$template$taxonomic_coverage.txt$content)
  colnames(x1$template$taxonomic_coverage.txt$content) <- c(
    "nayme", "type", expected_colnames[-1:-2])
  
  expect_error(
    validate_taxonomic_coverage_column_names(x1),
    regexp = "Unsupported column names in taxonomic coverage template:")
  
  expect_error(
    validate_taxonomic_coverage(x1),
    regexp = "Unsupported column names in taxonomic coverage template:")
  
  # authority_system - Unsupported authorities are not allowed
  
  x1 <- x
  x1$template$taxonomic_coverage.txt$content$authority_system[1:2] <- 
    "unsupported_authority_id"
  
  expect_true(
    stringr::str_detect(
      validate_taxonomic_coverage_authority_system(x1),
      "Unsupported authorities for entries: .+. Supported authorities are"))
  
  r <- validate_taxonomic_coverage(x1)
  expect_true(
    stringr::str_detect(
      r$issues,
      "Unsupported authorities for entries: .+. Supported authorities are"))
  expect_null(r$x$template$taxonomic_coverage.txt)
  
  # incomplete entries - Incomplete entries (missing authority_system or 
  # authority_id) will not return information
  
  x1 <- x
  x1$template$taxonomic_coverage.txt$content$authority_system[1] <- ""
  x1$template$taxonomic_coverage.txt$content$authority_id[2] <- ""
  
  expect_true(
    stringr::str_detect(
      validate_taxonomic_coverage_completeness(x1),
      "Missing inputs. A taxonomic authority and corresponding identifier "))
  
  r <- validate_taxonomic_coverage(x1)
  expect_true(
    stringr::str_detect(
      r$issues,
      "Missing inputs. A taxonomic authority and corresponding identifier "))
  expect_null(r$x$template$taxonomic_coverage.txt)
  
  # resolved names - Use the raw name if it cannot be resolved
  
  x1 <- x
  x1$template$taxonomic_coverage.txt$content$name_resolved[1] <- ""
  
  r <- validate_taxonomic_coverage(x1)
  expect_true(
    all(
      !is.na(r$x$template$taxonomic_coverage.txt$content$name_resolved) & 
        r$x$template$taxonomic_coverage.txt$content$name_resolved != ""))
    
  # If multiple validation issues, then report all issues with a warning and
  # corresponding changes to x (the data and metadata list object).
  
  x1 <- x
  # authority_system
  x1$template$taxonomic_coverage.txt$content$authority_system[1:2] <- 
    "unsupported_authority_id"
  # incomplete entries
  x1$template$taxonomic_coverage.txt$content$authority_system[3] <- ""
  x1$template$taxonomic_coverage.txt$content$authority_id[4] <- ""

  # Expectations
  r <- validate_taxonomic_coverage(x1)
  expect_true(
    stringr::str_detect(
      r$issues,
      "Unsupported authorities for entries: .+. Supported authorities are"))
  expect_true(
    stringr::str_detect(
      r$issues,
      "Missing inputs. A taxonomic authority and corresponding identifier "))
  expect_null(r$x$template$taxonomic_coverage.txt)
  
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
