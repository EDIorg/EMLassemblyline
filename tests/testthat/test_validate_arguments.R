context('validate_arguments()')
library(EMLassemblyline)

# template_annotations() ------------------------------------------------------

testthat::test_that('template_annotations()', {
  
  unlink(paste0(tempdir(), "/annotations.txt"), recursive = TRUE)
  
  # Missing path
  
  expect_error(
    validate_arguments(
      fun.name = 'template_annotations',
      fun.args = list(path = NULL)
    )
  )
  
  # Invalid path
  
  expect_error(
    validate_arguments(
      fun.name = 'template_annotations',
      fun.args = list(path = "/an/invalid/path")
    )
  )
  
  # default.annotations
  
  df <- data.table::fread(
    system.file(
      "/templates/annotation_defaults.txt", 
      package = "EMLassemblyline"
    )
  )
  
  expect_null(
    validate_arguments(
      fun.name = 'template_annotations',
      fun.args = list(
        path = tempdir(),
        default.annotations = df
      )
    )
  )
  
  expect_error(
    validate_arguments(
      fun.name = 'template_annotations',
      fun.args = list(
        path = tempdir(),
        default.annotations = as.list(df)
      )
    )
  )
  
  expect_null(
    validate_arguments(
      fun.name = 'template_annotations',
      fun.args = list(
        path = tempdir(),
        default.annotations = df
      )
    )
  )
  
  df2 <- df
  colnames(df2) <- c("el", "predicate_label", "predicate_uri", "object_label", 
                    "object_uri")
  
  expect_error(
    validate_arguments(
      fun.name = 'template_annotations',
      fun.args = list(
        path = tempdir(),
        default.annotations = df2
      )
    )
  )
  
  rm(df2)
  rm(df)
  
  # eml (legacy eml)
  
  expect_error(
    validate_arguments(
      fun.name = 'template_annotations',
      fun.args = list(
        path = tempdir(),
        eml = "This is not the expected emld list object."
      )
    )
  )

})


# annotate_eml() ------------------------------------------------------

testthat::test_that('annotate_eml()', {
  
  # annotations
  
  expect_error(
    validate_arguments(
      fun.name = 'annotate_eml',
      fun.args = list(
        annotations = NULL
      )
    )
  )
  
  expect_error(
    validate_arguments(
      fun.name = 'annotate_eml',
      fun.args = list(
        annotations = "/a/non/existant/path"
      )
    )
  )
  
  expect_error(
    validate_arguments(
      fun.name = 'annotate_eml',
      fun.args = list(
        annotations = list(
          data.table::fread(
            system.file(
              "/examples/pkg_260/metadata_templates/annotations.txt", 
              package = "EMLassemblyline"
            )
          )
        )
      )
    )
  )
  
  expect_error(
    validate_arguments(
      fun.name = 'annotate_eml',
      fun.args = list(
        annotations = system.file(
          "/examples/pkg_260/metadata_templates/annotations.txt", 
          package = "EMLassemblyline"
        )
      )
    )
  )
  
  # eml.in
  
  expect_error(
    validate_arguments(
      fun.name = 'annotate_eml',
      fun.args = list(
        annotations = system.file(
          "/examples/pkg_260/metadata_templates/annotations.txt", 
          package = "EMLassemblyline"
        ),
        eml.in = "/an/invalid/eml/file"
      )
    )
  )
  
  expect_error(
    validate_arguments(
      fun.name = 'annotate_eml',
      fun.args = list(
        annotations = system.file(
          "/examples/pkg_260/metadata_templates/annotations.txt", 
          package = "EMLassemblyline"
        ),
        eml.in = system.file(
          "/examples/eml/edi.260.3.xml", 
          package = "EMLassemblyline"
        )
      )
    )
  )
  
  # eml.out
  
  expect_error(
    validate_arguments(
      fun.name = 'annotate_eml',
      fun.args = list(
        annotations = system.file(
          "/examples/pkg_260/metadata_templates/annotations.txt", 
          package = "EMLassemblyline"
        ),
        eml.in = system.file(
          "/examples/eml/edi.260.3.xml", 
          package = "EMLassemblyline"
        ),
        eml.out = "/an/invalid/path/for/new/eml"
      )
    )
  )
})


# make_eml() ------------------------------------------------------------------

testthat::test_that("make_eml()", {
  
  # Parameterize
  
  attr_tmp <- read_template_characteristics()
  x <- template_arguments(
    path = system.file(
      '/examples/pkg_260/metadata_templates',
      package = 'EMLassemblyline'),
    data.path = system.file(
      '/examples/pkg_260/data_objects',
      package = 'EMLassemblyline'),
    data.table = c("decomp.csv", "nitrogen.csv"),
    other.entity = c("ancillary_data.zip", "processing_and_analysis.R"))
  x$x$template$taxonomic_coverage.txt <- NULL
  
  x$data.path <- system.file('/examples/pkg_260/data_objects', package = 'EMLassemblyline')
  x$data.table <- c("decomp.csv", "nitrogen.csv")
  x$data.table.name <- c("Decomp file name", "Nitrogen file name")
  x$data.table.description <- c("Decomp file description", "Nitrogen file description")
  x$data.table.quote.character  <- c("\\'", "\\'")
  x$data.table.url <- c("https://url/to/decomp.csv", "https://url/to/nitrogen.csv")
  x$dataset.title <- 'Sphagnum and Vascular Plant Decomposition under Increasing Nitrogen Additions: 2014-2015'
  x$eml.path <- system.file('/examples/pkg_260/eml', package = 'EMLassemblyline')
  x$geographic.coordinates <- c('55.895', '112.094','55.895', '112.094')
  x$geographic.description <- 'Alberta, Canada, 100 km south of Fort McMurray, Canada'
  x$maintenance.description <- 'Completed'
  x$other.entity <- c("ancillary_data.zip", "processing_and_analysis.R")
  x$other.entity.name <- c("ancillary_data file name", "processing_and_analysis file name")
  x$other.entity.description <- c("ancillary_data file description", "processing_and_analysis file description")
  x$other.entity.url <- c("https://url/to/ancillary_data.zip", "https://url/to/processing_and_analysis.R")
  x$package.id <- "edi.100.1"
  x$path <- system.file('/examples/pkg_260/metadata_templates', package = 'EMLassemblyline')
  x$provenance <- NULL
  x$return.obj <- T
  x$temporal.coverage <- c('2014-05-01', '2015-10-31')
  x$user.domain <- c("EDI", "LTER")
  x$user.id <- c("userid1", "userid2")
  x$write.file <- F

  # write.file - eml.path is required
  
  x1 <- x
  x1$path <- NULL
  x1$eml.path <- NULL
  x1$write.file <- TRUE
  expect_error(do.call(make_eml, x1[names(x1) %in% names(formals(make_eml))]))
  
})

# template_provenance() -------------------------------------------------------

testthat::test_that("template_provenance()", {
  
  # path
  expect_error(
    validate_arguments(
      fun.name = 'template_provenance',
      fun.args = list(path = "/not/a/directory")), 
    regexp = 'The directory specified by the argument "path" does not')

})

testthat::test_that("template_table_attributes()", {
  
  # template_table_attributes() -----------------------------------------------
  
  # Column names - Follow naming best practices
  
  dir.create(paste0(tempdir(), "/dataset"))
  d <- data.table::fread(
    system.file(
      '/examples/pkg_260/data_objects/decomp.csv', 
      package = 'EMLassemblyline'))
  n <- stringr::str_replace(names(d), "_", " ")
  n <- stringr::str_replace(n, "t", "%")
  names(d) <- n
  data.table::fwrite(d, paste0(tempdir(), "/dataset/decomp.csv"))
  
  x <- template_arguments()
  x$path <- paste0(tempdir(), "/dataset")
  x$data.path <- paste0(tempdir(), "/dataset")
  x$data.table <- "decomp.csv"
  x$write.file <- TRUE
  x$x <- NULL
  
  expect_warning(
    do.call(
      template_table_attributes, 
      x[names(x) %in% names(formals(template_table_attributes))]),
    regexp = paste0(
      "decomp.csv has column names that are not composed of strictly ",
      "alphanumeric characters and underscores \\(recommended\\). This ",
      "best practice ensures the data may be read by most software ",
      "applications. Consider revising these columns: %ype, da%e, ",
      "n%rt, percen% loss, %axa"))
  
  unlink(paste0(tempdir(), "/dataset"), recursive = TRUE)
  
})





# dataset.title ---------------------------------------------------------------

testthat::test_that("dataset.title", {
  
  # Missing
  x <- list(dataset.title = NULL)
  expect_true(
    stringr::str_detect(
      validate_title_presence(x),
      "The dataset title is missing."))
  expect_true(
    stringr::str_detect(
      validate_title(x),
      "The dataset title is missing."))
  
  # Present
  x <- list(dataset.title = "This title is of an adequate length.")
  expect_null(validate_title_length(x))
  expect_null(validate_title(x))
  
  # Too short
  x <- list(dataset.title = "Title is too short.")
  expect_true(
    stringr::str_detect(
      validate_title_length(x),
      "The dataset title should be at least 5 words."))
  expect_true(
    stringr::str_detect(
      validate_title(x),
      "The dataset title should be at least 5 words."))

})

# data.table.name -------------------------------------------------------------

testthat::test_that("data.table.name", {
  
  # Missing
  x <- list(
    data.table = c("table1.csv", "table2.csv"),
    data.table.name = NULL)
  r <- validate_table_name(x)
  expect_true(
    stringr::str_detect(
      r$issues,
      "Missing name for"))
  expect_true(
    all(
      r$fun.args$data.table.name %in% x$data.table))
  
  # Present
  x <- list(
    data.table = c("table1.csv", "table2.csv"),
    data.table.name = c("Name 1", "Name 2"))
  r <- validate_table_name(x)
  expect_null(r$issues)
  expect_false(
    all(
      r$fun.args$data.table.name %in% x$data.table))
  
})

# data.table.description ------------------------------------------------------

testthat::test_that("data.table.description", {
  
  # Missing
  x <- list(
    data.table = c("table1.csv", "table2.csv"),
    data.table.description = NULL)
  r <- validate_table_description(x)
  expect_true(
    stringr::str_detect(
      r$issues,
      "Missing description for"))
  expect_true(
    all(
      r$fun.args$data.table.description %in% x$data.table))
  
  # Present
  x <- list(
    data.table = c("table1.csv", "table2.csv"),
    data.table.description = c("Description 1", "Description 2"))
  r <- validate_table_description(x)
  expect_null(r$issues)
  expect_false(
    all(
      r$fun.args$data.table.description %in% x$data.table))
  
})

# data.table.quote.character --------------------------------------------------

testthat::test_that("data.table.quote.character", {
  
  # Unused argument (this argument is not required)
  x <- list(
    data.table = c("table1.csv", "table2.csv"),
    data.table.quote.character = NULL)
  r <- validate_table_quote_character(x)
  expect_null(r$issues)
  
  # Missing one
  x <- list(
    data.table = c("table1.csv", "table2.csv"),
    data.table.quote.character = '"')
  r <- validate_table_quote_character(x)
  expect_true(
    stringr::str_detect(
      r$issues,
      "Missing quote character for"))
  expect_equal(
      length(r$fun.args$data.table.quote.character),
      length(r$fun.args$data.table))
  
  # Present
  x <- list(
    data.table = c("table1.csv", "table2.csv"),
    data.table.quote.character = c('"', '"'))
  r <- validate_table_quote_character(x)
  expect_null(r$issues)
  expect_equal(
    length(r$fun.args$data.table.quote.character),
    length(r$fun.args$data.table))
  
})

# data.table.url --------------------------------------------------------------

testthat::test_that("data.table.url", {
  
  # Unused argument (this argument is not required)
  x <- list(
    data.table = c("table1.csv", "table2.csv"),
    data.table.url = NULL)
  r <- validate_table_url(x)
  expect_null(r$issues)
  
  # Missing one
  x <- list(
    data.table = c("table1.csv", "table2.csv"),
    data.table.url = "/some/url/table1.csv")
  r <- validate_table_url(x)
  expect_true(
    stringr::str_detect(
      r$issues,
      "Missing URL for"))
  expect_equal(
    length(r$fun.args$data.table.url),
    length(r$fun.args$data.table))
  
  # Present
  x <- list(
    data.table = c("table1.csv", "table2.csv"),
    data.table.url = c("/some/url/table1.csv", "/some/url/table2.csv"))
  r <- validate_table_url(x)
  expect_null(r$issues)
  expect_equal(
    length(r$fun.args$data.table.url),
    length(r$fun.args$data.table))
  
})

# geographic.coordinates and geographic.description ---------------------------

testthat::test_that("geographic.coordinates and geographic.description", {
  
  # Missing one (geographic.coordinates)
  x <- list(
    geographic.coordinates = NULL,
    geographic.description = "some description")
  r <- validate_geographic_coord_desc(x)
  expect_true(
    stringr::str_detect(
      r$issues,
      "Geographic coordinates are missing"))
  expect_null(r$fun.args$geographic.description)
  
  # Missing one (geographic.description)
  x <- list(
    geographic.coordinates = c("45", "-120", "44", "-121"),
    geographic.description = NULL)
  r <- validate_geographic_coord_desc(x)
  expect_true(
    stringr::str_detect(
      r$issues,
      "Geographic description is missing"))
  expect_null(r$fun.args$geographic.coordinates)
  
  # Present
  x <- list(
    geographic.coordinates = c("45", "-120", "44", "-121"),
    geographic.description = "some description")
  r <- validate_geographic_coord_desc(x)
  expect_null(r$issues)
  expect_equal(
    r$fun.args$geographic.coordinates,
    x$geographic.coordinates)
  expect_equal(
    r$fun.args$geographic.description,
    x$geographic.description)
  
})

# maintenance.description -----------------------------------------------------

testthat::test_that("maintenance.description", {
  
  # Missing
  x <- list(
    maintenance.description = NULL)
  r <- validate_maintenance_description(x)
  expect_true(
    stringr::str_detect(
      r$issues,
      "A maintenance description is recommended"))
  expect_null(r$fun.args$geographic.description)
  
  # Present
  x <- list(
    maintenance.description = "Some description")
  r <- validate_maintenance_description(x)
  expect_null(r$issues)
  expect_equal(
    r$fun.args$maintenance.description,
    x$maintenance.description)
  
})

# other.entity.name -------------------------------------------------------------

testthat::test_that("other.entity.name", {
  
  # Missing
  x <- list(
    other.entity = c("other1.zip", "other2.zip"),
    other.entity.name = NULL)
  r <- validate_other_entity_name(x)
  expect_true(
    stringr::str_detect(
      r$issues,
      "Missing name for"))
  expect_true(
    all(
      r$fun.args$other.entity.name %in% x$other.entity))
  
  # Present
  x <- list(
    other.entity = c("other1.zip", "other2.zip"),
    other.entity.name = c("Name 1", "Name 2"))
  r <- validate_other_entity_name(x)
  expect_null(r$issues)
  expect_false(
    all(
      r$fun.args$other.entity.name %in% x$other.entity))
  
})

# other.entity.description ------------------------------------------------------

testthat::test_that("other.entity.description", {
  
  # Missing
  x <- list(
    other.entity = c("other1.zip", "other2.zip"),
    other.entity.description = NULL)
  r <- validate_other_entity_description(x)
  expect_true(
    stringr::str_detect(
      r$issues,
      "Missing description for"))
  expect_true(
    all(
      r$fun.args$other.entity.description %in% x$other.entity))
  
  # Present
  x <- list(
    other.entity = c("other1.zip", "other2.zip"),
    other.entity.description = c("Description 1", "Description 2"))
  r <- validate_other_entity_description(x)
  expect_null(r$issues)
  expect_false(
    all(
      r$fun.args$other.entity.description %in% x$other.entity))
  
})

# other.entity.url --------------------------------------------------------------

testthat::test_that("other.entity.url", {
  
  # Unused argument (this argument is not required)
  x <- list(
    other.entity = c("other1.zip", "other2.zip"),
    other.entity.url = NULL)
  r <- validate_other_entity_url(x)
  expect_null(r$issues)
  
  # Missing one
  x <- list(
    other.entity = c("other1.zip", "other2.zip"),
    other.entity.url = "/some/url/other1.zip")
  r <- validate_other_entity_url(x)
  expect_true(
    stringr::str_detect(
      r$issues,
      "Missing URL for"))
  expect_equal(
    length(r$fun.args$other.entity.url),
    length(r$fun.args$other.entity))
  
  # Present
  x <- list(
    other.entity = c("other1.zip", "other2.zip"),
    other.entity.url = c("/some/url/other1.zip", "/some/url/other2.zip"))
  r <- validate_other_entity_url(x)
  expect_null(r$issues)
  expect_equal(
    length(r$fun.args$other.entity.url),
    length(r$fun.args$other.entity))
  
})

# temporal.coverage -----------------------------------------------------------

testthat::test_that("temporal.coverage", {
  
  # Missing
  x <- list(temporal.coverage = NULL)
  r <- validate_temporal_coverage(x)
  expect_true(
    stringr::str_detect(
      r$issues,
      "Temporal coverage is missing"))
  expect_null(r$fun.args$geographic.description)
  
  # Missing one component
  x <- list(temporal.coverage = c("2012-01-01"))
  r <- validate_temporal_coverage(x)
  expect_true(
    stringr::str_detect(
      r$issues,
      "Temporal coverage requires a begin and end date"))
  expect_null(r$fun.args$geographic.coordinates)
  
  # Present
  x <- list(temporal.coverage = c("2012-01-01", "2013-01-01"))
  r <- validate_temporal_coverage(x)
  expect_null(r$issues)
  expect_equal(
    r$fun.args$temporal.coverage,
    x$temporal.coverage)
  
})
