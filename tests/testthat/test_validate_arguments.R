context('validate_arguments()')
library(EMLassemblyline)

# template_annotations() ------------------------------------------------------

testthat::test_that('template_annotations()', {
  
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
  
  attr_tmp <- read_template_attributes()
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
  
  # dataset.title - Warn if missing

  x1 <- x
  x1$dataset.title <- NULL
  expect_warning(
    do.call(make_eml, x1[names(x1) %in% names(formals(make_eml))]),
    regexp = "A dataset title is required")
  
  # data.table.description - Warn if missing
  
  x1 <- x
  x1$data.table.description <- NULL
  expect_warning(
    do.call(make_eml, x1[names(x1) %in% names(formals(make_eml))]),
    regexp = "Data table descriptions are recommended")
  
  # data.table.description - Warn if length doesn't match data.table
  
  x1 <- x
  x1$data.table.description <- x1$data.table.description[1]
  expect_warning(
    do.call(make_eml, x1[names(x1) %in% names(formals(make_eml))]),
    regexp = "One or more data table descriptions are missing")
  
  # data.table.description - When length doesn't match data.table undescribed
  # tables will not have a description
  
  x1 <- x
  x1$data.table.description <- x1$data.table.description[1]
  r <- suppressWarnings(
    do.call(make_eml, x1[names(x1) %in% names(formals(make_eml))]))
  expect_true(!is.na(r$dataset$dataTable[[1]]$entityDescription))
  expect_true(is.na(r$dataset$dataTable[[2]]$entityDescription))
  
  # data.table.name - Warn if using defaults
  
  x1 <- x
  x1$data.table.name <- NULL
  expect_warning(
    do.call(make_eml, x1[names(x1) %in% names(formals(make_eml))]),
    regexp = paste0(
      "Data table names are missing. A short name for each data table is ",
      "recommended. Defaulting to data table file names."))
  
  # data.table.name - Warn if length doesn't match data.table

  x1 <- x
  x1$data.table.name <- x1$data.table.name[1]
  expect_warning(
    do.call(make_eml, x1[names(x1) %in% names(formals(make_eml))]),
    regexp = paste0("One or more data table names are missing. Defaulting to ",
                    "data table file names"))
  
  # data.table.quote.character - Warn if length doesn't match data.table
  
  x1 <- x
  x1$data.table.quote.character <- x1$data.table.quote.character[1]
  expect_warning(
    do.call(make_eml, x1[names(x1) %in% names(formals(make_eml))]),
    regexp = "One or more data table quote characters are missing.")
  
  # data.table.url - Warn if length doesn't match data.table
  
  x1 <- x
  x1$data.table.url <- x1$data.table.url[1]
  expect_warning(
    do.call(make_eml, x1[names(x1) %in% names(formals(make_eml))]),
    regexp = "One or more data table URLs are missing.")

  # geographic.corrdinates - Expected when using geographic.description 
  
  x1 <- x
  x1$geographic.coordinates <- NULL
  x1$x$template$geographic_coverage.txt <- NULL
  expect_warning(
    do.call(make_eml, x1[names(x1) %in% names(formals(make_eml))]),
    regexp = "Geographic coordinates are missing.")
  
  # geographic.description - Expected when using geographic.coordinates
  
  x1 <- x
  x1$geographic.description <- NULL
  expect_warning(
    do.call(make_eml, x1[names(x1) %in% names(formals(make_eml))]),
    regexp = "Geographic description is missing.")

  # maintenance.description
  
  x1 <- x
  x1$maintenance.description <- NULL
  expect_warning(
    do.call(make_eml, x1[names(x1) %in% names(formals(make_eml))]),
    regexp = 'A maintenance description is recommended.')

  # other.entity.description - Warn if missing
  
  x1 <- x
  x1$other.entity.description <- NULL
  expect_warning(
    do.call(make_eml, x1[names(x1) %in% names(formals(make_eml))]),
    regexp = "Other entity descriptions are recommended")
  
  # other.entity.description - Warn if length doesn't match other.entity
  
  x1 <- x
  x1$other.entity.description <- x1$other.entity.description[1]
  expect_warning(
    do.call(make_eml, x1[names(x1) %in% names(formals(make_eml))]),
    regexp = "One or more other entity descriptions are missing")

  # other.entity.name - Warn if using defaults
  
  x1 <- x
  x1$other.entity.name <- NULL
  expect_warning(
    do.call(make_eml, x1[names(x1) %in% names(formals(make_eml))]),
    regexp = paste0(
      "Other entity names are missing. A short name for each other entity is ",
      "recommended. Defaulting to other entity file names."))
  
  # other.entity.name - Warn if length doesn't match other.entity
  
  x1 <- x
  x1$other.entity.name <- x1$other.entity.name[1]
  expect_warning(
    do.call(make_eml, x1[names(x1) %in% names(formals(make_eml))]),
    regexp = paste0("One or more other entity names are missing. Defaulting to ",
                    "other entity file names"))
  
  # other.entity.url - Warn if length doesn't match other.entity
  
  x1 <- x
  x1$other.entity.url <- x1$other.entity.url[1]
  expect_warning(
    do.call(make_eml, x1[names(x1) %in% names(formals(make_eml))]),
    regexp = "One or more other entity URLs are missing.")
  
  # package.id - If malformed EDI or LTER package ID then warning.
  
  x1 <- x
  x1$package.id <- "knb-lter-ntl.5"
  expect_warning(
    do.call(make_eml, x1[names(x1) %in% names(formals(make_eml))]),
    regexp = "Warning: 'package.id' is not valid for EDI or LTER. Expected ")
  
  # provenance
  
  x1 <- x
  x1$provenance <- c("edi.349.1", "knb-lter-ntl.3.28", "knbee-lter-ntl.3.28")
  expect_warning(
    do.call(make_eml, x1[names(x1) %in% names(formals(make_eml))]),
    regexp = "Unable to generate provenance metadata for unrecognized identifier")
  
  # temporal.coverage - Warn if missing
  
  x1 <- x
  x1$temporal.coverage <- NULL
  expect_warning(
    do.call(make_eml, x1[names(x1) %in% names(formals(make_eml))]),
    regexp = "Temporal coverage is recommended")
  
  # temporal.coverage - Warn if missing start or end date
  
  x1 <- x
  x1$temporal.coverage <- x1$temporal.coverage[1]
  expect_warning(
    do.call(make_eml, x1[names(x1) %in% names(formals(make_eml))]),
    regexp = "Temporal coverage requires a begin and end date")

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