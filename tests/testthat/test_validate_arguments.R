context('Validate arguments')
library(EMLassemblyline)

testthat::test_that("make_eml()", {
  
  # make_eml() ----------------------------------------------------------------
  
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
  
  # dataset.title - required

  x1 <- x
  x1$dataset.title <- NULL
  expect_error(do.call(make_eml, x1[names(x1) %in% names(formals(make_eml))]))
  
  # data.table.description - required
  
  x1 <- x
  x1$data.table.description <- NULL
  expect_error(do.call(make_eml, x1[names(x1) %in% names(formals(make_eml))]))
  
  # data.table.description - required for each data.table
  
  x1 <- x
  x1$data.table.description <- x1$data.table.description[1]
  expect_error(do.call(make_eml, x1[names(x1) %in% names(formals(make_eml))]))
  
  # data.table.quote.character - required for each data.table
  
  x1 <- x
  x1$data.table.quote.character <- x1$data.table.quote.character[1]
  expect_error(do.call(make_eml, x1[names(x1) %in% names(formals(make_eml))]))
  
  # data.table.url - required for each data.table
  
  x1 <- x
  x1$data.table.url <- x1$data.table.url[1]
  expect_error(do.call(make_eml, x1[names(x1) %in% names(formals(make_eml))]))
  
  # geographic.corrdinates - required when using the geographic.description 
  # argument
  
  x1 <- x
  x1$geographic.coordinates <- NULL
  expect_error(do.call(make_eml, x1[names(x1) %in% names(formals(make_eml))]))
  
  # geographic.description - required when using the geographic.coordinates 
  # argument
  
  x1 <- x
  x1$geographic.description <- NULL
  expect_error(do.call(make_eml, x1[names(x1) %in% names(formals(make_eml))]))
  
  # maintenance.description
  
  x1 <- x
  x1$maintenance.description <- NULL
  expect_warning(do.call(make_eml, x1[names(x1) %in% names(formals(make_eml))]))
  
  # other.entity - required if other.entity.name or other.entity.description
  # is in use
  
  x1 <- x
  x1$other.entity <- NULL
  expect_error(do.call(make_eml, x1[names(x1) %in% names(formals(make_eml))]))
  
  # other.entity - required for each other.entity.description
  
  x1 <- x
  x1$other.entity <- x1$other.entity[1]
  expect_error(do.call(make_eml, x1[names(x1) %in% names(formals(make_eml))]))
  
  # other.entity.description - required
  
  x1 <- x
  x1$other.entity.description <- NULL
  expect_error(do.call(make_eml, x1[names(x1) %in% names(formals(make_eml))]))
  
  # other.entity.url - required for each other.entity
  
  x1 <- x
  x1$other.entity.url <- x1$other.entity.url[1]
  expect_error(do.call(make_eml, x1[names(x1) %in% names(formals(make_eml))]))
  
  # package.id - unrecognized package.id results in warning
  
  x1 <- x
  x1$package.id <- "edi.141"
  expect_warning(do.call(make_eml, x1[names(x1) %in% names(formals(make_eml))]))
  
  # provenance
  
  x1 <- x
  x1$provenance <- c("edi.349.1", "knb-lter-ntl.3.28", "knbee-lter-ntl.3.28")
  expect_warning(do.call(make_eml, x1[names(x1) %in% names(formals(make_eml))]))
  
  # temporal.coverage - temporal.coverage is required
  
  x1 <- x
  x1$temporal.coverage <- NULL
  expect_error(do.call(make_eml, x1[names(x1) %in% names(formals(make_eml))]))
  
  # temporal.coverage - temporal.coverage requires both a start date and an
  # end date
  
  x1 <- x
  x1$temporal.coverage <- x1$temporal.coverage[1]
  expect_error(do.call(make_eml, x1[names(x1) %in% names(formals(make_eml))]))
  
  # user.domain - required for each user.id
  
  x1 <- x
  x1$user.domain <- x1$user.domain[1]
  expect_error(do.call(make_eml, x1[names(x1) %in% names(formals(make_eml))]))
  
  # user.domain - is required
  
  x1 <- x
  x1$user.domain <- NULL
  expect_error(do.call(make_eml, x1[names(x1) %in% names(formals(make_eml))]))
  
  # user.domain - unrecognized user.domain results in warning
  
  x1 <- x
  x1$user.domain[1] <- "EDEYE"
  expect_warning(do.call(make_eml, x1[names(x1) %in% names(formals(make_eml))]))
  
  # user.id - required for each user.domain
  
  x1 <- x
  x1$user.id <- x1$user.id[1]
  expect_error(do.call(make_eml, x1[names(x1) %in% names(formals(make_eml))]))
  
  # user.id - is required
  
  x1 <- x
  x1$user.id <- NULL
  expect_error(do.call(make_eml, x1[names(x1) %in% names(formals(make_eml))]))

  # write.file - eml.path is required
  
  x1 <- x
  x1$path <- NULL
  x1$eml.path <- NULL
  x1$write.file <- T
  expect_error(do.call(make_eml, x1[names(x1) %in% names(formals(make_eml))]))
  
})
