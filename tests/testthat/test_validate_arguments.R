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
    data.table = c("decomp.csv", "nitrogen.csv"))
  
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
  expect_error(do.call(make_eml, x1[names(x1) %in% names(formals(make_eml))]))
  
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
  
  expect_error(
    make_eml(
      path = path,
      dataset.title = dataset.title,
      temporal.coverage = temporal.coverage,
      geographic.description = geographic.description,
      geographic.coordinates = geographic.coordinates,
      maintenance.description = maintenance.description,
      write.file = FALSE,
      user.id = c('csmith', 'nosuchuser'),
      user.domain = 'LTER'
    )
  )
  
  expect_error(
    make_eml(
      path = path,
      dataset.title = dataset.title,
      temporal.coverage = temporal.coverage,
      geographic.description = geographic.description,
      geographic.coordinates = geographic.coordinates,
      maintenance.description = maintenance.description,
      write.file = FALSE,
      user.id = 'csmith',
      user.domain = c('LTER', 'EDI')
    )
  )
  
  # user.domain - is required
  
  expect_error(
    make_eml(
      path = path,
      dataset.title = dataset.title,
      temporal.coverage = temporal.coverage,
      geographic.description = geographic.description,
      geographic.coordinates = geographic.coordinates,
      maintenance.description = maintenance.description,
      write.file = FALSE,
      user.id = user.id
    )
  )
  
  # user.domain (unsupported user.domain)
  
  expect_warning(
    suppressMessages(
      make_eml(
        path = system.file('/examples/templates_new_geocoverage', package = 'EMLassemblyline'),
        dataset.title = dataset.title,
        temporal.coverage = temporal.coverage,
        maintenance.description = maintenance.description,
        write.file = FALSE,
        user.id = 'csmith',
        user.domain = 'EDEYE'
      )
    )
  )
  
  # user.id - is required
  
  x1 <- x
  x1$user.id <- NULL
  expect_error(do.call(make_eml, x1[names(x1) %in% names(formals(make_eml))]))
  
  
  
  
  
})