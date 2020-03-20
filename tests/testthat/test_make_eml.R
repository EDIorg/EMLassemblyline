context('Make EML')
library(EMLassemblyline)

# Parameterize ----------------------------------------------------------------

path <- system.file(
  '/examples/templates/abstract.txt', 
  package = 'EMLassemblyline'
)
path <- substr(path, 1, nchar(path) - 23)
path_parent <- path

# Is EDI's API accessible?

edi_api <- try(
  EDIutils::api_get_provenance_metadata('edi.100.1'), 
  silent = TRUE
)

# Possible arguments to make_eml.R

data.path <- paste0(path, '/data')
eml.path <- paste0(path, '/eml')
path <- paste0(path, '/templates')
dataset.title <- 'Sphagnum and Vascular Plant Decomposition under Increasing Nitrogen Additions: 2014-2015'
data.table <- c('decomp.csv', 'nitrogen.csv')
data.table.name <- c("Decomp file name", "Nitrogen file name")
data.table.description <- c('Decomposition data', 'Nitrogen data')
temporal.coverage <- c('2014-05-01', '2015-10-31')
geographic.description <- 'Alberta, Canada, 100 km south of Fort McMurray, Canada'
geographic.coordinates <- c('55.895', '112.094','55.895', '112.094')
maintenance.description <- 'completed'
user.id <- c('csmith', 'someusername')
user.domain <- c('LTER', 'EDI')
package.id <- 'edi.141.1'
data.table.quote.character <- c("\\'", "\\'")
data.url <- 'https://lter.limnology.wisc.edu/sites/default/files/data/edi_141/data'
other.entity <- 'ancillary_data.zip'
other.entity.name <- 'Ancillary data file name'
other.entity.description <- 'Ancillary data'
provenance <- 'edi.100.1'

x_table <- template_arguments(
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

x_table_docall <- x_table

x_table <- x_table$x

x_table_other <- template_arguments(
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
  ),
  other.entity = 'ancillary_data.zip'
)

x_table_other_docall <- x_table_other

x_table_other <- x_table_other$x

x_other <- template_arguments(
  path = system.file(
    '/examples/templates', 
    package = 'EMLassemblyline'
  ),
  data.path = system.file(
    '/examples/data',
    package = 'EMLassemblyline'
  ),
  other.entity = 'ancillary_data.zip'
)

x_other_docall <- x_other

x_other <- x_other$x

# Add arguments to list structure x

x_table_docall$data.path <- data.path
x_table_docall$data.table <- data.table
x_table_docall$data.table.name <- data.table.name
x_table_docall$data.table.description <- data.table.description
x_table_docall$data.table.quote.character <- data.table.quote.character
x_table_docall$data.url <- data.url
x_table_docall$dataset.title <- dataset.title
x_table_docall$eml.path <- eml.path
x_table_docall$geographic.coordinates <- NULL
x_table_docall$geographic.description <- NULL
x_table_docall$maintenance.description <- maintenance.description
x_table_docall$package.id <- package.id
x_table_docall$path <- path
x_table_docall$provenance <- NULL
x_table_docall$return.obj <- TRUE
x_table_docall$temporal.coverage <- temporal.coverage
x_table_docall$user.domain <- user.domain
x_table_docall$user.id <- user.id
x_table_docall$write.file <- FALSE
x_table_docall$x$template$bounding_boxes.txt <- NULL

x_table_other_docall$data.path <- data.path
x_table_other_docall$data.table <- data.table
x_table_other_docall$data.table.name <- data.table.name
x_table_other_docall$data.table.description <- data.table.description
x_table_other_docall$data.table.quote.character <- data.table.quote.character
x_table_other_docall$other.entity <- other.entity
x_table_other_docall$other.entity.name <- other.entity.name
x_table_other_docall$other.entity.description <- other.entity.description
x_table_other_docall$data.url <- data.url
x_table_other_docall$dataset.title <- dataset.title
x_table_other_docall$eml.path <- eml.path
x_table_other_docall$geographic.coordinates <- NULL
x_table_other_docall$geographic.description <- NULL
x_table_other_docall$maintenance.description <- maintenance.description
x_table_other_docall$package.id <- package.id
x_table_other_docall$path <- path
x_table_other_docall$provenance <- NULL
x_table_other_docall$return.obj <- TRUE
x_table_other_docall$temporal.coverage <- temporal.coverage
x_table_other_docall$user.domain <- user.domain
x_table_other_docall$user.id <- user.id
x_table_other_docall$write.file <- FALSE
x_table_other_docall$x$template$bounding_boxes.txt<- NULL

x_other_docall$data.path <- data.path
x_other_docall$other.entity <- other.entity
x_other_docall$other.entity.name <- other.entity.name
x_other_docall$other.entity.description <- other.entity.description
x_other_docall$data.url <- data.url
x_other_docall$dataset.title <- dataset.title
x_other_docall$eml.path <- eml.path
x_other_docall$geographic.coordinates <- NULL
x_other_docall$geographic.description <- NULL
x_other_docall$maintenance.description <- maintenance.description
x_other_docall$package.id <- package.id
x_other_docall$path <- path
x_other_docall$provenance <- NULL
x_other_docall$return.obj <- TRUE
x_other_docall$temporal.coverage <- temporal.coverage
x_other_docall$user.domain <- user.domain
x_other_docall$user.id <- user.id
x_other_docall$write.file <- FALSE
x_other_docall$x$template$bounding_boxes.txt <- NULL

# File paths within R library

path.inst <- system.file(
  '/examples/templates/abstract.txt', 
  package = 'EMLassemblyline'
)
path.inst <- substr(path.inst, 1, nchar(path.inst)-23)

# Move templates to tempdir() and remove bounding_boxes.txt

test <- file.copy(
  from  = system.file(
    '/examples/templates',
    package = 'EMLassemblyline'
  ),
  to = tempdir(),
  recursive = TRUE
)

test <- file.remove(
  paste0(
    tempdir(),
    '/templates/bounding_boxes.txt'
  )
)

test <- file.remove(
  paste0(
    tempdir(),
    '/templates/taxonomicCoverage.xml'
  )
)

# Expect errors ---------------------------------------------------------------

testthat::test_that('Error out when required arguments are missing', {
  
  # path
  
  expect_error(
    make_eml(
      dataset.title = dataset.title,
      temporal.coverage = temporal.coverage,
      geographic.description = geographic.description,
      geographic.coordinates = geographic.coordinates,
      maintenance.description = maintenance.description,
      user.id = user.id,
      user.domain = user.domain,
      write.file = FALSE
    )
  )
  
  # dataset.title
  
  expect_error(
    make_eml(
      path = path,
      temporal.coverage = temporal.coverage,
      geographic.description = geographic.description,
      geographic.coordinates = geographic.coordinates,
      maintenance.description = maintenance.description,
      write.file = FALSE,
      user.id = user.id,
      user.domain = user.domain
    )
  )
  
  # temporal.coverage
  
  expect_error(
    make_eml(
      path = path,
      dataset.title = dataset.title,
      geographic.description = geographic.description,
      geographic.coordinates = geographic.coordinates,
      maintenance.description = maintenance.description,
      write.file = FALSE,
      user.id = user.id,
      user.domain = user.domain
    )
  )
  
  # temporal.coverage missing start date
  
  expect_error(
    make_eml(
      path = path,
      data.path = data.path,
      eml.path = eml.path,
      dataset.title = 'Sphagnum and Vascular Plant Decomposition under Increasing Nitrogen Additions: 2014-2015',
      data.table = data.table,
      data.table.description = c('Decomposition data', 'Nitrogen data'),
      temporal.coverage = '2015-10-31',
      geographic.description = 'Alberta, Canada, 100 km south of Fort McMurray, Canada',
      geographic.coordinates = c('55.895', '112.094','55.895', '112.094'),
      maintenance.description = 'completed',
      user.id = user.id,
      user.domain = user.domain,
      package.id = 'edi.141.1',
      write.file = FALSE
    )
  )
  
  # geographic.description
  
  expect_error(
    make_eml(
      path = path,
      dataset.title = dataset.title,
      temporal.coverage = temporal.coverage,
      geographic.coordinates = geographic.coordinates,
      maintenance.description = maintenance.description,
      write.file = FALSE,
      user.id = user.id,
      user.domain = user.domain
    )
  )
  
  # geographic.corrdinates
  
  expect_error(
    make_eml(
      path = path,
      dataset.title = dataset.title,
      temporal.coverage = temporal.coverage,
      geographic.description = geographic.description,
      maintenance.description = maintenance.description,
      write.file = FALSE,
      user.id = user.id,
      user.domain = user.domain
    )
  )
  
  # geographic.cordinates and bounding_box.txt missing
  
  expect_error(
    make_eml(
      path = paste0(path_parent),
      dataset.title = dataset.title,
      temporal.coverage = temporal.coverage,
      geographic.description = geographic.description,
      maintenance.description = maintenance.description,
      write.file = FALSE,
      user.id = user.id,
      user.domain = user.domain
    )
  )
  
  # maintenance.description
  
  expect_error(
    make_eml(
      path = path,
      dataset.title = dataset.title,
      temporal.coverage = temporal.coverage,
      geographic.description = geographic.description,
      geographic.coordinates = geographic.coordinates,
      write.file = FALSE,
      user.id = user.id,
      user.domain = user.domain
    )
  )
  
  # user.id
  
  expect_error(
    make_eml(
      path = path,
      dataset.title = dataset.title,
      temporal.coverage = temporal.coverage,
      geographic.description = geographic.description,
      geographic.coordinates = geographic.coordinates,
      maintenance.description = maintenance.description,
      write.file = FALSE,
      user.domain = user.domain
    )
  )
  
  # Mismatching user.id and affilation
  
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
  
  # user.domain
  
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
  
  # data.table.description
  
  expect_error(
    make_eml(
      path = path,
      data.path = data.path,
      eml.path = eml.path,
      dataset.title = 'Sphagnum and Vascular Plant Decomposition under Increasing Nitrogen Additions: 2014-2015',
      data.table = data.table,
      temporal.coverage = c('2014-05-01', '2015-10-31'),
      geographic.description = 'Alberta, Canada, 100 km south of Fort McMurray, Canada',
      geographic.coordinates = c('55.895', '112.094','55.895', '112.094'),
      maintenance.description = 'completed',
      user.id = user.id,
      user.domain = user.domain,
      package.id = 'edi.141.1',
      write.file = FALSE
    )
  )
  
  # Malformed package.id
  
  expect_warning(
    suppressMessages(
      make_eml(
        path = system.file('/examples/templates_new_geocoverage', package = 'EMLassemblyline'),
        data.path = data.path,
        eml.path = eml.path,
        dataset.title = 'Sphagnum and Vascular Plant Decomposition under Increasing Nitrogen Additions: 2014-2015',
        data.table = data.table,
        data.table.description = c('Decomposition data', 'Nitrogen data'),
        temporal.coverage = c('2014-05-01', '2015-10-31'),
        maintenance.description = 'completed',
        user.id = user.id,
        user.domain = user.domain,
        package.id = 'edi.141',
        write.file = FALSE
      )
    )
  )
  
  # Missing one of two data.table.description
  
  expect_error(
    make_eml(
      path = path,
      data.path = data.path,
      eml.path = eml.path,
      dataset.title = 'Sphagnum and Vascular Plant Decomposition under Increasing Nitrogen Additions: 2014-2015',
      data.table = data.table,
      data.table.name = data.table.name,
      data.table.description = 'Decomposition data',
      temporal.coverage = c('2014-05-01', '2015-10-31'),
      geographic.description = 'Alberta, Canada, 100 km south of Fort McMurray, Canada',
      geographic.coordinates = c('55.895', '112.094','55.895', '112.094'),
      maintenance.description = 'completed',
      user.id = user.id,
      user.domain = user.domain,
      package.id = 'edi.141.1',
      write.file = FALSE
    )
  )
  
  # Missing one of two data.table.quote.character
  
  expect_error(
    make_eml(
      path = path,
      data.path = data.path,
      eml.path = eml.path,
      dataset.title = 'Sphagnum and Vascular Plant Decomposition under Increasing Nitrogen Additions: 2014-2015',
      data.table = data.table,
      data.table.name = data.table.name,
      data.table.description = c('Decomposition data', 'Nitrogen data'),
      data.table.quote.character = "\'",
      temporal.coverage = c('2014-05-01', '2015-10-31'),
      geographic.description = 'Alberta, Canada, 100 km south of Fort McMurray, Canada',
      geographic.coordinates = c('55.895', '112.094','55.895', '112.094'),
      maintenance.description = 'completed',
      user.id = user.id,
      user.domain = user.domain,
      package.id = 'edi.141.1',
      write.file = FALSE
    )
  )
  
  # other.entity.description
  
  expect_error(
    make_eml(
      path = path,
      data.path = data.path,
      eml.path = eml.path,
      dataset.title = 'Sphagnum and Vascular Plant Decomposition under Increasing Nitrogen Additions: 2014-2015',
      data.table = data.table,
      data.table.name = data.table.name,
      data.table.description = c('Decomposition data', 'Nitrogen data'),
      other.entity = 'ancillary_data.zip',
      other.entity.name = other.entity.name,
      temporal.coverage = c('2014-05-01', '2015-10-31'),
      geographic.description = 'Alberta, Canada, 100 km south of Fort McMurray, Canada',
      geographic.coordinates = c('55.895', '112.094','55.895', '112.094'),
      maintenance.description = 'completed',
      user.id = user.id,
      user.domain = user.domain,
      package.id = 'edi.141.1',
      write.file = FALSE
    )
  )
  
  # other.entity
  
  expect_error(
    make_eml(
      path = path,
      data.path = data.path,
      eml.path = eml.path,
      dataset.title = 'Sphagnum and Vascular Plant Decomposition under Increasing Nitrogen Additions: 2014-2015',
      data.table = data.table,
      data.table.name = data.table.name,
      data.table.description = c('Decomposition data', 'Nitrogen data'),
      other.entity.name = other.entity.name,
      other.entity.description = 'Ancillary data',
      temporal.coverage = c('2014-05-01', '2015-10-31'),
      geographic.description = 'Alberta, Canada, 100 km south of Fort McMurray, Canada',
      geographic.coordinates = c('55.895', '112.094','55.895', '112.094'),
      maintenance.description = 'completed',
      user.id = user.id,
      user.domain = user.domain,
      package.id = 'edi.141.1',
      write.file = FALSE
    )
  )
  
  # Deprecated arguments result in warnings
  
  expect_warning(
    suppressMessages(
      make_eml(
        path = system.file('/examples/templates_new_geocoverage', package = 'EMLassemblyline'),
        data.path = data.path,
        eml.path = eml.path,
        dataset.title = 'Sphagnum and Vascular Plant Decomposition under Increasing Nitrogen Additions: 2014-2015',
        data.files = data.table,
        data.files.description = c('Decomposition data', 'Nitrogen data'),
        data.files.quote.character = c("\"", "\""),
        zip.dir = 'ancillary_data.zip',
        zip.dir.description = 'Ancillary data',
        data.files.url = '/some/url',
        temporal.coverage = c('2014-05-01', '2015-10-31'),
        maintenance.description = 'completed',
        user.id = user.id,
        affiliation = user.domain,
        package.id = 'edi.141.1',
        write.file = FALSE
      )
    )
  )
  
  # other.entity and other.entity.description mismatch
  
  expect_error(
    make_eml(
      path = path,
      data.path = data.path,
      eml.path = eml.path,
      dataset.title = 'Sphagnum and Vascular Plant Decomposition under Increasing Nitrogen Additions: 2014-2015',
      data.table = data.table,
      data.table.name = data.table.name,
      data.table.description = c('Decomposition data', 'Nitrogen data'),
      other.entity = 'ancillary_data.zip',
      other.entity.description = c('Ancillary data', 'Additional ancillary data'),
      temporal.coverage = c('2014-05-01', '2015-10-31'),
      geographic.description = 'Alberta, Canada, 100 km south of Fort McMurray, Canada',
      geographic.coordinates = c('55.895', '112.094','55.895', '112.094'),
      maintenance.description = 'completed',
      user.id = user.id,
      user.domain = user.domain,
      package.id = 'edi.141.1',
      write.file = FALSE
    )
  )
  
  # abstract.txt missing
  
  expect_error(
    suppressMessages(
      make_eml(
        path = paste0(path_parent, '/templates_ab'),
        data.path = data.path,
        eml.path = eml.path,
        dataset.title = 'Sphagnum and Vascular Plant Decomposition under Increasing Nitrogen Additions: 2014-2015',
        data.table = data.table,
        data.table.name = data.table.name,
        data.table.description = c('Decomposition data', 'Nitrogen data'),
        temporal.coverage = c('2014-05-01', '2015-10-31'),
        geographic.description = 'Alberta, Canada, 100 km south of Fort McMurray, Canada',
        geographic.coordinates = c('55.895', '112.094','55.895', '112.094'),
        maintenance.description = 'completed',
        user.id = user.id,
        user.domain = user.domain,
        package.id = 'edi.141.1',
        write.file = FALSE
      )
    )
  )
  
  # keywords.txt missing
  
  expect_error(
    suppressMessages(
      make_eml(
        path = paste0(path_parent, '/templates_kw'),
        data.path = data.path,
        eml.path = eml.path,
        dataset.title = 'Sphagnum and Vascular Plant Decomposition under Increasing Nitrogen Additions: 2014-2015',
        data.table = data.table,
        data.table.name = data.table.name,
        data.table.description = c('Decomposition data', 'Nitrogen data'),
        temporal.coverage = c('2014-05-01', '2015-10-31'),
        geographic.description = 'Alberta, Canada, 100 km south of Fort McMurray, Canada',
        geographic.coordinates = c('55.895', '112.094','55.895', '112.094'),
        maintenance.description = 'completed',
        user.id = user.id,
        user.domain = user.domain,
        package.id = 'edi.141.1',
        write.file = FALSE
      )
    )
  )
  
  # attributes.txt (content outside of table bounds)
  
  expect_error(
    suppressWarnings(
      suppressMessages(
        make_eml(
          path = paste0(path_parent, '/templates_attr'),
          data.path = data.path,
          eml.path = eml.path,
          dataset.title = 'Sphagnum and Vascular Plant Decomposition under Increasing Nitrogen Additions: 2014-2015',
          data.table = data.table,
          data.table.name = data.table.name,
          data.table.description = c('Decomposition data', 'Nitrogen data'),
          temporal.coverage = c('2014-05-01', '2015-10-31'),
          maintenance.description = 'completed',
          user.id = user.id,
          user.domain = user.domain,
          package.id = 'edi.141.1',
          write.file = FALSE
        )
      )
    )
  )
  
  # attributes.txt (missing definitions)
  
  expect_error(
    suppressWarnings(
      suppressMessages(
        make_eml(
          path = paste0(path_parent, '/templates_attr_df'),
          data.path = data.path,
          eml.path = eml.path,
          dataset.title = 'Sphagnum and Vascular Plant Decomposition under Increasing Nitrogen Additions: 2014-2015',
          data.table = data.table,
          data.table.name = data.table.name,
          data.table.description = c('Decomposition data', 'Nitrogen data'),
          temporal.coverage = c('2014-05-01', '2015-10-31'),
          maintenance.description = 'completed',
          user.id = user.id,
          user.domain = user.domain,
          package.id = 'edi.141.1',
          write.file = FALSE
        )
      )
    )
  )
  
  # attributes.txt (default format string present)
  
  expect_error(
    suppressWarnings(
      suppressMessages(
        make_eml(
          path = paste0(path_parent, '/templates_attr_dtf'),
          data.path = data.path,
          eml.path = eml.path,
          dataset.title = 'Sphagnum and Vascular Plant Decomposition under Increasing Nitrogen Additions: 2014-2015',
          data.table = data.table,
          data.table.name = data.table.name,
          data.table.description = c('Decomposition data', 'Nitrogen data'),
          temporal.coverage = c('2014-05-01', '2015-10-31'),
          maintenance.description = 'completed',
          user.id = user.id,
          user.domain = user.domain,
          package.id = 'edi.141.1',
          write.file = FALSE
        )
      )
    )
  )
  
  # attributes.txt (missing attribute class)
  
  expect_error(
    suppressWarnings(
      suppressMessages(
        make_eml(
          path = paste0(path_parent, '/templates_attr_cl'),
          data.path = data.path,
          eml.path = eml.path,
          dataset.title = 'Sphagnum and Vascular Plant Decomposition under Increasing Nitrogen Additions: 2014-2015',
          data.table = data.table,
          data.table.name = data.table.name,
          data.table.description = c('Decomposition data', 'Nitrogen data'),
          temporal.coverage = c('2014-05-01', '2015-10-31'),
          maintenance.description = 'completed',
          user.id = user.id,
          user.domain = user.domain,
          package.id = 'edi.141.1',
          write.file = FALSE
        )
      )
    )
  )
  
  # attributes.txt (missing date time string)
  
  expect_error(
    suppressWarnings(
      suppressMessages(
        make_eml(
          path = paste0(path_parent, '/templates_attr_dts'),
          data.path = data.path,
          eml.path = eml.path,
          dataset.title = 'Sphagnum and Vascular Plant Decomposition under Increasing Nitrogen Additions: 2014-2015',
          data.table = data.table,
          data.table.name = data.table.name,
          data.table.description = c('Decomposition data', 'Nitrogen data'),
          temporal.coverage = c('2014-05-01', '2015-10-31'),
          maintenance.description = 'completed',
          user.id = user.id,
          user.domain = user.domain,
          package.id = 'edi.141.1',
          write.file = FALSE
        )
      )
    )
  )
  
  # attributes.txt (default unit marker present)
  
  expect_error(
    suppressWarnings(
      make_eml(
        path = paste0(path_parent, '/templates_attr_unit'),
        data.path = data.path,
        eml.path = eml.path,
        dataset.title = 'Sphagnum and Vascular Plant Decomposition under Increasing Nitrogen Additions: 2014-2015',
        data.table = data.table,
        data.table.name = data.table.name,
        data.table.description = c('Decomposition data', 'Nitrogen data'),
        temporal.coverage = c('2014-05-01', '2015-10-31'),
        maintenance.description = 'completed',
        user.id = user.id,
        user.domain = user.domain,
        package.id = 'edi.141.1',
        write.file = FALSE
      )
    )
  )
  
  # attributes.txt (missing unit)
  
  expect_error(
    suppressWarnings(
      make_eml(
        path = paste0(path_parent, '/templates_attr_unitb'),
        data.path = data.path,
        eml.path = eml.path,
        dataset.title = 'Sphagnum and Vascular Plant Decomposition under Increasing Nitrogen Additions: 2014-2015',
        data.table = data.table,
        data.table.name = data.table.name,
        data.table.description = c('Decomposition data', 'Nitrogen data'),
        temporal.coverage = c('2014-05-01', '2015-10-31'),
        maintenance.description = 'completed',
        user.id = user.id,
        user.domain = user.domain,
        package.id = 'edi.141.1',
        write.file = FALSE
      )
    )
  )
  
  # attributes.txt (missing value code explanation missing)
  
  expect_error(
    suppressWarnings(
      make_eml(
        path = paste0(path_parent, '/templates_attr_mve'),
        data.path = data.path,
        eml.path = eml.path,
        dataset.title = 'Sphagnum and Vascular Plant Decomposition under Increasing Nitrogen Additions: 2014-2015',
        data.table = data.table,
        data.table.name = data.table.name,
        data.table.description = c('Decomposition data', 'Nitrogen data'),
        temporal.coverage = c('2014-05-01', '2015-10-31'),
        maintenance.description = 'completed',
        user.id = user.id,
        user.domain = user.domain,
        package.id = 'edi.141.1',
        write.file = FALSE
      )
    )
  )
  
  # attributes.txt (More than one missing value code present)
  
  expect_error(
    suppressWarnings(
      make_eml(
        path = paste0(path_parent, '/templates_attr_mvcn'),
        data.path = data.path,
        eml.path = eml.path,
        dataset.title = 'Sphagnum and Vascular Plant Decomposition under Increasing Nitrogen Additions: 2014-2015',
        data.table = data.table,
        data.table.name = data.table.name,
        data.table.description = c('Decomposition data', 'Nitrogen data'),
        temporal.coverage = c('2014-05-01', '2015-10-31'),
        maintenance.description = 'completed',
        user.id = user.id,
        user.domain = user.domain,
        package.id = 'edi.141.1',
        write.file = FALSE
      )
    )
  )
  
  # intellectual_rights.txt missing
  
  expect_error(
    make_eml(
      path = paste0(path_parent, '/templates_ir'),
      data.path = data.path,
      eml.path = eml.path,
      dataset.title = 'Sphagnum and Vascular Plant Decomposition under Increasing Nitrogen Additions: 2014-2015',
      data.table = data.table,
      data.table.name = data.table.name,
      data.table.description = c('Decomposition data', 'Nitrogen data'),
      temporal.coverage = c('2014-05-01', '2015-10-31'),
      maintenance.description = 'completed',
      user.id = user.id,
      user.domain = user.domain,
      package.id = 'edi.141.1',
      write.file = FALSE
    )
  )
  
  # methods.txt missing
  
  expect_error(
    make_eml(
      path = paste0(path_parent, '/templates_me'),
      data.path = data.path,
      eml.path = eml.path,
      dataset.title = 'Sphagnum and Vascular Plant Decomposition under Increasing Nitrogen Additions: 2014-2015',
      data.table = data.table,
      data.table.name = data.table.name,
      data.table.description = c('Decomposition data', 'Nitrogen data'),
      temporal.coverage = c('2014-05-01', '2015-10-31'),
      maintenance.description = 'completed',
      user.id = user.id,
      user.domain = user.domain,
      package.id = 'edi.141.1',
      write.file = FALSE
    )
  )
  
  
})

# Expect equal ----------------------------------------------------------------

testthat::test_that('Expect equal', {

  # Deprecated bounding_boxes.txt results in warning
  
  expect_warning(
    suppressMessages(
      make_eml(
        path = system.file('/examples/templates_bb', package = 'EMLassemblyline'),
        data.path = data.path,
        eml.path = eml.path,
        dataset.title = 'Sphagnum and Vascular Plant Decomposition under Increasing Nitrogen Additions: 2014-2015',
        data.table = data.table,
        data.table.name = data.table.name,
        data.table.description = c('Decomposition data', 'Nitrogen data'),
        temporal.coverage = c('2014-05-01', '2015-10-31'),
        maintenance.description = 'completed',
        user.id = user.id,
        user.domain = user.domain,
        package.id = 'edi.141.1',
        write.file = FALSE
      )
    )
  )
  
  # class = 'list'

  output <- make_eml(
    path = paste0(tempdir(), '/templates'),
    data.path = data.path,
    eml.path = eml.path,
    dataset.title = 'Sphagnum and Vascular Plant Decomposition under Increasing Nitrogen Additions: 2014-2015',
    data.table = data.table,
    data.table.name = data.table.name,
    data.table.description = c('Decomposition data', 'Nitrogen data'),
    temporal.coverage = c('2014-05-01', '2015-10-31'),
    maintenance.description = 'completed',
    user.id = user.id,
    user.domain = user.domain,
    package.id = 'edi.141.1',
    return.obj = TRUE,
    write.file = FALSE
  )
  
  expect_equal(
    class(output) == 'list',
    TRUE
  )
  
  # data.table.quote.character (is present)
  
  output <- make_eml(
    path = paste0(tempdir(), '/templates'),
    data.path = data.path,
    eml.path = eml.path,
    dataset.title = 'Sphagnum and Vascular Plant Decomposition under Increasing Nitrogen Additions: 2014-2015',
    data.table = data.table,
    data.table.name = data.table.name,
    data.table.description = c('Decomposition data', 'Nitrogen data'),
    data.table.quote.character = c("\'", "\'"),
    temporal.coverage = c('2014-05-01', '2015-10-31'),
    maintenance.description = 'completed',
    user.id = user.id,
    user.domain = user.domain,
    package.id = 'edi.141.1',
    return.obj = TRUE,
    write.file = FALSE
  )

  expect_equal(
    output$dataset$dataTable[[1]]$physical$dataFormat$textFormat$simpleDelimited$quoteCharacter,
    "'"
  )
  
  # other.entity
  
  output <- make_eml(
    path = system.file('/examples/templates_new_geocoverage', package = 'EMLassemblyline'),
    data.path = data.path,
    eml.path = eml.path,
    dataset.title = 'Sphagnum and Vascular Plant Decomposition under Increasing Nitrogen Additions: 2014-2015',
    data.table = data.table,
    data.table.name = data.table.name,
    data.table.description = c('Decomposition data', 'Nitrogen data'),
    data.table.quote.character = c("\'", "\'"),
    other.entity = 'ancillary_data.zip',
    other.entity.name = other.entity.name,
    other.entity.description = 'Ancillary data',
    temporal.coverage = c('2014-05-01', '2015-10-31'),
    maintenance.description = 'completed',
    user.id = user.id,
    user.domain = user.domain,
    package.id = 'edi.141.1',
    return.obj = TRUE,
    write.file = FALSE
  )
  
  expect_equal(
    length(output$dataset$otherEntity),
    1
  )
  
  expect_equal(
    output$dataset$otherEntity[[1]]$physical$objectName,
    'ancillary_data.zip'
  )
  
  # Using data.url
  
  output <- make_eml(
    path = system.file('/examples/templates_new_geocoverage', package = 'EMLassemblyline'),
    data.path = data.path,
    eml.path = eml.path,
    dataset.title = 'Sphagnum and Vascular Plant Decomposition under Increasing Nitrogen Additions: 2014-2015',
    data.table = data.table,
    data.table.name = data.table.name,
    data.table.description = c('Decomposition data', 'Nitrogen data'),
    data.table.quote.character = c("\'", "\'"),
    other.entity = 'ancillary_data.zip',
    other.entity.name = other.entity.name,
    other.entity.description = 'Ancillary data',
    data.url = 'https://lter.limnology.wisc.edu/sites/default/files/data',
    temporal.coverage = c('2014-05-01', '2015-10-31'),
    maintenance.description = 'completed',
    user.id = user.id,
    user.domain = user.domain,
    package.id = 'edi.141.1',
    return.obj = TRUE,
    write.file = FALSE
  )
  
  expect_true(
    stringr::str_detect(
      output$dataset$dataTable[[1]]$physical$distribution$online$url[[1]],
      'https://lter.limnology.wisc.edu/sites/default/files/data/decomp.csv'
    )
  )
  
  expect_true(
    stringr::str_detect(
      output$dataset$dataTable[[2]]$physical$distribution$online$url[[1]],
      'https://lter.limnology.wisc.edu/sites/default/files/data/nitrogen.csv'
    )
  )
  
  expect_true(
    stringr::str_detect(
      output$dataset$otherEntity[[1]]$physical$distribution$online$url[[1]],
      'https://lter.limnology.wisc.edu/sites/default/files/data/ancillary_data.zip'
    )
  )

  # # Using provenance
  # # class = 'eml'
  # 
  # if (all(class(edi_api) != 'try-error')){
  #   
  #   output <- make_eml(
  #     path = path,
  #     data.path = data.path,
  #     eml.path = eml.path,
  #     dataset.title = 'Sphagnum and Vascular Plant Decomposition under Increasing Nitrogen Additions: 2014-2015',
  #     data.table = data.table,
  #     data.table.description = c('Decomposition data', 'Nitrogen data'),
  #     data.table.quote.character = c("\'", "\'"),
  #     other.entity = 'ancillary_data.zip',
  #     other.entity.description = 'Ancillary data',
  #     data.url = 'https://lter.limnology.wisc.edu/sites/default/files/data',
  #     temporal.coverage = c('2014-05-01', '2015-10-31'),
  #     geographic.description = 'Alberta, Canada, 100 km south of Fort McMurray, Canada',
  #     geographic.coordinates = c('55.895', '112.094','55.895', '112.094'),
  #     maintenance.description = 'completed',
  #     user.id = user.id,
  #     user.domain = user.domain,
  #     package.id = 'edi.141.1',
  #     provenance = c('edi.100.1', 'edi.7.1'),
  #     return.obj = TRUE,
  #     write.file = FALSE
  #   )
  #   
  # }
  # 
  # expect_equal(
  #   class(output) == 'eml',
  #   TRUE
  # )
  
  # Using bounding_boxes.txt
  
  output <- suppressWarnings(
    make_eml(
      path = system.file('/examples/templates_bb', package = 'EMLassemblyline'),
      data.path = data.path,
      eml.path = eml.path,
      dataset.title = 'Sphagnum and Vascular Plant Decomposition under Increasing Nitrogen Additions: 2014-2015',
      data.table = data.table,
      data.table.name = data.table.name,
      data.table.description = c('Decomposition data', 'Nitrogen data'),
      data.table.quote.character = c("\'", "\'"),
      temporal.coverage = c('2014-05-01', '2015-10-31'),
      maintenance.description = 'completed',
      user.id = user.id,
      user.domain = user.domain,
      package.id = 'edi.141.1',
      return.obj = TRUE,
      write.file = FALSE
    )
  )
  
  expect_equal(
    length(output$dataset$coverage$geographicCoverage),
    2
  )
  
  # personnel.txt, project title is missing
  # class = 'list'
  
  output <- make_eml(
    path = paste0(path_parent, '/templates_pt'),
    data.path = data.path,
    eml.path = eml.path,
    dataset.title = 'Sphagnum and Vascular Plant Decomposition under Increasing Nitrogen Additions: 2014-2015',
    data.table = data.table,
    data.table.name = data.table.name,
    data.table.description = c('Decomposition data', 'Nitrogen data'),
    data.table.quote.character = c("\'", "\'"),
    temporal.coverage = c('2014-05-01', '2015-10-31'),
    maintenance.description = 'completed',
    user.id = user.id,
    user.domain = user.domain,
    package.id = 'edi.141.1',
    return.obj = TRUE,
    write.file = FALSE
  )
  
  expect_equal(
    output$dataset$project$title,
    'No project title to report'
  )
  
  # personnel.txt, only project funding is present
  # class = 'list'
  
  output <- suppressWarnings(
    make_eml(
      path = system.file('/examples/templates_pf', package = 'EMLassemblyline'),
      data.path = data.path,
      eml.path = eml.path,
      dataset.title = 'Sphagnum and Vascular Plant Decomposition under Increasing Nitrogen Additions: 2014-2015',
      data.table = data.table,
      data.table.name = data.table.name,
      data.table.description = c('Decomposition data', 'Nitrogen data'),
      data.table.quote.character = c("\'", "\'"),
      temporal.coverage = c('2014-05-01', '2015-10-31'),
      maintenance.description = 'completed',
      user.id = user.id,
      user.domain = user.domain,
      package.id = 'edi.141.1',
      return.obj = TRUE,
      write.file = FALSE
    )
  )
  
  expect_equal(
    output$dataset$project$funding,
    '6875309'
  )
  
  # personnel.txt (first project funding agency is present but all other fields blank)
  # class = 'list'
  
  output <- make_eml(
    path = paste0(path_parent, '/templates_pfab'),
    data.path = data.path,
    eml.path = eml.path,
    dataset.title = 'Sphagnum and Vascular Plant Decomposition under Increasing Nitrogen Additions: 2014-2015',
    data.table = data.table,
    data.table.name = data.table.name,
    data.table.description = c('Decomposition data', 'Nitrogen data'),
    data.table.quote.character = c("\'", "\'"),
    temporal.coverage = c('2014-05-01', '2015-10-31'),
    maintenance.description = 'completed',
    user.id = user.id,
    user.domain = user.domain,
    package.id = 'edi.141.1',
    return.obj = TRUE,
    write.file = FALSE
  )
  
  # personnel.txt (agency and number are listed in overall project)
  # class = 'list'
  
  output <- make_eml(
    path = paste0(path_parent, '/templates_pfan'),
    data.path = data.path,
    eml.path = eml.path,
    dataset.title = 'Sphagnum and Vascular Plant Decomposition under Increasing Nitrogen Additions: 2014-2015',
    data.table = data.table,
    data.table.name = data.table.name,
    data.table.description = c('Decomposition data', 'Nitrogen data'),
    data.table.quote.character = c("\'", "\'"),
    temporal.coverage = c('2014-05-01', '2015-10-31'),
    maintenance.description = 'completed',
    user.id = user.id,
    user.domain = user.domain,
    package.id = 'edi.141.1',
    return.obj = TRUE,
    write.file = FALSE
  )
  
  expect_equal(
    class(output) == 'list',
    TRUE
  )
  
  # personnel.txt (funding title is present, and all else is absent)
  # class = 'eml'
  
  output <- make_eml(
    path = paste0(path_parent, '/templates_pft'),
    data.path = data.path,
    eml.path = eml.path,
    dataset.title = 'Sphagnum and Vascular Plant Decomposition under Increasing Nitrogen Additions: 2014-2015',
    data.table = data.table,
    data.table.name = data.table.name,
    data.table.description = c('Decomposition data', 'Nitrogen data'),
    data.table.quote.character = c("\'", "\'"),
    temporal.coverage = c('2014-05-01', '2015-10-31'),
    maintenance.description = 'completed',
    user.id = user.id,
    user.domain = user.domain,
    package.id = 'edi.141.1',
    return.obj = TRUE,
    write.file = FALSE
  )
  
  expect_equal(
    class(output) == 'list',
    TRUE
  )
  
  # personnel.txt (funding title and number are present, agency is absent)
  # class = 'list'
  
  output <- make_eml(
    path = paste0(path_parent, '/templates_pftn'),
    data.path = data.path,
    eml.path = eml.path,
    dataset.title = 'Sphagnum and Vascular Plant Decomposition under Increasing Nitrogen Additions: 2014-2015',
    data.table = data.table,
    data.table.name = data.table.name,
    data.table.description = c('Decomposition data', 'Nitrogen data'),
    data.table.quote.character = c("\'", "\'"),
    temporal.coverage = c('2014-05-01', '2015-10-31'),
    maintenance.description = 'completed',
    user.id = user.id,
    user.domain = user.domain,
    package.id = 'edi.141.1',
    return.obj = TRUE,
    write.file = FALSE
  )
  
  expect_equal(
    class(output) == 'list',
    TRUE
  )
  
  # custom_units.txt, file is missing
  # class = 'eml'
  
  output <- suppressWarnings(
    make_eml(
      path = paste0(path_parent, '/templates_pf'),
      data.path = data.path,
      eml.path = eml.path,
      dataset.title = 'Sphagnum and Vascular Plant Decomposition under Increasing Nitrogen Additions: 2014-2015',
      data.table = data.table,
      data.table.name = data.table.name,
      data.table.description = c('Decomposition data', 'Nitrogen data'),
      data.table.quote.character = c("\'", "\'"),
      temporal.coverage = c('2014-05-01', '2015-10-31'),
      maintenance.description = 'completed',
      user.id = user.id,
      user.domain = user.domain,
      package.id = 'edi.141.1',
      return.obj = TRUE,
      write.file = FALSE
    )
  )
  
  expect_true(is.null(output$dataset$additionalInfo$section))
  expect_true(is.null(output$dataset$additionalInfo$para))

  # .docx templates are supported
  
  output <- suppressWarnings(
    make_eml(
      path = system.file('/examples/templates_docx', package = 'EMLassemblyline'),
      data.path = data.path,
      eml.path = eml.path,
      dataset.title = 'Sphagnum and Vascular Plant Decomposition under Increasing Nitrogen Additions: 2014-2015',
      data.table = data.table,
      data.table.name = data.table.name,
      data.table.description = c('Decomposition data', 'Nitrogen data'),
      data.table.quote.character = c("\'", "\'"),
      temporal.coverage = c('2014-05-01', '2015-10-31'),
      maintenance.description = 'completed',
      user.id = user.id,
      user.domain = user.domain,
      package.id = 'edi.141.1',
      return.obj = TRUE,
      write.file = FALSE
    )
  )
  
  expect_true(
    (nchar(output$dataset$abstract$para) > 100)
  )
  
  expect_true(
    (nchar(output$dataset$methods$methodStep$description$para) > 100)
  )
  
  expect_true(
    (nchar(output$dataset$additionalInfo$para) > 50)
  )
  
  # md templates are supported
  
  output <- suppressWarnings(
    make_eml(
      path = system.file('/examples/templates_md', package = 'EMLassemblyline'),
      data.path = data.path,
      eml.path = eml.path,
      dataset.title = 'Sphagnum and Vascular Plant Decomposition under Increasing Nitrogen Additions: 2014-2015',
      data.table = data.table,
      data.table.name = data.table.name,
      data.table.description = c('Decomposition data', 'Nitrogen data'),
      data.table.quote.character = c("\'", "\'"),
      temporal.coverage = c('2014-05-01', '2015-10-31'),
      maintenance.description = 'completed',
      user.id = user.id,
      user.domain = user.domain,
      package.id = 'edi.141.1',
      return.obj = TRUE,
      write.file = FALSE
    )
  )
  
  expect_equal(
    class(output) == 'list',
    TRUE
  )
  
  # data.table.name defaults to data.table
  
  output <- expect_warning(
    suppressMessages(
      make_eml(
        path = paste0(tempdir(), '/templates'),
        data.path = data.path,
        eml.path = eml.path,
        dataset.title = 'Sphagnum and Vascular Plant Decomposition under Increasing Nitrogen Additions: 2014-2015',
        data.table = data.table,
        data.table.description = data.table.description,
        temporal.coverage = c('2014-05-01', '2015-10-31'),
        maintenance.description = 'completed',
        user.id = user.id,
        user.domain = user.domain,
        package.id = 'edi.141.1',
        return.obj = TRUE,
        write.file = FALSE
      )
    )
  )
    
  for (i in 1:length(output$dataset$dataTable)) {
    expect_equal(
      output$dataset$dataTable[[i]]$entityName,
      output$dataset$dataTable[[i]]$physical$objectName
    )
  }
  
  # data.table.name
  
  output <- suppressMessages(
    make_eml(
      path = paste0(tempdir(), '/templates'),
      data.path = data.path,
      eml.path = eml.path,
      dataset.title = 'Sphagnum and Vascular Plant Decomposition under Increasing Nitrogen Additions: 2014-2015',
      data.table = data.table,
      data.table.name = data.table.name,
      data.table.description = data.table.description,
      temporal.coverage = c('2014-05-01', '2015-10-31'),
      maintenance.description = 'completed',
      user.id = user.id,
      user.domain = user.domain,
      package.id = 'edi.141.1',
      return.obj = TRUE,
      write.file = FALSE
    )
  )
  
  for (i in 1:length(output$dataset$dataTable)) {
    expect_equal(
      output$dataset$dataTable[[i]]$entityName,
      data.table.name[i]
    )
  }
  
  # other.entity.name defaults to other.entity
  
  output <- expect_warning(
    suppressMessages(
      make_eml(
        path = paste0(tempdir(), '/templates'),
        data.path = data.path,
        eml.path = eml.path,
        dataset.title = 'Sphagnum and Vascular Plant Decomposition under Increasing Nitrogen Additions: 2014-2015',
        other.entity = other.entity,
        other.entity.description = other.entity.description,
        temporal.coverage = c('2014-05-01', '2015-10-31'),
        maintenance.description = 'completed',
        user.id = user.id,
        user.domain = user.domain,
        package.id = 'edi.141.1',
        return.obj = TRUE,
        write.file = FALSE
      )
    )
  )
  
  for (i in 1:length(output$dataset$otherEntity)) {
    expect_equal(
      output$dataset$otherEntity[[i]]$entityName[[1]],
      output$dataset$otherEntity[[i]]$physical$objectName
    )
  }
  
  # other.entity.name
  
  output <- suppressMessages(
    make_eml(
      path = paste0(tempdir(), '/templates'),
      data.path = data.path,
      eml.path = eml.path,
      dataset.title = 'Sphagnum and Vascular Plant Decomposition under Increasing Nitrogen Additions: 2014-2015',
      other.entity = other.entity,
      other.entity.name = other.entity.name,
      other.entity.description = other.entity.description,
      temporal.coverage = c('2014-05-01', '2015-10-31'),
      maintenance.description = 'completed',
      user.id = user.id,
      user.domain = user.domain,
      package.id = 'edi.141.1',
      return.obj = TRUE,
      write.file = FALSE
    )
  )
  
  for (i in 1:length(output$dataset$otherEntity)) {
    expect_equal(
      output$dataset$otherEntity[[i]]$entityName[[1]],
      other.entity.name[i]
    )
  }

})

# Test usage with x (all templates and 2 data tables) -------------------------

testthat::test_that('Test usage with x (all templates and 2 data tables)', {
  
  # Using deprecated template bounding_boxes.txt results in warning
  
  input <- x_table
  input$template$geographic_coverage.txt <- NULL
  
  expect_warning(
    make_eml(
      data.path = data.path,
      eml.path = eml.path,
      dataset.title = 'Sphagnum and Vascular Plant Decomposition under Increasing Nitrogen Additions: 2014-2015',
      data.table = data.table,
      data.table.name = data.table.name,
      data.table.description = c('Decomposition data', 'Nitrogen data'),
      data.table.quote.character = c("\"","\""),
      data.url = data.url,
      temporal.coverage = c('2014-05-01', '2015-10-31'),
      maintenance.description = 'completed',
      user.id = user.id,
      user.domain = user.domain,
      package.id = 'edi.141.1',
      write.file = FALSE,
      x = input
    )
  )
  
  # Remove deprecated template
  
  x_table$template$bounding_boxes.txt <- NULL

  # Missing path has no effect
  
  output <- make_eml(
    data.path = data.path,
    eml.path = eml.path,
    dataset.title = 'Sphagnum and Vascular Plant Decomposition under Increasing Nitrogen Additions: 2014-2015',
    data.table = data.table,
    data.table.name = data.table.name,
    data.table.description = c('Decomposition data', 'Nitrogen data'),
    data.table.quote.character = c("\"","\""),
    data.url = data.url,
    temporal.coverage = c('2014-05-01', '2015-10-31'),
    maintenance.description = 'completed',
    user.id = user.id,
    user.domain = user.domain,
    package.id = 'edi.141.1',
    return.obj = TRUE,
    write.file = FALSE,
    x = x_table
  )
  
  expect_equal(
    class(output) == 'list',
    TRUE
  )
  
  # Missing path and eml.path has no effect if write.file = FALSE
  
  output <- make_eml(
    data.path = data.path,
    dataset.title = 'Sphagnum and Vascular Plant Decomposition under Increasing Nitrogen Additions: 2014-2015',
    data.table = data.table,
    data.table.name = data.table.name,
    data.table.description = c('Decomposition data', 'Nitrogen data'),
    data.table.quote.character = c("\"","\""),
    data.url = data.url,
    temporal.coverage = c('2014-05-01', '2015-10-31'),
    maintenance.description = 'completed',
    user.id = user.id,
    user.domain = user.domain,
    package.id = 'edi.141.1',
    return.obj = TRUE,
    write.file = FALSE,
    x = x_table
  )
  
  expect_equal(
    class(output) == 'list',
    TRUE
  )
  
  # Missing eml.path and write.file = TRUE results in error
  
  expect_error(
    suppressMessages(
      make_eml(
        data.path = data.path,
        dataset.title = 'Sphagnum and Vascular Plant Decomposition under Increasing Nitrogen Additions: 2014-2015',
        data.table = data.table,
        data.table.name = data.table.name,
        data.table.description = c('Decomposition data', 'Nitrogen data'),
        data.table.quote.character = c("\"","\""),
        data.url = data.url,
        temporal.coverage = c('2014-05-01', '2015-10-31'),
        geographic.description = 'Alberta, Canada, 100 km south of Fort McMurray, Canada',
        geographic.coordinates = c('55.895', '112.094','55.895', '112.094'),
        maintenance.description = 'completed',
        user.id = user.id,
        user.domain = user.domain,
        package.id = 'edi.141.1',
        return.obj = TRUE,
        write.file = TRUE,
        x = x_table
      )
    )
  )
  
  # Missing eml.path and write.file = FALSE has no effect
  
  output <- make_eml(
    data.path = data.path,
    dataset.title = 'Sphagnum and Vascular Plant Decomposition under Increasing Nitrogen Additions: 2014-2015',
    data.table = data.table,
    data.table.name = data.table.name,
    data.table.description = c('Decomposition data', 'Nitrogen data'),
    data.table.quote.character = c("\"","\""),
    data.url = data.url,
    temporal.coverage = c('2014-05-01', '2015-10-31'),
    maintenance.description = 'completed',
    user.id = user.id,
    user.domain = user.domain,
    package.id = 'edi.141.1',
    return.obj = TRUE,
    write.file = FALSE,
    x = x_table
  )
  
  expect_equal(
    class(output) == 'list',
    TRUE
  )

  # Arguments supplied to function in long form
  
  output <- make_eml(
    path = path,
    data.path = data.path,
    eml.path = eml.path,
    dataset.title = 'Sphagnum and Vascular Plant Decomposition under Increasing Nitrogen Additions: 2014-2015',
    data.table = data.table,
    data.table.name = data.table.name,
    data.table.description = c('Decomposition data', 'Nitrogen data'),
    data.table.quote.character = c("\"","\""),
    data.url = data.url,
    temporal.coverage = c('2014-05-01', '2015-10-31'),
    maintenance.description = 'completed',
    user.id = user.id,
    user.domain = user.domain,
    package.id = 'edi.141.1',
    return.obj = TRUE,
    write.file = FALSE,
    x = x_table
  )
  
  expect_equal(
    class(output) == 'list',
    TRUE
  )
  
  # Arguments supplied to function via x
  
  output <- suppressWarnings(
    do.call(
      make_eml, 
      x_table_docall[
        names(x_table_docall) %in% names(formals(make_eml))
      ]
    )
  )
  
  expect_equal(
    class(output) == 'list',
    TRUE
  )
  
  # New geographic_coverage.txt is supported
  
  input <- x_table

  input$template$geographic_coverage.txt <- NULL
  
  input <- suppressMessages(
    template_geographic_coverage(
      data.table = 'nitrogen.csv', 
      site.col = 'site_name', 
      lat.col = 'site_lat',
      lon.col = 'site_lon',
      x = input,
      write.file = FALSE
    ) 
  )
  
  input$template$bounding_boxes.txt <- NULL
    
  output <- make_eml(
    path = path,
    data.path = data.path,
    eml.path = eml.path,
    dataset.title = 'Sphagnum and Vascular Plant Decomposition under Increasing Nitrogen Additions: 2014-2015',
    data.table = data.table,
    data.table.name = data.table.name,
    data.table.description = c('Decomposition data', 'Nitrogen data'),
    data.table.quote.character = c("\"","\""),
    data.url = data.url,
    temporal.coverage = c('2014-05-01', '2015-10-31'),
    maintenance.description = 'completed',
    user.id = user.id,
    user.domain = user.domain,
    package.id = 'edi.141.1',
    return.obj = TRUE,
    write.file = FALSE,
    x = input
  )
  
  expect_equal(
    length(output$dataset$coverage$geographicCoverage) > 5,
    TRUE
  )
  
  
  
})

# Test usage with x (all templates, 2 data tables, and 1 other entity) --------

testthat::test_that('Test usage with x (all templates, 2 data tables, and 1 other entity)', {
  
  # Use of deprecated template bounding_boxes.txt results in warning
  
  input <- x_table_other
  input$template$geographic_coverage.txt <- NULL
  
  expect_warning(
    make_eml(
      path = system.file('/examples/templates_new_geocoverage', package = 'EMLassemblyline'),
      data.path = data.path,
      eml.path = eml.path,
      dataset.title = 'Sphagnum and Vascular Plant Decomposition under Increasing Nitrogen Additions: 2014-2015',
      data.table = data.table,
      data.table.name = data.table.name,
      data.table.description = c('Decomposition data', 'Nitrogen data'),
      data.table.quote.character = c("\"","\""),
      data.url = data.url,
      other.entity = 'ancillary_data.zip',
      other.entity.description = 'Ancillary data',
      temporal.coverage = c('2014-05-01', '2015-10-31'),
      maintenance.description = 'completed',
      user.id = user.id,
      user.domain = user.domain,
      package.id = 'edi.141.1',
      return.obj = FALSE,
      write.file = FALSE,
      x = input
    )
  )
  
  # Remove deprecated template bounding_boxes.txt
  
  x_table_other$template$bounding_boxes.txt <- NULL
  
  # Arguments supplied to function in long form
  
  output <- make_eml(
    path = path,
    data.path = data.path,
    eml.path = eml.path,
    dataset.title = 'Sphagnum and Vascular Plant Decomposition under Increasing Nitrogen Additions: 2014-2015',
    data.table = data.table,
    data.table.name = data.table.name,
    data.table.description = c('Decomposition data', 'Nitrogen data'),
    data.table.quote.character = c("\"","\""),
    data.url = data.url,
    other.entity = 'ancillary_data.zip',
    other.entity.name = other.entity.name,
    other.entity.description = 'Ancillary data',
    temporal.coverage = c('2014-05-01', '2015-10-31'),
    maintenance.description = 'completed',
    user.id = user.id,
    user.domain = user.domain,
    package.id = 'edi.141.1',
    return.obj = TRUE,
    write.file = FALSE,
    x = x_table_other
  )
  
  expect_equal(
    class(output) == 'list',
    TRUE
  )

  output <- make_eml(
    path = path,
    data.path = data.path,
    eml.path = eml.path,
    dataset.title = 'Sphagnum and Vascular Plant Decomposition under Increasing Nitrogen Additions: 2014-2015',
    data.table = data.table,
    data.table.name = data.table.name,
    data.table.description = c('Decomposition data', 'Nitrogen data'),
    data.table.quote.character = c("\"","\""),
    data.url = data.url,
    other.entity = 'ancillary_data.zip',
    other.entity.name = other.entity.name,
    other.entity.description = 'Ancillary data',
    temporal.coverage = c('2014-05-01', '2015-10-31'),
    maintenance.description = 'completed',
    user.id = user.id,
    user.domain = user.domain,
    package.id = 'edi.141.1',
    return.obj = TRUE,
    write.file = FALSE,
    x = x_table_other
  )
  
  expect_equal(
    class(output) == 'list',
    TRUE
  )
  
  # Arguments supplied to function via x
  
  output <- suppressWarnings(
    do.call(
      make_eml, 
      x_table_other_docall[
        names(x_table_other_docall) %in% names(formals(make_eml))
      ]
    )
  )
  
  expect_equal(
    class(output) == 'list',
    TRUE
  )
  
})

# Test usage with x (all templates and 1 other entity) ------------------------

testthat::test_that('Test usage with x (all templates and 1 other entity)', {
  
  # Use of deprecated template bounding_boxes.txt results in warning
  
  input <- x_other
  input$template$geographic_coverage.txt <- NULL
  
  expect_warning(
    make_eml(
      path = path,
      data.path = data.path,
      eml.path = eml.path,
      dataset.title = 'Sphagnum and Vascular Plant Decomposition under Increasing Nitrogen Additions: 2014-2015',
      data.url = data.url,
      other.entity = 'ancillary_data.zip',
      other.entity.description = 'Ancillary data',
      temporal.coverage = c('2014-05-01', '2015-10-31'),
      maintenance.description = 'completed',
      user.id = user.id,
      user.domain = user.domain,
      package.id = 'edi.141.1',
      return.obj = TRUE,
      write.file = FALSE,
      x = input
    )
  )
  
  # Remove deprecated template bounding_boxes.txt
  
  x_other$template$bounding_boxes.txt <- NULL
  
  # Arguments supplied to function in long form
  
  output <- make_eml(
    path = path,
    data.path = data.path,
    eml.path = eml.path,
    dataset.title = 'Sphagnum and Vascular Plant Decomposition under Increasing Nitrogen Additions: 2014-2015',
    data.url = data.url,
    other.entity = 'ancillary_data.zip',
    other.entity.name = other.entity.name,
    other.entity.description = 'Ancillary data',
    temporal.coverage = c('2014-05-01', '2015-10-31'),
    maintenance.description = 'completed',
    user.id = user.id,
    user.domain = user.domain,
    package.id = 'edi.141.1',
    return.obj = TRUE,
    write.file = FALSE,
    x = x_other
  )
  
  expect_equal(
    class(output) == 'list',
    TRUE
  )
  
  # Arguments supplied to function via x
  
  output <- suppressWarnings(
    do.call(
      make_eml, 
      x_other_docall[
        names(x_other_docall) %in% names(formals(make_eml))
      ]
    )
  )
  
  expect_equal(
    class(output) == 'list',
    TRUE
  )
  
})

# Missing value codes should not be mandatory ---------------------------------
# Methods for handling datasets without missing value codes changed in the 
# recent EML v2.0.0 dependency.

testthat::test_that('Missing value codes are not required', {
  
  # Remove missing value codes and explanations in attributes_decomp.csv with 
  # empty fields
  
  x_missing_value_codes_empty <- x_table
  
  x_missing_value_codes_empty$template$attributes_decomp.txt$content$missingValueCode <- ''
  x_missing_value_codes_empty$template$attributes_decomp.txt$content$missingValueCodeExplanation <- ''
  x_missing_value_codes_empty$template$bounding_boxes.txt <- NULL
  
  output <- suppressMessages(
    make_eml(
      data.path = data.path,
      eml.path = eml.path,
      dataset.title = 'Sphagnum and Vascular Plant Decomposition under Increasing Nitrogen Additions: 2014-2015',
      data.table = data.table,
      data.table.name = data.table.name,
      data.table.description = c('Decomposition data', 'Nitrogen data'),
      temporal.coverage = c('2014-05-01', '2015-10-31'),
      maintenance.description = 'completed',
      user.id = user.id,
      user.domain = user.domain,
      package.id = 'edi.141.1',
      write.file = FALSE,
      return.obj = TRUE,
      x = x_missing_value_codes_empty
    )
  )
  
  expect_true(
    all(is.na(output$dataset$dataTable[[1]]$attributeList$attribute[[6]]$missingValueCode))
  )
  
})

# geographicCoverage ----------------------------------------------------------

testthat::test_that('geographicCoverage', {
  
  # Error if more than one source of geographicCoverage
  # 3 sources (geographic.coverage, geographic_coverage.txt, 
  # bounding_boxes.txt)
  
  x_geographicCoverage <- x_table
  x_geographicCoverage$template$bounding_boxes.txt$content <- data.frame(
    geographicDescription = c('site1', 'site2'),
    northBoundingCoordinate = c('45', '43'),
    southBoundingCoordinate = c('30', '33'),
    eastBoundingCoordinate = c('-23', '-43'),
    westBoundingCoordinate = c('-123', '-123'),
    stringsAsFactors = F
  )
  x_geographicCoverage$template$geographic_coverage.txt$content <- data.frame(
    geographicDescription = c('site1', 'site2'),
    northBoundingCoordinate = c('45', '43'),
    southBoundingCoordinate = c('30', '33'),
    eastBoundingCoordinate = c('-23', '-43'),
    westBoundingCoordinate = c('-123', '-123'),
    stringsAsFactors = F
  )
  
  expect_error(
    suppressMessages(
      make_eml(
        data.path = data.path,
        eml.path = eml.path,
        dataset.title = 'Sphagnum and Vascular Plant Decomposition under Increasing Nitrogen Additions: 2014-2015',
        data.table = data.table,
        data.table.name = data.table.name,
        data.table.description = c('Decomposition data', 'Nitrogen data'),
        temporal.coverage = c('2014-05-01', '2015-10-31'),
        geographic.description = 'Alberta, Canada, 100 km south of Fort McMurray, Canada',
        geographic.coordinates = c('55.895', '112.094','55.895', '112.094'),
        maintenance.description = 'completed',
        user.id = user.id,
        user.domain = user.domain,
        package.id = 'edi.141.1',
        write.file = FALSE,
        x = x_geographicCoverage
      )
    )
  )
  
  # Error if more than one source of geographicCoverage
  # 2 sources (geographic.coverage, geographic_coverage.txt)
  
  x_geographicCoverage$template$bounding_boxes.txt$content <- NULL

  expect_error(
    suppressMessages(
      make_eml(
        data.path = data.path,
        eml.path = eml.path,
        dataset.title = 'Sphagnum and Vascular Plant Decomposition under Increasing Nitrogen Additions: 2014-2015',
        data.table = data.table,
        data.table.name = data.table.name,
        data.table.description = c('Decomposition data', 'Nitrogen data'),
        temporal.coverage = c('2014-05-01', '2015-10-31'),
        geographic.description = 'Alberta, Canada, 100 km south of Fort McMurray, Canada',
        geographic.coordinates = c('55.895', '112.094','55.895', '112.094'),
        maintenance.description = 'completed',
        user.id = user.id,
        user.domain = user.domain,
        package.id = 'edi.141.1',
        write.file = FALSE,
        x = x_geographicCoverage
      )
    )
  )
  
  # Create geographic coverage from geographic_coverage.txt

  output <- suppressMessages(
    make_eml(
      data.path = data.path,
      eml.path = eml.path,
      dataset.title = 'Sphagnum and Vascular Plant Decomposition under Increasing Nitrogen Additions: 2014-2015',
      data.table = data.table,
      data.table.name = data.table.name,
      data.table.description = c('Decomposition data', 'Nitrogen data'),
      temporal.coverage = c('2014-05-01', '2015-10-31'),
      maintenance.description = 'completed',
      user.id = user.id,
      user.domain = user.domain,
      package.id = 'edi.141.1',
      write.file = FALSE,
      x = x_geographicCoverage,
      return.obj = TRUE
    )
  )
  
  expect_true(
    length(output$dataset$coverage$geographicCoverage) >= 1
  )
  
  # Create geographic coverage from geographic.coordinates
  
  x_geographicCoverage$template$geographic_coverage.txt$content <- NULL
  
  output <- suppressMessages(
    make_eml(
      data.path = data.path,
      eml.path = eml.path,
      dataset.title = 'Sphagnum and Vascular Plant Decomposition under Increasing Nitrogen Additions: 2014-2015',
      data.table = data.table,
      data.table.name = data.table.name,
      data.table.description = c('Decomposition data', 'Nitrogen data'),
      temporal.coverage = c('2014-05-01', '2015-10-31'),
      geographic.description = 'Alberta, Canada, 100 km south of Fort McMurray, Canada',
      geographic.coordinates = c('55.895', '112.094','55.895', '112.094'),
      maintenance.description = 'completed',
      user.id = user.id,
      user.domain = user.domain,
      package.id = 'edi.141.1',
      write.file = FALSE,
      x = x_geographicCoverage,
      return.obj = TRUE
    )
  )
  
  expect_true(
    length(output$dataset$coverage$geographicCoverage) >= 1
  )
  
  # Create geographic coverage from bounding_boxes.txt
  
  x_geographicCoverage$template$bounding_boxes.txt$content <- data.frame(
    geographicDescription = c('site1', 'site2'),
    northBoundingCoordinate = c('45', '43'),
    southBoundingCoordinate = c('30', '33'),
    eastBoundingCoordinate = c('-23', '-43'),
    westBoundingCoordinate = c('-123', '-123'),
    stringsAsFactors = F
  )
  
  output <- suppressMessages(
    suppressWarnings(
      make_eml(
        data.path = data.path,
        eml.path = eml.path,
        dataset.title = 'Sphagnum and Vascular Plant Decomposition under Increasing Nitrogen Additions: 2014-2015',
        data.table = data.table,
        data.table.name = data.table.name,
        data.table.description = c('Decomposition data', 'Nitrogen data'),
        temporal.coverage = c('2014-05-01', '2015-10-31'),
        maintenance.description = 'completed',
        user.id = user.id,
        user.domain = user.domain,
        package.id = 'edi.141.1',
        write.file = FALSE,
        x = x_geographicCoverage,
        return.obj = TRUE
      )
    )
  )
  
  expect_true(
    length(output$dataset$coverage$geographicCoverage) >= 1
  )

})


# data.table.url & other.entity.url -------------------------------------------

testthat::test_that('data.table.url and other.entity.url', {
  
  file.copy(
    from  = system.file('/examples/pkg_260', package = 'EMLassemblyline'),
    to = tempdir(),
    recursive = TRUE)
  
  expect_error(
    make_eml(
      path = paste0(tempdir(), "/pkg_260/metadata_templates"),
      data.path = paste0(tempdir(), "/pkg_260/data_objects"),
      eml.path = paste0(tempdir(), "/pkg_260/eml"),
      dataset.title = 'Sphagnum and Vascular Plant Decomposition under Increasing Nitrogen Additions: 2014-2015',
      data.table = c("decomp.csv", "nitrogen.csv"),
      data.table.name = c("decomp table name", "nitrogen table name"),
      data.table.description = c('Decomposition data', 'Nitrogen data'),
      data.table.url = c("https://datatableurl_1"),
      other.entity = c("ancillary_data.zip", "processing_and_analysis.R"),
      other.entity.name = c("Ancillary data", "Processing and analysis script"),
      other.entity.description = c("Ancillary data description", "Processing and analysis script description"),
      other.entity.url = c("https://otherentityurl_1", "https://otherentityurl_2"),
      temporal.coverage = c('2014-05-01', '2015-10-31'),
      maintenance.description = 'completed',
      user.id = "userid",
      user.domain = "user.domain",
      package.id = 'edi.141.1',
      write.file = F,
      return.obj = F
    )
  )
  
  expect_error(
    make_eml(
      path = paste0(tempdir(), "/pkg_260/metadata_templates"),
      data.path = paste0(tempdir(), "/pkg_260/data_objects"),
      eml.path = paste0(tempdir(), "/pkg_260/eml"),
      dataset.title = 'Sphagnum and Vascular Plant Decomposition under Increasing Nitrogen Additions: 2014-2015',
      data.table = c("decomp.csv", "nitrogen.csv"),
      data.table.name = c("decomp table name", "nitrogen table name"),
      data.table.description = c('Decomposition data', 'Nitrogen data'),
      data.table.url = c("https://datatableurl_1", "https://datatableurl_2"),
      other.entity = c("ancillary_data.zip", "processing_and_analysis.R"),
      other.entity.name = c("Ancillary data", "Processing and analysis script"),
      other.entity.description = c("Ancillary data description", "Processing and analysis script description"),
      other.entity.url = c("https://otherentityurl_1"),
      temporal.coverage = c('2014-05-01', '2015-10-31'),
      maintenance.description = 'completed',
      user.id = "userid",
      user.domain = "user.domain",
      package.id = 'edi.141.1',
      write.file = F,
      return.obj = F
    )
  )
  
  output <- make_eml(
    path = paste0(tempdir(), "/pkg_260/metadata_templates"),
    data.path = paste0(tempdir(), "/pkg_260/data_objects"),
    eml.path = paste0(tempdir(), "/pkg_260/eml"),
    dataset.title = 'Sphagnum and Vascular Plant Decomposition under Increasing Nitrogen Additions: 2014-2015',
    data.table = c("decomp.csv", "nitrogen.csv"),
    data.table.name = c("decomp table name", "nitrogen table name"),
    data.table.description = c('Decomposition data', 'Nitrogen data'),
    data.table.url = c("https://datatableurl_1", "https://datatableurl_2"),
    other.entity = c("ancillary_data.zip", "processing_and_analysis.R"),
    other.entity.name = c("Ancillary data", "Processing and analysis script"),
    other.entity.description = c("Ancillary data description", "Processing and analysis script description"),
    other.entity.url = c("https://otherentityurl_1", "https://otherentityurl_2"),
    temporal.coverage = c('2014-05-01', '2015-10-31'),
    maintenance.description = 'completed',
    user.id = "userid",
    user.domain = "user.domain",
    package.id = 'edi.141.1',
    write.file = F,
    return.obj = T
  )
  
  expect_true(
    output$dataset$dataTable[[1]]$physical$distribution$online$url ==
      "https://datatableurl_1")
  expect_true(
    output$dataset$dataTable[[2]]$physical$distribution$online$url ==
      "https://datatableurl_2")
  expect_true(
    output$dataset$otherEntity[[1]]$physical$distribution$online$url ==
      "https://otherentityurl_1")
  expect_true(
    output$dataset$otherEntity[[2]]$physical$distribution$online$url ==
      "https://otherentityurl_2")
  
  unlink(paste0(tempdir(), "/pkg_260/metadata_templates"), recursive = T, force = T)
  
})

