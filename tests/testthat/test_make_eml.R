context('Make EML')
library(EMLassemblyline)
suppressWarnings(library(EML))

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
other.entity.description <- 'Ancillary data'
provenance <- 'edi.100.1'

x_table <- read_files(
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

x_table_other <- read_files(
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
  other.entity = 'ancillary.data.zip'
)

x_other <- read_files(
  path = system.file(
    '/examples/templates', 
    package = 'EMLassemblyline'
  ),
  data.path = system.file(
    '/examples/data',
    package = 'EMLassemblyline'
  ),
  other.entity = 'ancillary.data.zip'
)

# Add arguments to list structure x

x_table$argument$data.path <- data.path
x_table$argument$data.table <- data.table
x_table$argument$data.table.description <- data.table.description
x_table$argument$data.table.quote.character <- data.table.quote.character
x_table$argument$data.url <- data.url
x_table$argument$dataset.title <- dataset.title
x_table$argument$eml.path <- eml.path
x_table$argument$geographic.coordinates <- geographic.coordinates
x_table$argument$geographic.description <- geographic.description
x_table$argument$maintenance.description <- maintenance.description
x_table$argument$package.id <- package.id
x_table$argument$path <- path
x_table$argument$provenance <- provenance
x_table$argument$return.obj <- TRUE
x_table$argument$temporal.coverage <- temporal.coverage
x_table$argument$user.domain <- user.domain
x_table$argument$user.id <- user.id
x_table$argument$write.file <- FALSE
x_table$argument$x <- x_table

x_table_other$argument$data.path <- data.path
x_table_other$argument$data.table <- data.table
x_table_other$argument$data.table.description <- data.table.description
x_table_other$argument$data.table.quote.character <- data.table.quote.character
x_table_other$argument$other.entity <- other.entity
x_table_other$argument$other.entity.description <- other.entity.description
x_table_other$argument$data.url <- data.url
x_table_other$argument$dataset.title <- dataset.title
x_table_other$argument$eml.path <- eml.path
x_table_other$argument$geographic.coordinates <- geographic.coordinates
x_table_other$argument$geographic.description <- geographic.description
x_table_other$argument$maintenance.description <- maintenance.description
x_table_other$argument$package.id <- package.id
x_table_other$argument$path <- path
x_table_other$argument$provenance <- provenance
x_table_other$argument$return.obj <- TRUE
x_table_other$argument$temporal.coverage <- temporal.coverage
x_table_other$argument$user.domain <- user.domain
x_table_other$argument$user.id <- user.id
x_table_other$argument$write.file <- FALSE
x_table_other$argument$x <- x_table_other

x_other$argument$data.path <- data.path
x_other$argument$other.entity <- other.entity
x_other$argument$other.entity.description <- other.entity.description
x_other$argument$data.url <- data.url
x_other$argument$dataset.title <- dataset.title
x_other$argument$eml.path <- eml.path
x_other$argument$geographic.coordinates <- geographic.coordinates
x_other$argument$geographic.description <- geographic.description
x_other$argument$maintenance.description <- maintenance.description
x_other$argument$package.id <- package.id
x_other$argument$path <- path
x_other$argument$provenance <- provenance
x_other$argument$return.obj <- TRUE
x_other$argument$temporal.coverage <- temporal.coverage
x_other$argument$user.domain <- user.domain
x_other$argument$user.id <- user.id
x_other$argument$write.file <- FALSE
x_other$argument$x <- x_other

# File paths within R library

path.inst <- system.file(
  '/examples/templates/abstract.txt', 
  package = 'EMLassemblyline'
)
path.inst <- substr(path.inst, 1, nchar(path.inst)-23)

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
      user.domain = user.domain
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
      user.id = user.id,
      user.domain = user.domain
    )
  )
  
  # user.id
  
  expect_warning(
    make_eml(
      path = path,
      dataset.title = dataset.title,
      temporal.coverage = temporal.coverage,
      geographic.description = geographic.description,
      geographic.coordinates = geographic.coordinates,
      maintenance.description = maintenance.description,
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
      user.id = 'csmith',
      user.domain = c('LTER', 'EDI')
    )
  )
  
  # user.domain
  
  expect_warning(
    make_eml(
      path = path,
      dataset.title = dataset.title,
      temporal.coverage = temporal.coverage,
      geographic.description = geographic.description,
      geographic.coordinates = geographic.coordinates,
      maintenance.description = maintenance.description,
      user.id = user.id
    )
  )
  
  # user.domain (unsupported user.domain)
  
  expect_warning(
    make_eml(
      path = path,
      dataset.title = dataset.title,
      temporal.coverage = temporal.coverage,
      geographic.description = geographic.description,
      geographic.coordinates = geographic.coordinates,
      maintenance.description = maintenance.description,
      user.id = 'csmith',
      user.domain = 'EDEYE'
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
    make_eml(
      path = path,
      data.path = data.path,
      eml.path = eml.path,
      dataset.title = 'Sphagnum and Vascular Plant Decomposition under Increasing Nitrogen Additions: 2014-2015',
      data.table = data.table,
      data.table.description = c('Decomposition data', 'Nitrogen data'),
      temporal.coverage = c('2014-05-01', '2015-10-31'),
      geographic.description = 'Alberta, Canada, 100 km south of Fort McMurray, Canada',
      geographic.coordinates = c('55.895', '112.094','55.895', '112.094'),
      maintenance.description = 'completed',
      user.id = user.id,
      user.domain = user.domain,
      package.id = 'edi.141',
      write.file = FALSE
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
      data.table.description = c('Decomposition data', 'Nitrogen data'),
      other.entity = 'ancillary_data.zip',
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
      data.table.description = c('Decomposition data', 'Nitrogen data'),
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
  
  # other.entity and other.entity.description mismatch
  
  expect_error(
    make_eml(
      path = path,
      data.path = data.path,
      eml.path = eml.path,
      dataset.title = 'Sphagnum and Vascular Plant Decomposition under Increasing Nitrogen Additions: 2014-2015',
      data.table = data.table,
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
    make_eml(
      path = paste0(path_parent, '/templates_ab'),
      data.path = data.path,
      eml.path = eml.path,
      dataset.title = 'Sphagnum and Vascular Plant Decomposition under Increasing Nitrogen Additions: 2014-2015',
      data.table = data.table,
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
  
  # keywords.txt missing
  
  expect_error(
    make_eml(
      path = paste0(path_parent, '/templates_kw'),
      data.path = data.path,
      eml.path = eml.path,
      dataset.title = 'Sphagnum and Vascular Plant Decomposition under Increasing Nitrogen Additions: 2014-2015',
      data.table = data.table,
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
  
  # attributes.txt (content outside of table bounds)
  
  expect_error(
    make_eml(
      path = paste0(path_parent, '/templates_attr'),
      data.path = data.path,
      eml.path = eml.path,
      dataset.title = 'Sphagnum and Vascular Plant Decomposition under Increasing Nitrogen Additions: 2014-2015',
      data.table = data.table,
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
  
  # attributes.txt (missing definitions)
  
  expect_error(
    make_eml(
      path = paste0(path_parent, '/templates_attr_df'),
      data.path = data.path,
      eml.path = eml.path,
      dataset.title = 'Sphagnum and Vascular Plant Decomposition under Increasing Nitrogen Additions: 2014-2015',
      data.table = data.table,
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
  
  # attributes.txt (default format string present)
  
  expect_error(
    make_eml(
      path = paste0(path_parent, '/templates_attr_dtf'),
      data.path = data.path,
      eml.path = eml.path,
      dataset.title = 'Sphagnum and Vascular Plant Decomposition under Increasing Nitrogen Additions: 2014-2015',
      data.table = data.table,
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
  
  # attributes.txt (missing attribute class)
  
  expect_error(
    make_eml(
      path = paste0(path_parent, '/templates_attr_cl'),
      data.path = data.path,
      eml.path = eml.path,
      dataset.title = 'Sphagnum and Vascular Plant Decomposition under Increasing Nitrogen Additions: 2014-2015',
      data.table = data.table,
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
  
  # attributes.txt (missing date time string)
  
  expect_error(
    make_eml(
      path = paste0(path_parent, '/templates_attr_dts'),
      data.path = data.path,
      eml.path = eml.path,
      dataset.title = 'Sphagnum and Vascular Plant Decomposition under Increasing Nitrogen Additions: 2014-2015',
      data.table = data.table,
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
  
  # attributes.txt (default unit marker present)
  
  expect_error(
    make_eml(
      path = paste0(path_parent, '/templates_attr_unit'),
      data.path = data.path,
      eml.path = eml.path,
      dataset.title = 'Sphagnum and Vascular Plant Decomposition under Increasing Nitrogen Additions: 2014-2015',
      data.table = data.table,
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
  
  # attributes.txt (missing unit)
  
  expect_error(
    make_eml(
      path = paste0(path_parent, '/templates_attr_unitb'),
      data.path = data.path,
      eml.path = eml.path,
      dataset.title = 'Sphagnum and Vascular Plant Decomposition under Increasing Nitrogen Additions: 2014-2015',
      data.table = data.table,
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
  
  # attributes.txt (missing value code explanation missing)
  
  expect_error(
    make_eml(
      path = paste0(path_parent, '/templates_attr_mve'),
      data.path = data.path,
      eml.path = eml.path,
      dataset.title = 'Sphagnum and Vascular Plant Decomposition under Increasing Nitrogen Additions: 2014-2015',
      data.table = data.table,
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
  
  # attributes.txt (More than one missing value codepresent)
  
  expect_error(
    make_eml(
      path = paste0(path_parent, '/templates_attr_mvcn'),
      data.path = data.path,
      eml.path = eml.path,
      dataset.title = 'Sphagnum and Vascular Plant Decomposition under Increasing Nitrogen Additions: 2014-2015',
      data.table = data.table,
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
  
  # intellectual_rights.txt missing
  
  expect_error(
    make_eml(
      path = paste0(path_parent, '/templates_ir'),
      data.path = data.path,
      eml.path = eml.path,
      dataset.title = 'Sphagnum and Vascular Plant Decomposition under Increasing Nitrogen Additions: 2014-2015',
      data.table = data.table,
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
  
  # methods.txt missing
  
  expect_error(
    make_eml(
      path = paste0(path_parent, '/templates_me'),
      data.path = data.path,
      eml.path = eml.path,
      dataset.title = 'Sphagnum and Vascular Plant Decomposition under Increasing Nitrogen Additions: 2014-2015',
      data.table = data.table,
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
  
  
})

# Expect equal ----------------------------------------------------------------

testthat::test_that('Expect equal', {

  # class = 'eml'

  output <- make_eml(
    path = path,
    data.path = data.path,
    eml.path = eml.path,
    dataset.title = 'Sphagnum and Vascular Plant Decomposition under Increasing Nitrogen Additions: 2014-2015',
    data.table = data.table,
    data.table.description = c('Decomposition data', 'Nitrogen data'),
    temporal.coverage = c('2014-05-01', '2015-10-31'),
    geographic.description = 'Alberta, Canada, 100 km south of Fort McMurray, Canada',
    geographic.coordinates = c('55.895', '112.094','55.895', '112.094'),
    maintenance.description = 'completed',
    user.id = user.id,
    user.domain = user.domain,
    package.id = 'edi.141.1',
    return.obj = TRUE,
    write.file = FALSE
  )
  
  expect_equal(
    class(output) == 'eml',
    TRUE
  )
  
  # data.table.quote.character (is present)
  
  output <- make_eml(
    path = path,
    data.path = data.path,
    eml.path = eml.path,
    dataset.title = 'Sphagnum and Vascular Plant Decomposition under Increasing Nitrogen Additions: 2014-2015',
    data.table = data.table,
    data.table.description = c('Decomposition data', 'Nitrogen data'),
    data.table.quote.character = c("\'", "\'"),
    temporal.coverage = c('2014-05-01', '2015-10-31'),
    geographic.description = 'Alberta, Canada, 100 km south of Fort McMurray, Canada',
    geographic.coordinates = c('55.895', '112.094','55.895', '112.094'),
    maintenance.description = 'completed',
    user.id = user.id,
    user.domain = user.domain,
    package.id = 'edi.141.1',
    return.obj = TRUE,
    write.file = FALSE
  )
  
  expect_equal(
    class(output) == 'eml',
    TRUE
  )
  
  expect_equal(
    output@dataset@dataTable[[1]]@physical[[1]]@dataFormat@textFormat@simpleDelimited@quoteCharacter[[1]]@.Data,
    "'"
  )
  
  # other.entity
  
  output <- make_eml(
    path = path,
    data.path = data.path,
    eml.path = eml.path,
    dataset.title = 'Sphagnum and Vascular Plant Decomposition under Increasing Nitrogen Additions: 2014-2015',
    data.table = data.table,
    data.table.description = c('Decomposition data', 'Nitrogen data'),
    data.table.quote.character = c("\'", "\'"),
    other.entity = 'ancillary_data.zip',
    other.entity.description = 'Ancillary data',
    temporal.coverage = c('2014-05-01', '2015-10-31'),
    geographic.description = 'Alberta, Canada, 100 km south of Fort McMurray, Canada',
    geographic.coordinates = c('55.895', '112.094','55.895', '112.094'),
    maintenance.description = 'completed',
    user.id = user.id,
    user.domain = user.domain,
    package.id = 'edi.141.1',
    return.obj = TRUE,
    write.file = FALSE
  )
  
  expect_equal(
    class(output) == 'eml',
    TRUE
  )
  
  expect_equal(
    length(output@dataset@otherEntity),
    1
  )
  
  expect_equal(
    output@dataset@otherEntity[[1]]@physical[[1]]@objectName[[1]],
    'ancillary_data.zip'
  )
  
  # Using data.url
  # class = 'eml'
  
  output <- make_eml(
    path = path,
    data.path = data.path,
    eml.path = eml.path,
    dataset.title = 'Sphagnum and Vascular Plant Decomposition under Increasing Nitrogen Additions: 2014-2015',
    data.table = data.table,
    data.table.description = c('Decomposition data', 'Nitrogen data'),
    data.table.quote.character = c("\'", "\'"),
    other.entity = 'ancillary_data.zip',
    other.entity.description = 'Ancillary data',
    data.url = 'https://lter.limnology.wisc.edu/sites/default/files/data',
    temporal.coverage = c('2014-05-01', '2015-10-31'),
    geographic.description = 'Alberta, Canada, 100 km south of Fort McMurray, Canada',
    geographic.coordinates = c('55.895', '112.094','55.895', '112.094'),
    maintenance.description = 'completed',
    user.id = user.id,
    user.domain = user.domain,
    package.id = 'edi.141.1',
    return.obj = TRUE,
    write.file = FALSE
  )
  
  expect_equal(
    class(output) == 'eml',
    TRUE
  )
  
  # Using provenance
  # class = 'eml'
  
  if (all(class(edi_api) != 'try-error')){
    
    output <- make_eml(
      path = path,
      data.path = data.path,
      eml.path = eml.path,
      dataset.title = 'Sphagnum and Vascular Plant Decomposition under Increasing Nitrogen Additions: 2014-2015',
      data.table = data.table,
      data.table.description = c('Decomposition data', 'Nitrogen data'),
      data.table.quote.character = c("\'", "\'"),
      other.entity = 'ancillary_data.zip',
      other.entity.description = 'Ancillary data',
      data.url = 'https://lter.limnology.wisc.edu/sites/default/files/data',
      temporal.coverage = c('2014-05-01', '2015-10-31'),
      geographic.description = 'Alberta, Canada, 100 km south of Fort McMurray, Canada',
      geographic.coordinates = c('55.895', '112.094','55.895', '112.094'),
      maintenance.description = 'completed',
      user.id = user.id,
      user.domain = user.domain,
      package.id = 'edi.141.1',
      provenance = c('edi.100.1', 'edi.7.1'),
      return.obj = TRUE,
      write.file = FALSE
    )
    
  }

  expect_equal(
    class(output) == 'eml',
    TRUE
  )
  
  # Using bounding_boxes.txt
  # class = 'eml'
  
  output <- make_eml(
    path = paste0(path_parent, '/templates_bb'),
    data.path = data.path,
    eml.path = eml.path,
    dataset.title = 'Sphagnum and Vascular Plant Decomposition under Increasing Nitrogen Additions: 2014-2015',
    data.table = data.table,
    data.table.description = c('Decomposition data', 'Nitrogen data'),
    data.table.quote.character = c("\'", "\'"),
    temporal.coverage = c('2014-05-01', '2015-10-31'),
    geographic.description = 'Alberta, Canada, 100 km south of Fort McMurray, Canada',
    geographic.coordinates = c('55.895', '112.094','55.895', '112.094'),
    maintenance.description = 'completed',
    user.id = user.id,
    user.domain = user.domain,
    package.id = 'edi.141.1',
    return.obj = TRUE,
    write.file = FALSE
  )
  
  expect_equal(
    class(output) == 'eml',
    TRUE
  )
  
  # personnel.txt, project title is missing
  # class = 'eml'
  
  output <- make_eml(
    path = paste0(path_parent, '/templates_pt'),
    data.path = data.path,
    eml.path = eml.path,
    dataset.title = 'Sphagnum and Vascular Plant Decomposition under Increasing Nitrogen Additions: 2014-2015',
    data.table = data.table,
    data.table.description = c('Decomposition data', 'Nitrogen data'),
    data.table.quote.character = c("\'", "\'"),
    temporal.coverage = c('2014-05-01', '2015-10-31'),
    geographic.description = 'Alberta, Canada, 100 km south of Fort McMurray, Canada',
    geographic.coordinates = c('55.895', '112.094','55.895', '112.094'),
    maintenance.description = 'completed',
    user.id = user.id,
    user.domain = user.domain,
    package.id = 'edi.141.1',
    return.obj = TRUE,
    write.file = FALSE
  )
  
  expect_equal(
    class(output) == 'eml',
    TRUE
  )
  
  # personnel.txt, only project funding is present
  # class = 'eml'
  
  output <- make_eml(
    path = paste0(path_parent, '/templates_pf'),
    data.path = data.path,
    eml.path = eml.path,
    dataset.title = 'Sphagnum and Vascular Plant Decomposition under Increasing Nitrogen Additions: 2014-2015',
    data.table = data.table,
    data.table.description = c('Decomposition data', 'Nitrogen data'),
    data.table.quote.character = c("\'", "\'"),
    temporal.coverage = c('2014-05-01', '2015-10-31'),
    geographic.description = 'Alberta, Canada, 100 km south of Fort McMurray, Canada',
    geographic.coordinates = c('55.895', '112.094','55.895', '112.094'),
    maintenance.description = 'completed',
    user.id = user.id,
    user.domain = user.domain,
    package.id = 'edi.141.1',
    return.obj = TRUE,
    write.file = FALSE
  )
  
  expect_equal(
    class(output) == 'eml',
    TRUE
  )
  
  # personnel.txt (first project funding agency is present but all other fields blank)
  # class = 'eml'
  
  output <- make_eml(
    path = paste0(path_parent, '/templates_pfab'),
    data.path = data.path,
    eml.path = eml.path,
    dataset.title = 'Sphagnum and Vascular Plant Decomposition under Increasing Nitrogen Additions: 2014-2015',
    data.table = data.table,
    data.table.description = c('Decomposition data', 'Nitrogen data'),
    data.table.quote.character = c("\'", "\'"),
    temporal.coverage = c('2014-05-01', '2015-10-31'),
    geographic.description = 'Alberta, Canada, 100 km south of Fort McMurray, Canada',
    geographic.coordinates = c('55.895', '112.094','55.895', '112.094'),
    maintenance.description = 'completed',
    user.id = user.id,
    user.domain = user.domain,
    package.id = 'edi.141.1',
    return.obj = TRUE,
    write.file = FALSE
  )
  
  # personnel.txt (agency and number are listed in overall project)
  # class = 'eml'
  
  output <- make_eml(
    path = paste0(path_parent, '/templates_pfan'),
    data.path = data.path,
    eml.path = eml.path,
    dataset.title = 'Sphagnum and Vascular Plant Decomposition under Increasing Nitrogen Additions: 2014-2015',
    data.table = data.table,
    data.table.description = c('Decomposition data', 'Nitrogen data'),
    data.table.quote.character = c("\'", "\'"),
    temporal.coverage = c('2014-05-01', '2015-10-31'),
    geographic.description = 'Alberta, Canada, 100 km south of Fort McMurray, Canada',
    geographic.coordinates = c('55.895', '112.094','55.895', '112.094'),
    maintenance.description = 'completed',
    user.id = user.id,
    user.domain = user.domain,
    package.id = 'edi.141.1',
    return.obj = TRUE,
    write.file = FALSE
  )
  
  expect_equal(
    class(output) == 'eml',
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
    data.table.description = c('Decomposition data', 'Nitrogen data'),
    data.table.quote.character = c("\'", "\'"),
    temporal.coverage = c('2014-05-01', '2015-10-31'),
    geographic.description = 'Alberta, Canada, 100 km south of Fort McMurray, Canada',
    geographic.coordinates = c('55.895', '112.094','55.895', '112.094'),
    maintenance.description = 'completed',
    user.id = user.id,
    user.domain = user.domain,
    package.id = 'edi.141.1',
    return.obj = TRUE,
    write.file = FALSE
  )
  
  expect_equal(
    class(output) == 'eml',
    TRUE
  )
  
  # personnel.txt (funding title and number are present, agency is absent)
  # class = 'eml'
  
  output <- make_eml(
    path = paste0(path_parent, '/templates_pftn'),
    data.path = data.path,
    eml.path = eml.path,
    dataset.title = 'Sphagnum and Vascular Plant Decomposition under Increasing Nitrogen Additions: 2014-2015',
    data.table = data.table,
    data.table.description = c('Decomposition data', 'Nitrogen data'),
    data.table.quote.character = c("\'", "\'"),
    temporal.coverage = c('2014-05-01', '2015-10-31'),
    geographic.description = 'Alberta, Canada, 100 km south of Fort McMurray, Canada',
    geographic.coordinates = c('55.895', '112.094','55.895', '112.094'),
    maintenance.description = 'completed',
    user.id = user.id,
    user.domain = user.domain,
    package.id = 'edi.141.1',
    return.obj = TRUE,
    write.file = FALSE
  )
  
  expect_equal(
    class(output) == 'eml',
    TRUE
  )
  
  # custom_units.txt, file is missing
  # class = 'eml'
  
  output <- make_eml(
    path = paste0(path_parent, '/templates_pf'),
    data.path = data.path,
    eml.path = eml.path,
    dataset.title = 'Sphagnum and Vascular Plant Decomposition under Increasing Nitrogen Additions: 2014-2015',
    data.table = data.table,
    data.table.description = c('Decomposition data', 'Nitrogen data'),
    data.table.quote.character = c("\'", "\'"),
    temporal.coverage = c('2014-05-01', '2015-10-31'),
    geographic.description = 'Alberta, Canada, 100 km south of Fort McMurray, Canada',
    geographic.coordinates = c('55.895', '112.094','55.895', '112.094'),
    maintenance.description = 'completed',
    user.id = user.id,
    user.domain = user.domain,
    package.id = 'edi.141.1',
    return.obj = TRUE,
    write.file = FALSE
  )
  
  expect_equal(
    class(output) == 'eml',
    TRUE
  )
  
})

# Test usage with x (all templates and 2 data tables) -------------------------

testthat::test_that('Test usage with x (all templates and 2 data tables)', {
  
  # Arguments supplied to function in long form
  
  output <- make_eml(
    path = path,
    data.path = data.path,
    eml.path = eml.path,
    dataset.title = 'Sphagnum and Vascular Plant Decomposition under Increasing Nitrogen Additions: 2014-2015',
    data.table = data.table,
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
    write.file = FALSE,
    x = x_table
  )
  
  expect_equal(
    class(output) == 'eml',
    TRUE
  )
  
  # Arguments supplied to function via x
  
  output <- do.call(
    make_eml, 
    x_table$argument
  )
  
  expect_equal(
    class(output) == 'eml',
    TRUE
  )

})

# Test usage with x (all templates, 2 data tables, and 1 other entity) --------

testthat::test_that('Test usage with x (all templates, 2 data tables, and 1 other entity)', {
  
  # Arguments supplied to function in long form
  
  output <- make_eml(
    path = path,
    data.path = data.path,
    eml.path = eml.path,
    dataset.title = 'Sphagnum and Vascular Plant Decomposition under Increasing Nitrogen Additions: 2014-2015',
    data.table = data.table,
    data.table.description = c('Decomposition data', 'Nitrogen data'),
    data.table.quote.character = c("\"","\""),
    data.url = data.url,
    other.entity = 'ancillary_data.zip',
    other.entity.description = 'Ancillary data',
    temporal.coverage = c('2014-05-01', '2015-10-31'),
    geographic.description = 'Alberta, Canada, 100 km south of Fort McMurray, Canada',
    geographic.coordinates = c('55.895', '112.094','55.895', '112.094'),
    maintenance.description = 'completed',
    user.id = user.id,
    user.domain = user.domain,
    package.id = 'edi.141.1',
    return.obj = TRUE,
    write.file = FALSE,
    x = x_table_other
  )
  
  expect_equal(
    class(output) == 'eml',
    TRUE
  )
  
  # Arguments supplied to function via x
  
  output <- do.call(
    make_eml, 
    x_table_other$argument
  )
  
  expect_equal(
    class(output) == 'eml',
    TRUE
  )
  
})

# Test usage with x (all templates and 1 other entity) ------------------------

testthat::test_that('Test usage with x (all templates and 1 other entity)', {
  
  # Arguments supplied to function in long form
  
  output <- make_eml(
    path = path,
    data.path = data.path,
    eml.path = eml.path,
    dataset.title = 'Sphagnum and Vascular Plant Decomposition under Increasing Nitrogen Additions: 2014-2015',
    data.url = data.url,
    other.entity = 'ancillary_data.zip',
    other.entity.description = 'Ancillary data',
    temporal.coverage = c('2014-05-01', '2015-10-31'),
    geographic.description = 'Alberta, Canada, 100 km south of Fort McMurray, Canada',
    geographic.coordinates = c('55.895', '112.094','55.895', '112.094'),
    maintenance.description = 'completed',
    user.id = user.id,
    user.domain = user.domain,
    package.id = 'edi.141.1',
    return.obj = TRUE,
    write.file = FALSE,
    x = x_other
  )
  
  expect_equal(
    class(output) == 'eml',
    TRUE
  )
  
  # Arguments supplied to function via x
  
  output <- do.call(
    make_eml, 
    x_other$argument
  )
  
  expect_equal(
    class(output) == 'eml',
    TRUE
  )
  
})
