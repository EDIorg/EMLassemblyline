context('Make EML')
library(EMLassemblyline)
library(EML)

# Parameterize ----------------------------------------------------------------

path <- system.file(
  '/examples/templates/abstract.txt', 
  package = 'EMLassemblyline'
)
path <- substr(path, 1, nchar(path) - 23)
path_parent <- path

# Possible arguments to make_eml.R

data.path <- paste0(path, '/data')
eml.path <- paste0(path, '/eml')
path <- paste0(path, '/templates')
dataset.title <- 'Sphagnum and Vascular Plant Decomposition under Increasing Nitrogen Additions: 2014-2015'
data.files <- c('decomp.csv', 'nitrogen.csv')
data.files.description <- c('Decomposition data', 'Nitrogen data')
temporal.coverage <- c('2014-05-01', '2015-10-31')
geographic.description <- 'Alberta, Canada, 100 km south of Fort McMurray, Canada'
geographic.coordinates <- c('55.895', '112.094','55.895', '112.094')
maintenance.description <- 'completed'
user.id <- c('csmith', 'someusername')
affiliation <- c('LTER', 'EDI')
package.id <- 'edi.141.1'
data.files.quote.character <- c("\\'", "\\'")
data.files.url <- 'https://lter.limnology.wisc.edu/sites/default/files/data/edi_141/data'
zip.dir <- 'protocols.zip'
zip.dir.description <- 'Collection of sampling and analytical protocols'
provenance <- 'edi.100.1'

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
      affiliation = affiliation
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
      affiliation = affiliation
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
      affiliation = affiliation
    )
  )
  
  # temporal.coverage missing start date
  
  expect_error(
    make_eml(
      path = path,
      data.path = data.path,
      eml.path = eml.path,
      dataset.title = 'Sphagnum and Vascular Plant Decomposition under Increasing Nitrogen Additions: 2014-2015',
      data.files = data.files,
      data.files.description = c('Decomposition data', 'Nitrogen data'),
      temporal.coverage = '2015-10-31',
      geographic.description = 'Alberta, Canada, 100 km south of Fort McMurray, Canada',
      geographic.coordinates = c('55.895', '112.094','55.895', '112.094'),
      maintenance.description = 'completed',
      user.id = user.id,
      affiliation = affiliation,
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
      affiliation = affiliation
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
      affiliation = affiliation
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
      affiliation = affiliation
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
      affiliation = affiliation
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
      affiliation = affiliation
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
      affiliation = 'LTER'
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
      affiliation = c('LTER', 'EDI')
    )
  )
  
  # affiliation
  
  expect_error(
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
  
  # affiliation (unsupported affiliation)
  
  expect_error(
    make_eml(
      path = path,
      dataset.title = dataset.title,
      temporal.coverage = temporal.coverage,
      geographic.description = geographic.description,
      geographic.coordinates = geographic.coordinates,
      maintenance.description = maintenance.description,
      user.id = 'csmith',
      affiliation = 'EDEYE'
    )
  )
  
  # data.files.description
  
  expect_error(
    make_eml(
      path = path,
      data.path = data.path,
      eml.path = eml.path,
      dataset.title = 'Sphagnum and Vascular Plant Decomposition under Increasing Nitrogen Additions: 2014-2015',
      data.files = data.files,
      temporal.coverage = c('2014-05-01', '2015-10-31'),
      geographic.description = 'Alberta, Canada, 100 km south of Fort McMurray, Canada',
      geographic.coordinates = c('55.895', '112.094','55.895', '112.094'),
      maintenance.description = 'completed',
      user.id = user.id,
      affiliation = affiliation,
      package.id = 'edi.141.1',
      write.file = FALSE
    )
  )
  
  # Malformed package.id
  
  expect_error(
    make_eml(
      path = path,
      data.path = data.path,
      eml.path = eml.path,
      dataset.title = 'Sphagnum and Vascular Plant Decomposition under Increasing Nitrogen Additions: 2014-2015',
      data.files = data.files,
      data.files.description = c('Decomposition data', 'Nitrogen data'),
      temporal.coverage = c('2014-05-01', '2015-10-31'),
      geographic.description = 'Alberta, Canada, 100 km south of Fort McMurray, Canada',
      geographic.coordinates = c('55.895', '112.094','55.895', '112.094'),
      maintenance.description = 'completed',
      user.id = user.id,
      affiliation = affiliation,
      package.id = 'edi.141',
      write.file = FALSE
    )
  )
  
  # Missing one of two data.files.description
  
  expect_error(
    make_eml(
      path = path,
      data.path = data.path,
      eml.path = eml.path,
      dataset.title = 'Sphagnum and Vascular Plant Decomposition under Increasing Nitrogen Additions: 2014-2015',
      data.files = data.files,
      data.files.description = 'Decomposition data',
      temporal.coverage = c('2014-05-01', '2015-10-31'),
      geographic.description = 'Alberta, Canada, 100 km south of Fort McMurray, Canada',
      geographic.coordinates = c('55.895', '112.094','55.895', '112.094'),
      maintenance.description = 'completed',
      user.id = user.id,
      affiliation = affiliation,
      package.id = 'edi.141.1',
      write.file = FALSE
    )
  )
  
  # Missing one of two data.files.quote.character
  
  expect_error(
    make_eml(
      path = path,
      data.path = data.path,
      eml.path = eml.path,
      dataset.title = 'Sphagnum and Vascular Plant Decomposition under Increasing Nitrogen Additions: 2014-2015',
      data.files = data.files,
      data.files.description = c('Decomposition data', 'Nitrogen data'),
      data.files.quote.character = "\'",
      temporal.coverage = c('2014-05-01', '2015-10-31'),
      geographic.description = 'Alberta, Canada, 100 km south of Fort McMurray, Canada',
      geographic.coordinates = c('55.895', '112.094','55.895', '112.094'),
      maintenance.description = 'completed',
      user.id = user.id,
      affiliation = affiliation,
      package.id = 'edi.141.1',
      write.file = FALSE
    )
  )
  
  # zip.dir.description
  
  expect_error(
    make_eml(
      path = path,
      data.path = data.path,
      eml.path = eml.path,
      dataset.title = 'Sphagnum and Vascular Plant Decomposition under Increasing Nitrogen Additions: 2014-2015',
      data.files = data.files,
      data.files.description = c('Decomposition data', 'Nitrogen data'),
      zip.dir = 'ancillary_data.zip',
      temporal.coverage = c('2014-05-01', '2015-10-31'),
      geographic.description = 'Alberta, Canada, 100 km south of Fort McMurray, Canada',
      geographic.coordinates = c('55.895', '112.094','55.895', '112.094'),
      maintenance.description = 'completed',
      user.id = user.id,
      affiliation = affiliation,
      package.id = 'edi.141.1',
      write.file = FALSE
    )
  )
  
  # zip.dir
  
  expect_error(
    make_eml(
      path = path,
      data.path = data.path,
      eml.path = eml.path,
      dataset.title = 'Sphagnum and Vascular Plant Decomposition under Increasing Nitrogen Additions: 2014-2015',
      data.files = data.files,
      data.files.description = c('Decomposition data', 'Nitrogen data'),
      zip.dir.description = 'Ancillary data',
      temporal.coverage = c('2014-05-01', '2015-10-31'),
      geographic.description = 'Alberta, Canada, 100 km south of Fort McMurray, Canada',
      geographic.coordinates = c('55.895', '112.094','55.895', '112.094'),
      maintenance.description = 'completed',
      user.id = user.id,
      affiliation = affiliation,
      package.id = 'edi.141.1',
      write.file = FALSE
    )
  )
  
  # zip.dir and zip.dir.description mismatch
  
  expect_error(
    make_eml(
      path = path,
      data.path = data.path,
      eml.path = eml.path,
      dataset.title = 'Sphagnum and Vascular Plant Decomposition under Increasing Nitrogen Additions: 2014-2015',
      data.files = data.files,
      data.files.description = c('Decomposition data', 'Nitrogen data'),
      zip.dir = 'ancillary_data.zip',
      zip.dir.description = c('Ancillary data', 'Additional ancillary data'),
      temporal.coverage = c('2014-05-01', '2015-10-31'),
      geographic.description = 'Alberta, Canada, 100 km south of Fort McMurray, Canada',
      geographic.coordinates = c('55.895', '112.094','55.895', '112.094'),
      maintenance.description = 'completed',
      user.id = user.id,
      affiliation = affiliation,
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
      data.files = data.files,
      data.files.description = c('Decomposition data', 'Nitrogen data'),
      temporal.coverage = c('2014-05-01', '2015-10-31'),
      geographic.description = 'Alberta, Canada, 100 km south of Fort McMurray, Canada',
      geographic.coordinates = c('55.895', '112.094','55.895', '112.094'),
      maintenance.description = 'completed',
      user.id = user.id,
      affiliation = affiliation,
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
      data.files = data.files,
      data.files.description = c('Decomposition data', 'Nitrogen data'),
      temporal.coverage = c('2014-05-01', '2015-10-31'),
      geographic.description = 'Alberta, Canada, 100 km south of Fort McMurray, Canada',
      geographic.coordinates = c('55.895', '112.094','55.895', '112.094'),
      maintenance.description = 'completed',
      user.id = user.id,
      affiliation = affiliation,
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
      data.files = data.files,
      data.files.description = c('Decomposition data', 'Nitrogen data'),
      temporal.coverage = c('2014-05-01', '2015-10-31'),
      geographic.description = 'Alberta, Canada, 100 km south of Fort McMurray, Canada',
      geographic.coordinates = c('55.895', '112.094','55.895', '112.094'),
      maintenance.description = 'completed',
      user.id = user.id,
      affiliation = affiliation,
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
      data.files = data.files,
      data.files.description = c('Decomposition data', 'Nitrogen data'),
      temporal.coverage = c('2014-05-01', '2015-10-31'),
      geographic.description = 'Alberta, Canada, 100 km south of Fort McMurray, Canada',
      geographic.coordinates = c('55.895', '112.094','55.895', '112.094'),
      maintenance.description = 'completed',
      user.id = user.id,
      affiliation = affiliation,
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
      data.files = data.files,
      data.files.description = c('Decomposition data', 'Nitrogen data'),
      temporal.coverage = c('2014-05-01', '2015-10-31'),
      geographic.description = 'Alberta, Canada, 100 km south of Fort McMurray, Canada',
      geographic.coordinates = c('55.895', '112.094','55.895', '112.094'),
      maintenance.description = 'completed',
      user.id = user.id,
      affiliation = affiliation,
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
      data.files = data.files,
      data.files.description = c('Decomposition data', 'Nitrogen data'),
      temporal.coverage = c('2014-05-01', '2015-10-31'),
      geographic.description = 'Alberta, Canada, 100 km south of Fort McMurray, Canada',
      geographic.coordinates = c('55.895', '112.094','55.895', '112.094'),
      maintenance.description = 'completed',
      user.id = user.id,
      affiliation = affiliation,
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
      data.files = data.files,
      data.files.description = c('Decomposition data', 'Nitrogen data'),
      temporal.coverage = c('2014-05-01', '2015-10-31'),
      geographic.description = 'Alberta, Canada, 100 km south of Fort McMurray, Canada',
      geographic.coordinates = c('55.895', '112.094','55.895', '112.094'),
      maintenance.description = 'completed',
      user.id = user.id,
      affiliation = affiliation,
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
      data.files = data.files,
      data.files.description = c('Decomposition data', 'Nitrogen data'),
      temporal.coverage = c('2014-05-01', '2015-10-31'),
      geographic.description = 'Alberta, Canada, 100 km south of Fort McMurray, Canada',
      geographic.coordinates = c('55.895', '112.094','55.895', '112.094'),
      maintenance.description = 'completed',
      user.id = user.id,
      affiliation = affiliation,
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
      data.files = data.files,
      data.files.description = c('Decomposition data', 'Nitrogen data'),
      temporal.coverage = c('2014-05-01', '2015-10-31'),
      geographic.description = 'Alberta, Canada, 100 km south of Fort McMurray, Canada',
      geographic.coordinates = c('55.895', '112.094','55.895', '112.094'),
      maintenance.description = 'completed',
      user.id = user.id,
      affiliation = affiliation,
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
      data.files = data.files,
      data.files.description = c('Decomposition data', 'Nitrogen data'),
      temporal.coverage = c('2014-05-01', '2015-10-31'),
      geographic.description = 'Alberta, Canada, 100 km south of Fort McMurray, Canada',
      geographic.coordinates = c('55.895', '112.094','55.895', '112.094'),
      maintenance.description = 'completed',
      user.id = user.id,
      affiliation = affiliation,
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
      data.files = data.files,
      data.files.description = c('Decomposition data', 'Nitrogen data'),
      temporal.coverage = c('2014-05-01', '2015-10-31'),
      geographic.description = 'Alberta, Canada, 100 km south of Fort McMurray, Canada',
      geographic.coordinates = c('55.895', '112.094','55.895', '112.094'),
      maintenance.description = 'completed',
      user.id = user.id,
      affiliation = affiliation,
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
      data.files = data.files,
      data.files.description = c('Decomposition data', 'Nitrogen data'),
      temporal.coverage = c('2014-05-01', '2015-10-31'),
      geographic.description = 'Alberta, Canada, 100 km south of Fort McMurray, Canada',
      geographic.coordinates = c('55.895', '112.094','55.895', '112.094'),
      maintenance.description = 'completed',
      user.id = user.id,
      affiliation = affiliation,
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
      data.files = data.files,
      data.files.description = c('Decomposition data', 'Nitrogen data'),
      temporal.coverage = c('2014-05-01', '2015-10-31'),
      geographic.description = 'Alberta, Canada, 100 km south of Fort McMurray, Canada',
      geographic.coordinates = c('55.895', '112.094','55.895', '112.094'),
      maintenance.description = 'completed',
      user.id = user.id,
      affiliation = affiliation,
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
    data.files = data.files,
    data.files.description = c('Decomposition data', 'Nitrogen data'),
    temporal.coverage = c('2014-05-01', '2015-10-31'),
    geographic.description = 'Alberta, Canada, 100 km south of Fort McMurray, Canada',
    geographic.coordinates = c('55.895', '112.094','55.895', '112.094'),
    maintenance.description = 'completed',
    user.id = user.id,
    affiliation = affiliation,
    package.id = 'edi.141.1',
    return.obj = TRUE,
    write.file = FALSE
  )
  
  expect_equal(
    class(output) == 'eml',
    TRUE
  )
  
  # data.files.quote.character (is present)
  
  output <- make_eml(
    path = path,
    data.path = data.path,
    eml.path = eml.path,
    dataset.title = 'Sphagnum and Vascular Plant Decomposition under Increasing Nitrogen Additions: 2014-2015',
    data.files = data.files,
    data.files.description = c('Decomposition data', 'Nitrogen data'),
    data.files.quote.character = c("\'", "\'"),
    temporal.coverage = c('2014-05-01', '2015-10-31'),
    geographic.description = 'Alberta, Canada, 100 km south of Fort McMurray, Canada',
    geographic.coordinates = c('55.895', '112.094','55.895', '112.094'),
    maintenance.description = 'completed',
    user.id = user.id,
    affiliation = affiliation,
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
  
  # zip.dir
  
  output <- make_eml(
    path = path,
    data.path = data.path,
    eml.path = eml.path,
    dataset.title = 'Sphagnum and Vascular Plant Decomposition under Increasing Nitrogen Additions: 2014-2015',
    data.files = data.files,
    data.files.description = c('Decomposition data', 'Nitrogen data'),
    data.files.quote.character = c("\'", "\'"),
    zip.dir = 'ancillary_data.zip',
    zip.dir.description = 'Ancillary data',
    temporal.coverage = c('2014-05-01', '2015-10-31'),
    geographic.description = 'Alberta, Canada, 100 km south of Fort McMurray, Canada',
    geographic.coordinates = c('55.895', '112.094','55.895', '112.094'),
    maintenance.description = 'completed',
    user.id = user.id,
    affiliation = affiliation,
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
  
  # Using data.files.url
  # class = 'eml'
  
  output <- make_eml(
    path = path,
    data.path = data.path,
    eml.path = eml.path,
    dataset.title = 'Sphagnum and Vascular Plant Decomposition under Increasing Nitrogen Additions: 2014-2015',
    data.files = data.files,
    data.files.description = c('Decomposition data', 'Nitrogen data'),
    data.files.quote.character = c("\'", "\'"),
    zip.dir = 'ancillary_data.zip',
    zip.dir.description = 'Ancillary data',
    data.files.url = 'https://lter.limnology.wisc.edu/sites/default/files/data',
    temporal.coverage = c('2014-05-01', '2015-10-31'),
    geographic.description = 'Alberta, Canada, 100 km south of Fort McMurray, Canada',
    geographic.coordinates = c('55.895', '112.094','55.895', '112.094'),
    maintenance.description = 'completed',
    user.id = user.id,
    affiliation = affiliation,
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
  
  output <- make_eml(
    path = path,
    data.path = data.path,
    eml.path = eml.path,
    dataset.title = 'Sphagnum and Vascular Plant Decomposition under Increasing Nitrogen Additions: 2014-2015',
    data.files = data.files,
    data.files.description = c('Decomposition data', 'Nitrogen data'),
    data.files.quote.character = c("\'", "\'"),
    zip.dir = 'ancillary_data.zip',
    zip.dir.description = 'Ancillary data',
    data.files.url = 'https://lter.limnology.wisc.edu/sites/default/files/data',
    temporal.coverage = c('2014-05-01', '2015-10-31'),
    geographic.description = 'Alberta, Canada, 100 km south of Fort McMurray, Canada',
    geographic.coordinates = c('55.895', '112.094','55.895', '112.094'),
    maintenance.description = 'completed',
    user.id = user.id,
    affiliation = affiliation,
    package.id = 'edi.141.1',
    provenance = c('edi.100.1', 'edi.7.1'),
    return.obj = TRUE,
    write.file = FALSE
  )
  
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
    data.files = data.files,
    data.files.description = c('Decomposition data', 'Nitrogen data'),
    data.files.quote.character = c("\'", "\'"),
    temporal.coverage = c('2014-05-01', '2015-10-31'),
    geographic.description = 'Alberta, Canada, 100 km south of Fort McMurray, Canada',
    geographic.coordinates = c('55.895', '112.094','55.895', '112.094'),
    maintenance.description = 'completed',
    user.id = user.id,
    affiliation = affiliation,
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
    data.files = data.files,
    data.files.description = c('Decomposition data', 'Nitrogen data'),
    data.files.quote.character = c("\'", "\'"),
    temporal.coverage = c('2014-05-01', '2015-10-31'),
    geographic.description = 'Alberta, Canada, 100 km south of Fort McMurray, Canada',
    geographic.coordinates = c('55.895', '112.094','55.895', '112.094'),
    maintenance.description = 'completed',
    user.id = user.id,
    affiliation = affiliation,
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
    data.files = data.files,
    data.files.description = c('Decomposition data', 'Nitrogen data'),
    data.files.quote.character = c("\'", "\'"),
    temporal.coverage = c('2014-05-01', '2015-10-31'),
    geographic.description = 'Alberta, Canada, 100 km south of Fort McMurray, Canada',
    geographic.coordinates = c('55.895', '112.094','55.895', '112.094'),
    maintenance.description = 'completed',
    user.id = user.id,
    affiliation = affiliation,
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
    data.files = data.files,
    data.files.description = c('Decomposition data', 'Nitrogen data'),
    data.files.quote.character = c("\'", "\'"),
    temporal.coverage = c('2014-05-01', '2015-10-31'),
    geographic.description = 'Alberta, Canada, 100 km south of Fort McMurray, Canada',
    geographic.coordinates = c('55.895', '112.094','55.895', '112.094'),
    maintenance.description = 'completed',
    user.id = user.id,
    affiliation = affiliation,
    package.id = 'edi.141.1',
    return.obj = TRUE,
    write.file = FALSE
  )
  
  expect_equal(
    class(output) == 'eml',
    TRUE
  )
  
  
})
