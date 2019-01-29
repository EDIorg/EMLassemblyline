context('Make EML')
library(EMLassemblyline)
library(EML)

# Parameterize ----------------------------------------------------------------

path <- system.file(
  '/examples/templates/abstract.txt', 
  package = 'EMLassemblyline'
)
path <- substr(path, 1, nchar(path) - 23)

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
user.id <- 'csmith'
affiliation <- 'LTER'
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
  
})

# Expect equal ----------------------------------------------------------------

testthat::test_that('Expect equal', {

  # class = 'eml'

  expect_equal(
    class(
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
        user.id = 'csmith',
        affiliation = 'LTER',
        package.id = 'edi.141.1',
        return.obj = TRUE,
        write.file = FALSE
      )
    ) == 'eml',
    TRUE
  )
  
})
