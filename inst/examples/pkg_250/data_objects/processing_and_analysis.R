# This script is an example workflow demonstrating the creation of EML metadata
# for a dataset composed of 2 .csv tables.

# Initialize workspace --------------------------------------------------------

rm(list = ls())

library(EMLassemblyline)
library(taxonomyCleanr)

# Define arguments ------------------------------------------------------------

path <- 'C:\\Users\\Colin\\Documents\\EDI\\data_sets\\edi_141\\templates'
data.path <- 'C:\\Users\\Colin\\Documents\\EDI\\data_sets\\edi_141\\data'
eml.path <- 'C:\\Users\\Colin\\Documents\\EDI\\data_sets\\edi_141\\eml'
data.files <- c('decomp.csv', 'nitrogen.csv')

# Import templates ------------------------------------------------------------

EMLassemblyline::import_templates(
  path = path,
  data.path = data.path,
  data.files = data.files,
  license = 'CC0'
)

# Define categorical variables ------------------------------------------------

EMLassemblyline::define_catvars(
  path = path,
  data.path = data.path
)

# Resolve taxa to ITIS and create taxonomicCoverage element -------------------

df <- taxonomyCleanr::resolve_sci_taxa(
  data.sources = '3',
  x = c(
    'Rhododendron groenlandicum',
    'Andromeda polifolia',
    'Chamaedaphne calyculata',
    'Smilacina trifolia',
    'Rubus chamaemorus',
    'Sphagnum fuscum'
  )
)

taxonomyCleanr::make_taxonomicCoverage(
  taxa.clean = df$taxa_clean,
  authority = df$authority,
  authority.id = df$authority_id,
  path = path
)

# Make EML --------------------------------------------------------------------

EMLassemblyline::make_eml(
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
  package.id = 'edi.141.1'
)


