
rm(list = ls())
library(EMLassemblyline)
library(taxonomyCleanr)

EMLassemblyline::import_templates(
  path = '/Users/csmith/Documents/EDI/datasets/edi_281/metadata_templates',
  data.path = '/Users/csmith/Documents/EDI/datasets/edi_281/data',
  data.files = 'Sphagnum_and_vascular_plant_decomposition_data.csv',
  license = 'CC0'
)

EMLassemblyline::define_catvars(
  path = 'C:\\Users\\Colin\\Downloads\\datasets\\datasets\\edi_281\\metadata_templates',
  data.path = 'C:\\Users\\Colin\\Downloads\\datasets\\datasets\\edi_281\\data'
)

d <- taxonomyCleanr::resolve_sci_taxa(data.sources = '3',
                                      x = c('Rhododendron groenlandicum',
                                            'Andromeda polifolia',
                                            'Chamaedaphne calyculata',
                                            'Smilacina trifolia',
                                            'Rubus chamaemorus',
                                            'Sphagnum fuscum'))
taxonomyCleanr::make_taxonomicCoverage(
  taxa.clean = d$taxa_clean,
  authority = d$authority,
  authority.id = d$authority_id,
  path = 'C:\\Users\\Colin\\Downloads\\datasets\\datasets\\edi_281\\metadata_templates')

EMLassemblyline::make_eml(
  path = 'C:\\Users\\Colin\\Downloads\\datasets\\datasets\\edi_281\\metadata_templates',
  data.path = 'C:\\Users\\Colin\\Downloads\\datasets\\datasets\\edi_281\\data',
  eml.path = 'C:\\Users\\Colin\\Downloads\\datasets\\datasets\\edi_281\\eml',
  dataset.title = 'Sphagnum and Vascular Plant Decomposition under Increasing Nitrogen Additions: 2014-2015',
  data.files = 'Sphagnum_and_vascular_plant_decomposition_data.csv',
  data.files.description = 'Sphagnum and Vascular Plant Decomposition Data',
  temporal.coverage = c('2014-05-01', '2015-10-31'),
  geographic.description = 'Alberta, Canada, 100 km south of Fort McMurray, Canada',
  geographic.coordinates = c('55.895', '112.094','55.895', '112.094'),
  maintenance.description = 'completed',
  user.id = 'csmith',
  affiliation = 'LTER',
  package.id = 'edi.260.1'
)


