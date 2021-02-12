
# Initialize workspace --------------------------------------------------------

library(EMLassemblyline)

path <- "C:\\Users\\Colin\\Documents\\EDI\\data_sets\\pkg_260\\metadata_templates"
data_path <- "C:\\Users\\Colin\\Documents\\EDI\\data_sets\\pkg_260\\data_objects"
eml_path <- tempdir()


# Make EML --------------------------------------------------------------------

eml <- make_eml(
  path = path,
  data.path = data_path,
  eml.path = eml_path, 
  dataset.title = "The title of this information rich and wonderful dataset",
  temporal.coverage = c("2020-01-01", "2020-01-05"),
  maintenance.description = "Complete", 
  data.table = c(
    "decomp.csv", 
    "nitrogen.csv"),
  data.table.name = c("Decomposition data", "Nitrogen data"),
  data.table.description = c("Decomposition rates measured at a fortnightly frequency in 3 streams",
                             "Nitrogen concentrations corresponding to decomposition rates"),
  data.table.quote.character = c('"', '"'),
  other.entity = c("ancillary_data.zip", "processing_and_analysis.R"),
  other.entity.name = c("Ancillary data", "Processing and analysis scripts"),
  other.entity.description = c("Ancillary data including maps and experimental arrangement",
                               "Processing and analysis scripts used in the paper"),
  user.id = "myid",
  user.domain = "EDI", 
  package.id = "edi.260.1",
  write.file = F,
  return.obj = T)