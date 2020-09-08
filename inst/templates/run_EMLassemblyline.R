# This script executes an EMLassemblyline workflow.

# Initialize workspace --------------------------------------------------------

# Update EMLassemblyline and load
# NOTE: you only need to run this once. After you install it on your computer you just need to 
# load the libray using the code on line 10
remotes::install_github("EDIorg/EMLassemblyline")

library(EMLassemblyline)
library(clipr)

readClipboard() #For Windows ONLY. Copy the folder path from your file explorer and then use this function to paste it in R. Nice way to get the folder paths in an R-readable format to insert into "" on lines 18-20
read_clip() #For Windows, OS X, and Unix-like systems. Copy the folder path from your file explorer and then use this function to paste it in R. Nice way to get the folder paths in an R-readable format to insert into "" on lines 18-20

# Define folder paths for your metadata templates, data, and EML

path_templates <- ""
path_data <- ""
path_eml <- ""

# Create metadata templates ---------------------------------------------------

# Below is a list of boiler plate function calls for creating metadata templates.
# They are meant to be a reminder and save you a little time. Remove the 
# functions and arguments you don't need AND ... don't forget to read the docs! 
# E.g. ?template_core_metadata

# Create core templates (required for all data packages)
# Creates empty text files for abstract, additional_info, intellectual_rights, keywords, methods, and personnel

EMLassemblyline::template_core_metadata(
  path = path_templates,
  license = "", #license choose "CC0" or "CCBY" for public release 
  file.type = "") #file.type can be ".txt",".docx", or ".md" 

# Create table attributes template (required when data tables are present)
# Creates empty text files for attributes and custom_units for each data table

EMLassemblyline::template_table_attributes(
  path = path_templates,
  data.path = path_data,
  data.table = c("")) #List data table file names with file extension (e.g., "MyTable.csv") 

# Create categorical variables template (required when attributes templates
# contains variables with a "categorical" class)
# * Fill out your attribute text file and specify 'class' for all variables
# before running this function *

EMLassemblyline::template_categorical_variables(
  path = path_templates, 
  data.path = path_data)

# Create geographic coverage (required when more than one geographic location
# is to be reported in the metadata).
# NOTE: if you just want to report a bounding box (not individual site locations),
# skip this step. Boundary box information can be entered in EMLassemblyline::make_eml() on line 95

EMLassemblyline::template_geographic_coverage(
  path = path_templates, 
  data.path = path_data, 
  data.table = "", #Table name containing geographic coordinates
  lat.col = "", #Column name containing latitude coordinates in decimal degrees,
  lon.col = "", #Column name containing longitude coordinates in decimal degrees
  site.col = "") #Column name containing site names

# Create taxonomic coverage template (Not-required. Use this to report 
# taxonomic entities in the metadata)

remotes::install_github("EDIorg/taxonomyCleanr")
library(taxonomyCleanr)

taxonomyCleanr::view_taxa_authorities()

EMLassemblyline::template_taxonomic_coverage(
  path = path_templates, 
  data.path = path_data,
  taxa.table = "", #Table name containing taxa.col
  taxa.col = "", #Column name containing taxa names
  taxa.name.type = "", #Can be: 'scientific', 'common', or 'both'
  taxa.authority = 3)

# Make EML from metadata templates --------------------------------------------

# Once all your metadata templates are complete call this function to create 
# the EML.

EMLassemblyline::make_eml(
  path = path_templates,
  data.path = path_data,
  eml.path = path_eml, 
  dataset.title = "", 
  temporal.coverage = c("YYYY-MM-DD", "YYYY-MM-DD"), 
  geographic.description = "", 
  geographic.coordinates = c("N", "E", "S", "W"), 
  maintenance.description = "", #Can be 'ongoing' or 'complete'
  data.table = c(""), 
  data.table.name = c(""),
  data.table.description = c(""),
  other.entity = c(""), #Use other.entity for all non-data.table files (i.e 'ancillary_data.zip', .pdf, .jpg, etc)
  other.entity.name = c(""),
  other.entity.description = c(""),
  user.id = "",
  user.domain = "", 
  package.id = "") #Version control your package with the last number (i.e. the 1 in "edi.556.1" means that is the first version of package edi.556)
