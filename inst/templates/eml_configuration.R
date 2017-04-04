
# Set parameters for data tables ----------------------------------------------

# Data table names

table_names <- trimws(c("ltreb_ginseng_populations.csv",
                        "ltreb_ginseng_coordinates.csv"))

# Data table descriptions (order must follow the above listing of tables).

data_table_descriptions <-trimws(c("Census information on wild populations of ginseng",
                                   "Bounding coordinates for each ginseng population"))

# New attribute names?
# If yes then entries must must follow the listed order of table_names.
# If no, then leave the list empty.

new_attribute_names <- list(trimws(c()),
                           trimws(c()))

# URLs of data tables (order must follow the above listing of tables)

data_table_urls <- trimws(c("https://lter.limnology.wisc.edu/sites/default/files/data/ltreb_ginseng_populations.csv",
                            "https://lter.limnology.wisc.edu/sites/default/files/data/ltreb_ginseng_coordinates.csv"))

# Dataset maintenance
# 
# A brief description of the maintenance of this data set. This includes 
# information about the frequency of update and whether data collection 
# is ongoing.

maintenance_description <- "ongoing"

# Enter date range of dataset
#
# Format must be: "YYYY-MM-DD"

begin_date <- trimws("1998-01-01")

end_date <- trimws("2016-01-01")

# Define table formatting (order must follow the above listing of tables)

num_header_lines <- trimws(c("1",
                             "1"))

record_delimeter <- trimws(c("\\r\\n",
                             "\\r\\n"))

attribute_orientation <- trimws(c("column",
                                  "column"))

field_delimeter <- trimws(c(",",
                            ","))


# Set parameters for spatial vectors ------------------------------------------

# Name of zipped vector folder(s) names

spatial_vector_names <- trimws(c())

# Description(s) of Zipped vector folder contents (order must follow the above listing of zipped vector folder(s) names).

spatial_vector_description <-trimws(c())

# URL(s) of Zipped vector folder(s) (order must follow the above listing of zipped vector folder(s)).

spatial_vector_urls <- trimws(c())

# Define spatial vector formatting (order must follow the above listing of zipped vector folder(s) names).

num_header_lines_sv <- trimws(c())

record_delimeter_sv <- trimws(c())

attribute_orientation_sv <- trimws(c())

field_delimeter_sv <- trimws(c())

# Enter size of .zip files IN BYTES (order must follow the above listing of zipped vector folder(s) names).

spatial_vector_sizes <- trimws(c())

# Geometry of spatial data (order must follow the above listing of zipped vector folder(s) names).

spatial_vector_geometry <- trimws(c())


# Set parameters for code/scripts that process this dataset -------------------

# Name of script files

code_names <- trimws(c())

# Code file descriptions (order must follow the above listing of code names)

code_description <- trimws(c())

# Define code formatting (order must follow the above listing of code names).

num_header_lines_code <- trimws(c())

record_delimeter_code <- trimws(c())

attribute_orientation_code <- trimws(c())

field_delimeter_code <- trimws(c())

quote_character <- trimws(c())

# URL(s) of scripts/code (order must follow the above listing of code names).

code_urls <- trimws(c())

# Set entity type, this is a terse description of the code type (e.g. "R code")

entity_type_code <- trimws(c())


# Set additional parameters -------------------------------------------------

# Dataset title

dataset_title <- trimws("Long Term Research in Environmental Biology: Demographic census data for thirty natural populations of American Ginseng: 1998-2016")

# Dataset keywords

keywords <- trimws(c("Long Term Research in Environmental Biology", "LTREB", "National Science Foundation", "NSF",
                     "ginseng", "Panax quinquefolius", "Araliaceae", 
                     "plant demography", "plant ecology", "plants", "plant communities", "plant growth", "plant seeds",
                     "harvest", "deer browse", "disease", "global change",
                     "leaf area",
                     "matrix models", "census", "surveys"))

# Geographic information
#
# Enter these as character strings of decimal degrees.
#
# If more than one set of bounding coordinates exists, enter these into the
# spread sheet "datasetname_spatial_bounds.xlsx".
#
# If the there are many geographical points that describe sampling sites (e.g.
# many lakes spread over a continent) repeat the lattidudinal value in both
# the North and South fields, and likewise repeat the longitudinal value in
# both the East and West fields.

geographic_location <- NULL # Set this to NULL if more than one set of spatial coordinates exists

coordinate_north <- NULL # Set this to NULL if more than one set of spatial coordinates exists

coordinate_east <- NULL # Set this to NULL if more than one set of spatial coordinates exists

coordinate_south <- NULL # Set this to NULL if more than one set of spatial coordinates exists

coordinate_west <- NULL # Set this to NULL if more than one set of spatial coordinates exists

# Funding information

funding_title = trimws(c("CRB: Effects of harvesting and white tailed deer browsing on demography and population viability of American ginseng",
                         "LTREB: Population growth and viability of American Ginseng (Panax quinquefolius L.)",
                         "LTREB renewal: Population growth and viability of American Ginseng (Panaxquinquefolius L.)"))

funding_grants = trimws("National Science Foundation DEB-0212411, DEB-0613611, DEB-11187021")

# Set root level info

data_package_id <- trimws("edi.9.2")

root_system <- "https://pasta.lternet.edu"

schema_location <- "eml://ecoinformatics.org/eml-2.1.1  http://nis.lternet.edu/schemas/EML/eml-2.1.1/eml.xsd"

# Set access

author_system <- "edi"

allow_principals <- trimws(c("uid=csmith,o=LTER,dc=ecoinformatics,dc=org",
                             "public"))

allow_permissions <- trimws(c("all",
                              "read")) # order follows allow_principals

access_order <- trimws("allowFirst")

access_scope <- trimws("document")

