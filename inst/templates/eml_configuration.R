
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

# Enter date range of dataset

begin_date <- "1998"

end_date <- "2016"

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

geographic_location <- trimws("Indiana, Kentucky, Maryland, New York, Pennsylvania, Virginia, West Virginia, United States.")

coordinate_north = 42.770

coordinate_east = -73.923

coordinate_south = 36.731

coordinate_west <- -88.155

# Funding information

funding_title = trimws(c("CRB: Effects of harvesting and white tailed deer browsing on demography and population viability of American ginseng",
                         "LTREB: Population growth and viability of American Ginseng (Panax quinquefolius L.)",
                         "LTREB renewal: Population growth and viability of American Ginseng (Panaxquinquefolius L.)"))

funding_grants = trimws("National Science Foundation DEB-0212411, DEB-0613611, DEB-11187021")

# Set root level info

data_package_id <- trimws("edi.9.2")

root_system <- "edi"

# Set access

author_system <- "edi"

allow_principals <- trimws(c("uid=csmith,o=LTER,dc=ecoinformatics,dc=org",
                             "public"))

allow_permissions <- trimws(c("all",
                              "read")) # order follows allow_principals

access_order <- trimws("allowFirst")

access_scope <- trimws("document")

