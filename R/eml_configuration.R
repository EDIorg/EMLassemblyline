#' EML configuration
#'
#' @description  A set of user specified parameters for functions of the EMLtools package.
#'
#' @usage eml_configuration(path)
#'
#' @param path A path to the working directory containing files required by the EMLtools
#' package. Run the function run_guide() for instructions on how to populate this working
#' directory with the requisite files.
#'
#' @return A configuration file in the specified working directory containing parameters
#' for use of functions of the EMLtools package.
#'
#' @seealso \code{\link{run_guide}} for guidance on setting up the working directory
#' @seealso \code{\link{write_attributes}} for writing data attributes
#' @seealso \code{\link{write_factors}} for writing data factors
#' @seealso \code{\link{create_eml}} for creating EML
#'

eml_configuration <- function(){


  # Set parameters for data tables ----------------------------------------------

  # Data table names

  table_names <- trimws(c("gleon_chloride_concentrations.csv",
                          "gleon_chloride_lake_characteristics.csv"))

  # Data table descriptions (order must follow the above listing of tables).

  data_table_descriptions <-trimws(c("Long term chloride concentration data from 529 lakes and reservoirs around North America and Europe.",
                                     "Lake characteristics, including climate, road density, and impervious surface data."))

  # New attribute names?
  # If yes then entries must must follow the listed order of table_names.
  # If no, then leave the list empty.

  new_attribute_names <- list(trimws(c()),
                             trimws(c()))

  # URLs of data tables (order must follow the above listing of tables)

  data_table_urls <- trimws(c("https://lter.limnology.wisc.edu/sites/default/files/data/gleon_chloride_concentrations.csv",
                              "https://lter.limnology.wisc.edu/sites/default/files/data/gleon_chloride_lake_characteristics.csv"))

  # Enter date range of dataset

  begin_date <- "1940-01-31"

  end_date <- "2016-07-06"

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

  spatial_vector_names <- trimws(c("gleon_chloride_shape_files.zip"))

  # Description(s) of Zipped vector folder contents (order must follow the above listing of zipped vector folder(s) names).

  spatial_vector_description <-trimws(c("Shapefiles that geographically delimited the perimeter of the lakes were required to quantify surrounding land use patterns. Shapefiles were acquired via five methods (see methods). Attributes inherited from these methods were preserved in the shape files. To find definitions for these attributes, please trace back to the methods creator."))

  # URL(s) of Zipped vector folder(s) (order must follow the above listing of zipped vector folder(s)).

  spatial_vector_urls <- trimws(c("https://lter.limnology.wisc.edu/sites/default/files/data/gleon_chloride_lake_shape_files.zip"))

  # Define spatial vector formatting (order must follow the above listing of zipped vector folder(s) names).

  num_header_lines_sv <- trimws(c("1"))

  record_delimeter_sv <- trimws(c("\\r\\n"))

  attribute_orientation_sv <- trimws(c("column"))

  field_delimeter_sv <- trimws(c(","))

  # Enter size of .zip files IN BYTES (order must follow the above listing of zipped vector folder(s) names).

  spatial_vector_sizes <- trimws(c("19524903"))

  # Geometry of spatial data (order must follow the above listing of zipped vector folder(s) names).

  spatial_vector_geometry <- trimws(c("Polygon"))


  # Set parameters for data set -------------------------------------------------

  # Dataset title

  dataset_title <- trimws("Global Lake Ecological Observatory Network: Long term chloride concentration from 529 lakes and reservoirs around North America and Europe: 1940-2016")

  # Dataset keywords

  keywords = trimws(c("chloride", "lakes", "reservoirs", "roads", "limnology",
                      "salt", "impervious surface", "freshwater", "land use",
                      "land cover", "time series", "GLEON",
                      "Global Lake Ecological Observatory Network", "NSF",
                      "National Science Foundation"))

  # Geographic information

  geographic_location <- trimws("North America and Europe")

  coordinate_north = 69.0

  coordinate_east = 28.53

  coordinate_south = 28.38

  coordinate_west <- -119.95

  # Funding information

  funding_title = trimws("Collaborative research: Building analytical, synthesis, and human network skills needed for macrosystem science: A next generation graduate student training model based on GLEON")

  funding_grants = trimws("National Science Foundation 1137327 and 1137353")

  # Set root level parameters

  data_package_id <- trimws("edi.8.1")

  system <- "edi"

  # Set access

  author_system <- "edi"

  allow_principals <- trimws(c("uid=csmith,o=LTER,dc=ecoinformatics,dc=org",
                               "public"))

  allow_permissions <- trimws(c("all",
                                "read")) # order follows allow_principals

  access_order <- trimws("allowFirst")

  access_scope <- trimws("document")


  # Do not edit these -----------------------------------------------------------

  # Identify dataset name

  template <- trimws(list.files(path, pattern = "*_template.docx"))

}
