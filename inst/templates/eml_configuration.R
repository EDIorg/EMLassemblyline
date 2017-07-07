# The configuration file
#
# This file contains information about the dataset that is not included in the 
# template files. It is called upon by compile_attributes.R, define_factors.R, and 
# create_eml.R. This file contains the EMLtools version number that is required 
# for successful operation.
#
# See copy_templates.R to copy metadata templates to the dataset working 
# directory.



# Define dataset parameters ---------------------------------------------------


# Enter a title for your dataset. Be descriptive (more than 5 words). We 
# recommend the following syntax:
#
# "Project name: Broad description: Time span"
#
# Example:
#
# dataset_title <- "Global Lake Ecological Observatory Network: Long term chloride concentration from 529 lakes and reservoirs around North America and Europe: 1940-2016"

dataset_title <- ""


# Enter the name of your dataset. This name must match that used for your 
# template files.
# 
# Example:
#
# dataset_name <- "gleon_chloride"

dataset_name <- ""


# List keywords that best describe your dataset. Consult this resource 
# (http://vocab.lternet.edu/vocab/vocab/index.php) as you populate the list.
# Additionally, use keywords that describe your lab, station, and project 
# (e.g. GLEON, NSF). Combine keywords into a vector.
#
# Example:
#
# keywords = c("chloride", "lakes", "reservoirs", "roads", "limnology",
#              "salt", "impervious surface", "freshwater", "land use",
#              "land cover", "time series", "GLEON",
#              "Global Lake Ecological Observatory Network", "NSF",
#              "National Science Foundation")

keywords = c("")


# Enter the beginning and ending dates covered by your dataset.
#
# Example:
#
# begin_date <- "1940-01-31"
# 
# end_date <- "2016-07-06"

begin_date <- ""

end_date <- ""


# Enter the spatial bounding coordinates of your dataset (in decimal degrees) 
# and a brief description. Longitudes west of the prime meridian and latitudes 
# south of the equator are prefixed with a minus sign (i.e. dash -). A detailed 
# list of sampling site coordinates can be supplied by using the 
# extract_geocoverage() function.
#
# Example:
#
# geographic_location <- "North America and Europe"
# 
# coordinate_north <- 69.0
# 
# coordinate_east <- 28.53
# 
# coordinate_south <- 28.38
# 
# coordinate_west <- -119.95

geographic_location <- ""

coordinate_north <- 

coordinate_east <- 

coordinate_south <-

coordinate_west <- 


# Provide information on funding of this work. If several grants were involved,
# list the main grant first.
#
# Example
#
# funding_title = "Collaborative research: Building analytical, synthesis, and human network skills needed for macrosystem science: A next generation graduate student training model based on GLEON"
# 
# funding_grants = "National Science Foundation 1137327 and 1137353"
  
funding_title = ""

funding_grants = ""
  

# Specify whether data collection for this dataset is "ongoing" or "completed".
#
# Example:
# 
# maintenance_description <- "completed"

maintenance_description <- ""  
  

# Enter information about the system you are publishing this dataset under. 
# If you have not been provided a value for this field by EDI staff, enter the 
# default value "edi".
#
# Example:
#
# root_system <- "edi"

root_system <- ""


# Enter your user ID. If you haven't received a user ID from EDI staff, use 
# the default value "name".
#
# Example:
#
# user_id <- "clnsmith"

user_id <- ""


# Enter the author system your user ID is associated with. If you haven't 
# received a author system specification from EDI staff, use the default value 
# "edi".
# 
# Example:
#
# author_system <- "edi"

author_system <- ""


# Enter the data package ID. If you have not been provided a package ID from 
# EDI staff, enter the default value "edi.1.1".
#
# Example:
#
# data_package_id <- "edi.8.2"

data_package_id <- ""

  

  
# Set data table parameters ---------------------------------------------------


# Enter the full name(s) of your data tables as a combined vector.
#
# Example:
#
# table_names <- c("gleon_chloride_concentrations.csv",
#                  "gleon_chloride_lake_characteristics.csv")

table_names <- c("")


# Provide a brief descriptions for your data tables. If more than one data 
# table, then combine your descriptions into a vector (order must follow that 
# listed in the table_names object).
#
# Example:
#
# data_table_descriptions <- c("Long term chloride concentration data from 529 lakes and reservoirs around North America and Europe.",
#                                    "Lake characteristics, including climate, road density, and impervious surface data.")

data_table_descriptions <- c("")


# Enter the URLs of the data tables if you will have them stored on a publicly 
# accessible server (i.e. does not require user ID or password) so PASTA can 
# upload them into the repository. If you will be manually uploading your data 
# tables to PASTA, then leave this object empty (i.e. ""), and enter a value 
# for storage_type below. If more than one data table, then combine your URLs 
# into a vector (order must follow that listed in the table_names object).
#
# Example:
#
# data_table_urls <- c("https://lter.limnology.wisc.edu/sites/default/files/data/gleon_chloride/gleon_chloride_concentrations.csv",
#                      "https://lter.limnology.wisc.edu/sites/default/files/data/gleon_chloride/gleon_chloride_lake_characteristics.csv")

data_table_urls <- c("")


# If your data is not available online (i.e. doesn't have a publicly accessible 
# URL), describe the medium on which the data is stored. If more than one data 
# table, then combine your storage types into a vector (order must follow that 
# listed in the table_names object).
#
# Example:
#
# storage_type <- ("Departmental server")
#

storage_type <- c("")



# Define the number of header lines of your data table(s). This is the number of 
# lines prior to the beginning of data. If there is more than one data table, 
# then combine these values into a vector (order must follow that listed in the 
# table_names object).
#
# Example:
#
# num_header_lines <- c("1", 
#                       "1")

num_header_lines <- c("")


# Define the end of line specifier for your data table(s). This character 
# denotes the end of a data row. If your computers operating system is 
# Windows, then enter "\\r\\n". If you are using a Mac OS then use the value 
# "\\n". If there is more than one data table, then combine these values into 
# a vector (order must follow that listed in the table_names object).
#
# Example:
#
# record_delimeter <- c("\\r\\n",
#                       "\\r\\n")

record_delimeter <- c("")


# Define the orientation of attributes in your data table. Acceptable value 
# is "column". If there is more than one data table, then combine 
# these values into a vector (order must follow that listed in the table_names 
# object).
#
# Example:
#
# attribute_orientation <- c("column",
#                            "column")

attribute_orientation <- c("")


# Define the field delimeter of your data tables. Acceptable values are "comma" 
# and "tab". If there is more than one data table, then combine these values 
# into a vector (order must follow that listed in the table_names object).
#
# Example:
#
# field_delimeter <- c("comma",
#                      "comma")

field_delimeter <- c("")


# Define the quote character used in your data tables. If the quote character 
# is quotation marks, then enter \" below. If the quote character is an
# apostrophe, then enter \' below. If there is no quote character used in your
# data tables then leave as is. If there is more than one data table, then 
# combine these values into a vector (order must follow that listed in the 
# table_names object).
#
# Example:
#
# quote_character <- c("\"",
#                      "\'")

quote_character <- c("")


