# The configuration file for EMLassemblyline
#
# This file contains information about the dataset that is not included in the 
# template files or as arguments to the function.
#
# See import_templates.R to copy metadata templates (including this one) to the dataset working 
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
# list of sampling site coordinates can be supplied (in addition to those below)
# by running the extract_geocoverage function.
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
# user_id <- "casmith"

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


# Enter the full name(s) of your data tables, including the file extension. If
# you have more than one file, separate them with a column as in the example
# below.
#
# Example:
#
# table_names <- c("gleon_chloride_concentrations.csv",
#                  "gleon_chloride_lake_characteristics.csv")

table_names <- c("")


# Provide a brief descriptions for your data tables. If more than one data 
# table, then combine your descriptions into a vector (order must follow that 
# listed in table_names as you defined above).
#
# Example:
#
# data_table_descriptions <- c("Long term chloride concentration data from 529 lakes and reservoirs around North America and Europe.",
#                                    "Lake characteristics, including climate, road density, and impervious surface data.")

data_table_descriptions <- c("")


# Enter the URLs of the data tables if you will have them stored on a publicly 
# accessible server (i.e. does not require user ID or password) so PASTA can 
# upload them into the repository. If you will be manually uploading your data 
# tables to PASTA, then leave this object empty (i.e. ""). If more than one data 
# table, then combine your URLs into a vector (order must follow that listed 
# in table_names as you defined above).
#
# Example:
#
# data_table_urls <- c("https://lter.limnology.wisc.edu/sites/default/files/data/gleon_chloride/gleon_chloride_concentrations.csv",
#                      "https://lter.limnology.wisc.edu/sites/default/files/data/gleon_chloride/gleon_chloride_lake_characteristics.csv")

data_table_urls <- c("")


# Define the number of header lines of your data table(s). This is the number of 
# lines prior to the beginning of data. If there is more than one data table, 
# then combine these values into a vector (order must follow that listed in 
# table_names).
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
# a vector (order must follow that listed in table_names).
#
# Example:
#
# record_delimeter <- c("\\r\\n",
#                       "\\r\\n")

record_delimeter <- c("")


# Define the orientation of attributes in your data table. Acceptable value 
# is "column". If there is more than one data table, then combine 
# these values into a vector (order must follow that listed in table_names).
#
# Example:
#
# attribute_orientation <- c("column",
#                            "column")

attribute_orientation <- c("")


# Define the field delimeter of your data tables. Acceptable values are "comma" 
# and "tab". If there is more than one data table, then combine these values 
# into a vector (order must follow that listed in table_names).
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
# combine these values into a vector (order must follow that listed in  
# table_names).
#
# Example:
#
# quote_character <- c("\"",
#                      "\'")

quote_character <- c("")


