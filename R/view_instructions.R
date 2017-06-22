#' Instructions for metadata templates
#'
#' @description  
#'     Instructions for the metadata templates and configuration file imported 
#'     to the dataset working directory by \code{import_templates}.
#'
#' @usage 
#'     view_instructions()
#'
#' @return
#'     Upon executing this function a series of instructions are printed to the 
#'     Console window. Proceed to the next step by pressing <enter>. Additional 
#'     details for each step are listed below.
#'
#' @details
#' 
#'     Detailed instructions for each step:
#'          
#'     \emph{"Move your data table(s) to the working directory."} Move your 
#'     data tables to the dataset working directory and name them following the 
#'     convention 'datasetname_datatablename', where datasetname is the name 
#'     of your dataset (e.g. gleon_chloride), and datatablename is the name of 
#'     your data table (e.g. lake_characteristics). In this example, the name 
#'     of the data table becomes 'gleon_chloride_lake_characteristics'.
#'          
#'     \emph{"Write an abstract for your dataset ..."} Write an abstract for 
#'     your dataset in the file datasetname_abstract.txt. Rename the file 
#'     following convention (e.g. gleon_chloride_abstract). The abstract 
#'     should cover what, why, when, where, and how. The template is a UTF-8 
#'     formatted file. Use only UTF-8 symbols and text.
#'     
#'     \emph{"Place additional information ..."} Place additional info in the 
#'     file datasetname_additional_info.txt. Rename the file following 
#'     convention (e.g. gleon_chloride_additional_info). This is a good place 
#'     to share additional text based information that doesn't fall under the 
#'     scope of the abstract or methods (e.g. a list of research articles or 
#'     theses derived from this dataset). The template is a UTF-8 formatted 
#'     file. Use only UTF-8 symbols and text. Delete this file if you have no 
#'     additional information to present.
#'     
#'     \emph{"Select an intellectual rights license for your dataset."} Select 
#'     an intellectual rights license. We have two recommendations: 
#'     'datasetname_cc_by_4.0_intellectual_rights.txt' and 
#'     'datasetname_cc0_1_intellectual_rights.txt'. Do not edit the text of
#'     these files. Delete the file you will not be using. Rename the file you
#'     will be using following convention (e.g. 
#'     gleon_chloride_intellectual_rights).
#'     
#'     \emph{"Write the methods for your dataset ..."} Explain the methods used
#'      to create this dataset in the file datasetname_methods.txt. Rename this 
#'      file following convention (e.g. gleon_chloride_methods). Be specific, 
#'      include instrument descriptions, or point to a protocol online. If this 
#'      dataset is a synthesis of other datasets please specify dataset origins, 
#'      preferably their DOI or URL plus general citation information. This 
#'      file is a UTF-8 formatted file. Use only UTF-8 symbols and text.
#'     
#'     \emph{"Enter personnel information for your dataset ..."} Enter 
#'     personnel information for your dataset in the file 
#'     datasetname_personnel.txt. This is a tab delimited file that can be 
#'     edited with a spreadsheet editor. Rename the file according to 
#'     convention (e.g. gleon_chloride_personnel). Valid entries for role are:
#'     \itemize{
#'         \item \strong{creator} Dataset creator
#'         \item \strong{pi} Principal investigator associated with this 
#'         dataset
#'         \item \strong{contact} Dataset contact
#'     } 
#'      Any other entries into the 'role' column are acceptable and will be 
#'      defined under the associated party element of this dataset. If a person 
#'      serves more than one role, duplicate this persons information in 
#'      another row but with the new role. A dataset creator, contact, and 
#'      principal investigator are mandatory. This file is a UTF-8 formatted 
#'      file. Use only UTF-8 symbols and text.
#'     
#'     \emph{"Add data table attributes ..."} Add data table attributes to the 
#'     file datasetname_datatablename_attributes.txt. This is a tab delimited 
#'     file that can be edited with a spreadsheet editor. Rename the file 
#'     according to convention (e.g. 
#'     gleon_chloride_lake_characteristics_attributes). Create an attributes 
#'     file for each data table. This file is a UTF-8 formatted file. Use only 
#'     UTF-8 symbols and text. Instructions for completing the attribute table 
#'     are as follows:
#'     \itemize{
#'         \item \strong{attributeName} Attribute name as it appears in the
#'         data table and in the same order as in the data table.
#'         \item \strong{attributeDefinition} Define the attribute. Be 
#'         specific, it can be lengthy.
#'         \item \strong{class} Specify the attribute class. This is the type 
#'         of value stored under the attribute. Valid options are: 
#'         \itemize{
#'             \item \strong{numeric} For numeric variables.
#'             \item \strong{categorical} For categorical variables.
#'             \item \strong{character} For variables containing text or 
#'             symbols that are not categorical. 
#'             \item \strong{Date} For date time data.
#'         }
#'         
#'         The list of valid options are case sensitive. If an attribute has 
#'         class of "numeric" or "Date", then all values of this attribute must 
#'         be either numeric or date time. If any character strings are present 
#'         in an otherwise "numeric" attribute, this attribute must be 
#'         classified as "character". Similarly if any values of a "Date" 
#'         attribute do not match the date time format string (details below), 
#'         then this attribute must be classified as "character".
#'         \item \strong{unit} If an attributes class is numeric, then you must
#'         provide units. If the attribute is numeric but does not have units, 
#'         enter "dimensionless". If the attribute class is a character or 
#'         vector, then leave the unit field blank. If the attribute is 
#'         numeric and has units search the standard unit dictionary (opened 
#'         in the source window) for the unit of interest and enter the unit 
#'         "name" as it appears in the  dictionary. Unit names are case 
#'         sensitive. If you cannot find a unit in the dictionary, enter the 
#'         unit in the tab delimited UTF-8 formatted file 
#'         datasetname_custom_units.txt. Rename this file to accord with the 
#'         naming convention (e.g. gleon_chloride_custom_units). Valid custom 
#'         units must be convertible to SI Units (i.e. International System of 
#'         Units). If it cannot be converted to SI then list it in the 
#'         attribute defintion and enter "dimensionless" in the unit field. To 
#'         create a custom unit define the:
#'         \itemize{
#'             \item \strong{id} This is equivalent to the unit name. Reference 
#'             the standard unit dictionary formatting.
#'             \item \strong{unitType} The type of unit being defined. 
#'             Reference the dictionary for examples.
#'             \item \strong{parentSI} The SI equivalent of the id you have 
#'             entered.
#'             \item \strong{multiplierToSI} This is the multiplier to convert 
#'             from your custom unit to the SI unit equivalent.
#'             \item \strong{description} A description of the custom unit. 
#'             Reference the dictionary for examples.
#'         }
#'         \item \strong{dateTimeFormatString} Enter the date time format 
#'         string for each attribute of "Date" class. Remember, a class of 
#'         "Date" specifies the attribute as a date, time, or datetime. Enter 
#'         the format string in this field. If the attribute class is not 
#'         "Date", leave this field blank. Below are rules for constructing 
#'         format strings. Additional information is listed under 
#'         "dateTime-eml-attribute" of the current EML specification 
#'         (https://knb.ecoinformatics.org/#external//emlparser/docs/index.html).
#'         Valid date time formats are a combination of date, time, and time 
#'         zone strings. Below are a set of best practice recomendations that 
#'         we strongly encourage you to follow, but are by no means the full 
#'         list of currently acceptable format strings.
#'         \itemize{
#'             \item \strong{Date format strings:} YYYY-MM-DD, YYYY, YYYYMMDD, 
#'             YYYY-MM, YYYYMM, YYYY-DDD, YYYYDDD; where YYYY is year, MM is 
#'             month, DD is day of month, and DDD is day of year.
#'             \item \strong{Time format strings:} hh:mm:ss.sss, hhmmss.sss,
#'             hh:mm:ss, hhmmss, hh:mm, hhmm, hh; where hh is hour (in 24 hr
#'             clock), mm is minute, ss is second, and ss.sss is decimal 
#'             second.
#'             \item\strong{Time zone format strings:} Z, +hh:mm, +hhmm, +hh,
#'             -hh:mm, -hhmm, -hh; where Z (capitalized) is Coordinated 
#'             Universal Time, and + and - denote times ahead and behind UTC
#'             respectively.
#'         }
#'         If reporting a date without time, select one of the date format 
#'         strings. If reporting a date and time, select one date and one time 
#'         format string and combine with a single space (e.g. 
#'         YYYY-MM-DD hh:mm) or with a "T" (e.g. YYYY-MM-DDThh:mm). If 
#'         reporting a date and time, it is recommended that a time zone 
#'         specifier be appended without a space (e.g. YYYY-MM-DD hh:mm-hh:mm, 
#'         or YYYY-MM-DDThh:mm-hh:mm).
#'         \item \strong{missingValueCode} If a code for 'no data' is used,
#'         specify it here (e.g. NA, -99999).
#'         \item \strong{missingValueCodeExplanation} Define the missing value 
#'         code here.
#'     }
#'     \emph{"Fill out the file eml_configuration.R"} Provide additional 
#'     information about your dataset. Detailed instructions are listed as 
#'     comments in this file.
#'     
#'     \emph{"Make sure all files of the working directory are closed."} Some 
#'     functions will error out if these files are open.
#'
#' @export
#'
#' @seealso 
#'     \code{\link{import_templates}} to import metadata templates to the 
#'     dataset working directory.


view_instructions <- function() {
  
  # Find additional details here
  
  readline(paste("Details for each of the following steps are found in the",
                 "\n", "documentation for this function. (i.e. view_instructions.R).",
                 "\n",
                 "Press <enter> to continue.",
                 sep = ""))
  writeLines("\n")
  
  # Data tables
  
  readline(paste("Move the data table(s) to the working directory.",
                 "\n", "Rename the files with the recommended convention.",
                 "\n", "See R Documentation of view_instructions for details.",
                 "\n",
                 "Press <enter> to continue.",
                 sep = ""))
  writeLines("\n")
  
  # Abstract
  
  readline(paste("Write an abstract for your dataset in the file datasetname_abstract.txt.",
                 "\n", "Rename this file following convention.",
                 "\n",
                 "Press <enter> to continue.",
                 sep = ""))
  writeLines("\n")
  
  # Additional info
  
  readline(paste("Place additional information in datasetname_additional_info.txt.",
                 "\n", "Follow the file naming convention if you will be using it.",
                 "\n", "Delete this file if you have no additional information to present.",
                 "\n",
                 "Press <enter> to continue.",
                 sep = ""))
  writeLines("\n")
  
  # Intellectual rights license
  
  readline(paste("Select an intellectual rights license for your dataset.",
                 "\n", "Rename this file following convention.",
                 "\n", "Delete the license you are not using.",
                 "\n",
                 "Press <enter> to continue.",
                 sep = ""))
  writeLines("\n")
  
  # Methods
  
  readline(paste("Write the methods for your dataset in datasetname_methods.txt.",
                 "\n", "Rename this file following convention.",
                 "\n",
                 "Press <enter> to continue.",
                 sep = ""))
  writeLines("\n")
  
  # Personnel information
  
  readline(paste("Enter personnel information for your dataset in datasetname_personnel.txt.",
                 "\n", "Rename this file following convention.",
                 "\n",
                 "Press <enter> to continue.",
                 sep = ""))
  writeLines("\n")
  
  # Table attributes
  
  standardUnits <- get_unitList()
  View(standardUnits$units)
  
  readline(paste("Add data table attributes to datatablename_attributes.txt.",
                 "\n", "The standard units dictionary has been opened for you.",
                 "\n", "Create an attributes file for each data table and rename following convention.",
                 "\n",
                 "Press <enter> to continue.",
                 sep = ""))
  writeLines("\n")
  
  # Configuration file
  
  readline(paste("Fill out the file eml_configuration.R.",
                 "\n",
                 "Press <enter> to continue.",
                 sep = ""))
  writeLines("\n")
  
  # Close all open files
  
  readline(paste("Make sure all files of the working directory are closed.",
                 "\n",
                 "Press <enter> to continue.",
                 sep = ""))
  writeLines("\n")
  
  # Now other functions can be run
  
  readline(paste("Now you can use these functions:",
                 "\n", "define_catvars()",
                 "\n", "extract_geocoverage()",
                 "\n", "make_eml()",
                 "\n",
                 "Press <enter> to continue.",
                 sep = ""))
  writeLines("\n")
  
}

