#' A guide to completing the metadata templates
#'
#' @description  
#'     A guide to completing the metadata templates and configuration file
#'     created by \code{copy_templates}.
#'
#' @usage run_guide()
#'
#' @return
#'     A series of prompts are displayed in the Console window describing steps
#'     and rules to be followed when filling out the metadata templates.
#'     Press <enter> to proceed to the next step.
#'
#' @details
#' 
#'     Additional details for each step:
#'     
#'     "Move your data table(s) to the working directory." Move your datasets
#'     to the working directory and rename them following the convention 
#'     'datasetname_datatablename.dat', where datasetname is the name of your
#'     dataset (e.g. gleon_chloride), datatablename is the name of your data 
#'     table (e.g. lake_characteristics), and .dat is the file extension of
#'     your data table (e.g. .csv). In this example the data table name is
#'     gleon_chloride_lake_characteristics.csv.
#'     
#'     "Write protect your data table(s)." Write protect your data table(s) to
#'     prevent Microsoft Excel (used in the course of creating EML for your 
#'     dataset) from automatically converting date time fields to an Excel 
#'     preferred format. To do this, right click the file and select 
#'     'Read-only' if using Windows OS or 'locked' for Mac OS.
#'     
#'     "Write an abstract for your dataset ...". Write an abstract for your
#'     dataset in the file datasetname_abstract.docx (e.g.
#'     gleon_chloride_abstract.docx). The abstract should cover what, why, 
#'     when, where, and how. Do not use special characters, symbols, or special
#'     formatting.
#'     
#'     "Place additional information ...". Place additional info in the file 
#'     datasetname_additional_info.docx (e.g. 
#'     gleon_chloride_additional_info.docx). This is a good place to present 
#'     additional text based information that doesn't fall under the scope of
#'     the abstract or methods (e.g. A list of research articles derived from
#'     this dataset). Do not use special characters, symbols, or special formatting.
#'     
#'     "Select an intellectual rights license for your dataset." Select an
#'     intellectual rights license. We have two recommendations: 
#'     'datasetname_cc_by_4.0_intellectual_rights.docx' and
#'     'datasetname_cc0_1_intellectual_rights.docx'. Do not edit the text of
#'     these files. Delete the file you will not be using. Rename the file you
#'     will be using to match the convention 
#'     datasetname_intellectual_rights.docx (e.g. 
#'     gleon_chloride_intellectual_rights.docx)
#'     
#'     "Write the methods for your dataset ...". Write the methods for your
#'     dataset in the file datasetname_methods.docx (e.g. 
#'     gleon_chloride_methods.docx). Please be specific, include instrument 
#'     descriptions, or point to a protocol online. If this is a data 
#'     compilation please specify datasets used, preferably their DOI or URL 
#'     plus general citation information. Do not use special characters, 
#'     symbols, or special formatting.
#'     
#'     "Enter personnel information for your dataset ...". Enter personnel 
#'     information for your dataset in the file datasetname_personnel.xlsx 
#'     (e.g. gleon_chloride_personnel.xlsx). Valid entries for role are: 
#'     "creator" = dataset creator, "pi" = principal investigator, "contact" = 
#'     dataset contact. Any other entries into the 'role' column are 
#'     acceptable and will be defined under the associated party element of 
#'     this dataset. If a person serves more than one role, add this role as 
#'     an additional line. A dataset creator, contact, and principal 
#'     investigator are mandatory. Do not use special characters, 
#'     symbols, or special formatting.
#'     
#'     "Add data table attributes ...". Add data table attributes to the file 
#'     datatablename_attributes_draft.xlsx 
#'     (e.g. gleon_chloride_lake_characteristics_attributes_draft.xlsx). 
#'     Create an attributes file for each data table. ENTER FIELD DEFINITIONS AND INSTRUCTIONS.
#'     STANDARD UNIT DICTIONARY. Do not use special characters, symbols, or special formatting.
#'
#'     "Fill out the file eml_configuration.R." Provide additional information 
#'     about your dataset. Detailed instructions are listed as comments in this 
#'     file.
#'     
#'     "Make sure all files of the working directory are closed." Some 
#'     functions will error out if files are open.
#'
#' @seealso \code{\link{copy_templates}} to copy metadata templates to the 
#'     dataset working directory.


run_guide <- function() {
  
  library("EML")
  
  # Additional details are presented in the R Documentation of this function
  
  readline(paste("Details for each of the following steps are found in the documentation for this function.",
                 "\n",
                 "Press <enter> when done.",
                 sep = ""))
  writeLines("\n")
  
  # Move data tables to the working directory
  
  readline(paste("Move the data table(s) to the working directory.",
                 "\n", "Rename the files with the recommended convention.",
                 "\n", "See R Documentation of run_guide() for details.",
                 "\n", "Do this for each data table.",
                 "\n",
                 "Press <enter> when done.",
                 sep = ""))
  writeLines("\n")
  
  # Write protect data tables
  
  readline(paste("Write protect your data table(s).",
                 "\n", "Right click on the data table file.",
                 "\n", "Select 'Read-only' for Windows OS.",
                 "\n", "Select 'locked' for Mac OS.",
                 "\n", "Click 'OK'.",
                 "\n", "Do this for each data table.",
                 "\n",
                 "Press <enter> when done.",
                 sep = ""))
  writeLines("\n")
  # 
  #   # Create 'datasetName_template.docx'
  #   
  #   readline(paste("Move the EDI metadata template of this data set to the working directory.",
  #                  "\n", "Rename the template with the data set name and append with _template.docx.",
  #                  "\n", "E.g. gleon_template.docx",
  #                  "\n", "'gleon' is the data set name." ,
  #                  "\n",
  #                  "Press <enter> when done.",
  #                  sep = ""))
  #   writeLines("\n")
  
  # Create abstract
  
  readline(paste("Write an abstract for your dataset in the file datasetname_abstract.docx.",
                 "\n", "Rename this file following the naming convention.",
                 "\n",
                 "Press <enter> when done.",
                 sep = ""))
  writeLines("\n")
  
  # Additional info
  
  readline(paste("Place additional information in datasetname_additional_info.docx.",
                 "\n", "Follow the file naming convention if you will be using it.",
                 "\n", "Delete this file if you have no additional info to present.",
                 "\n",
                 "Press <enter> when done.",
                 sep = ""))
  writeLines("\n")
  
  # Select an intellectual rights license
  
  readline(paste("Select an intellectual rights license for your dataset.",
                 "\n", "Rename the one you choose and follow the naming convention.",
                 "\n", "Delete the file you do not want.",
                 "\n",
                 "Press <enter> when done.",
                 sep = ""))
  writeLines("\n")
  
  # Add methods
  
  readline(paste("Write up methods for your dataset in the file datasetname_methods.docx.",
                 "\n", "Rename this file following the naming convention.",
                 "\n",
                 "Press <enter> when done.",
                 sep = ""))
  writeLines("\n")
  
  # Add personnel information
  
  readline(paste("Enter personnel information for your dataset in the file datasetname_personnel.xlsx.",
                 "\n", "Rename this file following the naming convention.",
                 "\n",
                 "Press <enter> when done.",
                 sep = ""))
  writeLines("\n")
  
  # Add table attributes (draft)
  
  standardUnits <- get_unitList()
  View(standardUnits$units)
  
  readline(paste("Add data table attributes to the file datatablename_attributes_draft.xlsx.",
                 "\n", "The standard units dictionary has been opened for you.",
                 "\n", "Create an attributes file for each data table and rename following convention.",
                 "\n",
                 "Press <enter> when done.",
                 sep = ""))
  writeLines("\n")
  
  # Fill out the configuration file
  
  readline(paste("Fill out the file eml_configuration.R.",
                 "\n",
                 "Press <enter> when done.",
                 sep = ""))
  writeLines("\n")
  
  # Close all open files
  
  readline(paste("Make sure all files of the working directory are closed.",
                 "\n",
                 "Press <enter> when done.",
                 sep = ""))
  writeLines("\n")
  
  # Green light to run other functions of this package
  
  readline(paste("Now you can use these functions:",
                 "\n", "write_attributes()",
                 "\n", "write_factors()",
                 "\n", "extract_geocoverage()",
                 "\n", "create_eml()",
                 "\n",
                 "Press <enter> when done.",
                 sep = ""))
  writeLines("\n")
  
}

