#' User guide
#'
#' @description  A guide for creating files required by functions of the EMLtools package.
#'
#' @usage run_guide()
#'
#' @return Prompts the user to complete a series of tasks before running other functions
#' of the EMLtools package.
#'
#' @details
#'     datasetname_abstract.docx Enter the abstract for your dataset here.
#'     
#'     datasetname_additional_info.docx This is a good place to present 
#'     additional text based information that doesn't fall under the scope
#'     of the abstract or methods (e.g. A list of research articles derived
#'     from this dataset).
#'     
#'     datasetname_cc_by_4.0_intellectual_rights.docx One of two recommended 
#'     licenses for your dataset. Do not edit this text.
#'     
#'     datasetname_cc0_1_intellectual_rights.docx One of two recommended 
#'     licenses for your dataset. Do not edit this text.
#'     
#'     datasetname_methods.docx Enter the methods used to create this dataset
#'     here.
#'     
#'     datasetname_personnel.xlsx Enter information on personnel associated
#'     with this dataset here.
#'     
#'     datatablename_attributes_draft.xlsx Enter information about the 
#'     attributes of your data here. Replicate this file for each of your data
#'     entities (i.e. data tables).
#'     
#'     eml_configuration.R Enter additional information about the data entities
#'     and dataset here.
#'
#' @seealso \code{\link{copy_templates}} to copy templates to the dataset
#'     working directory.


run_guide <- function() {
  
  #library("EML")

  # Create working directory

  readline(paste("Create a working directory for this data set.",
                 "\n",
                 "Press <enter> when done.",
                 sep = ""))
  writeLines("\n")

  # Move data tables
  
  readline(paste("Move the data table(s) to the working directory.",
                 "\n", "Use underscores to span spaces (e.g. lake_chloride.csv)",
                 "\n", "Do this for each data table.",
                 "\n",
                 "Press <enter> when done.",
                 sep = ""))
  writeLines("\n")
  
  # Write protect the data tables
  
  readline(paste("Write protect your data table(s).",
                 "\n", "Right click on the data table file.",
                 "\n", "Select 'properties'.",
                 "\n", "Select 'Read-only'.",
                 "\n", "Click 'Apply'.",
                 "\n", "Click 'OK'.",
                 "\n", "Do this for each data table.",
                 "\n",
                 "Press <enter> when done.",
                 sep = ""))
  writeLines("\n")

  # Create 'datasetName_template.docx'
  
  readline(paste("Move the EDI metadata template of this data set to the working directory.",
                 "\n", "Rename the template with the data set name and append with _template.docx.",
                 "\n", "E.g. gleon_template.docx",
                 "\n", "'gleon' is the data set name." ,
                 "\n",
                 "Press <enter> when done.",
                 sep = ""))
  writeLines("\n")

  # Create 'datasetName_methods.docx'

  readline(paste("Fill out the file datasetname_methods.docx in the working directory.",
                 "\n", "Remove any special formatting or unique characters.",
                 "\n",
                 "Press <enter> when done.",
                 sep = ""))
  writeLines("\n")

  # Manually create 'datasetName_abstract.docx'

  readline(paste("Fill out the file datasetname_abstract.docx in the working directory.",
                 "\n", "Copy over the abstract from template.docx.",
                 "\n", "Remove any special formatting or unique characters.",
                 "\n",
                 "Press <enter> when done.",
                 sep = ""))
  writeLines("\n")

  # Manually create 'datasetName_personnel.xlsx'

  readline(paste("Fill out the file datasetname_personnel.xlsx in the working directory.",
                 "\n", "Add information for the dataset creator(s) (mandatory).",
                 "\n", "Add principal investigator(s) information (mandatory, even if duplicating).",
                 "\n", "Add dataset contact information (mandatory, even if duplicating).",
                 "\n", "Add associated personnel information (optional, even if duplicating).",
                 "\n",
                 "\n", "Acceptable entries for the role field:",
                 "\n", "\"pi\" (for principal investigator).",
                 "\n", "\"creator\" (for dataset creator).",
                 "\n", "\"contact\" (for dataset contact)",
                 "\n", "Any other entries are accepted and will be catagorized as associated party.",
                 "\n",
                 "\n", "Remove any special formatting or unique characters.",
                 "\n",
                 "Press <enter> when done.",
                 sep = ""))
  writeLines("\n")

  # Manually create 'datatableName_attributes_draft.xlsx'
  
  #standardUnits <- get_unitList()
  #View(standardUnits$units)
  
  readline(paste("Fill out the file datatablename_attributes_draft.xlsx in the working directory.",
                 "\n", "Be sure to include the column names.",
                 "\n", "Remove all formatting.",
                 "\n", "Create a file for each data table of the dataset.",
                 "\n",
                 "Press <enter> when done.",
                 sep = ""))
  writeLines("\n")

  # Manually create 'datasetName_intellectual_rights.md'
  
  readline(paste("Select an intellectual rights license and edit the file name",
                 "\n", "to have the form datasetname_intellectual_rights.md.",
                 "\n", "Remove any non-used intellectual rights files from the working directory.",
                 "\n",
                 "Press <enter> when done.",
                 sep = ""))
  writeLines("\n")
  
  # Manually create 'datasetName_additional_info.docx'
  
  readline(paste("If you would like to include additional information with this dataset, enter it into datasetname_additional_info.docx.",
                 "\n", "This is a good place for references to publications and theses derived from this dataset.",
                 "\n", "Including this this information is optional.",
                 "\n", "Remove any special formatting or unique characters.",
                 "\n",
                 "Press <enter> when done.",
                 sep = ""))
  writeLines("\n")
  
  # Manually create 'shapefilesetname_shape_files_attributes_draft.xlsx'
  
  readline(paste("If you have spatial data files, then fill out the file shapefilesname_shape_files_attributes_draft.xlsx.",
                 "\n", "Be sure to include the shape file attribute names.",
                 "\n", "Remove all formatting.",
                 "\n", "Create a file for each .zip directory of shape files.",
                 "\n",
                 "Press <enter> when done.",
                 sep = ""))
  writeLines("\n")
  
  # Manually create 'eml_configuration.R'

  readline(paste("Fill out the file eml_configuration.R.",
                 "\n",
                 "Press <enter> when done.",
                 sep = ""))
  writeLines("\n")

  # Notify user to close all open files

  readline(paste("Make sure all files of the working directory are closed.",
                 "\n",
                 "Press <enter> when done.",
                 sep = ""))
  writeLines("\n")


  # Notify user they are ready to run scripts

  readline(paste("OK. Now you can use these functions:",
                 "\n", "write_attributes()",
                 "\n", "write_factors()",
                 "\n", "create_eml()",
                 "\n",
                 "Press <enter> when done.",
                 sep = ""))
  writeLines("\n")

}

