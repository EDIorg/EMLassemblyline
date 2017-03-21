#' User guide
#'
#' @description  A guide for creating files required by functions of the EMLtools package.
#'
#' @usage run_guide()
#'
#' @return Prompts the user to complete a series of tasks before running other functions
#' of the EMLtools package.
#'
#' @seealso \code{\link{write_attributes}} for writing data attributes
#' @seealso \code{\link{write_factors}} for writing data factors
#' @seealso \code{\link{create_eml}} for creating EML
#' @seealso \code{\link{copy_templates}} for creating template files
#'


run_guide <- function() {

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
                 "\n", "Remove any non-used intellectual rights files in the working directory.",
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

