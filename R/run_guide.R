#' User guide
#'
#' @description  A guide for creating files required by functions of the EMLtools package.
#'
#' @usage run_guide()
#'
#' @return Prompts the user to complete a series of tasks before running other functions
#' of the EMLtools package.
#'
#' @seealso \code{\link{eml_configuration}} for configuring eml
#' @seealso \code{\link{write_attributes}} for writing data attributes
#' @seealso \code{\link{write_factors}} for writing data factors
#' @seealso \code{\link{create_eml}} for creating EML


run_guide <- function() {

  # Create working directory

  readline(paste("Create a working directory for this data set.",
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

  # Move data tables

  readline(paste("Move the data table(s) to the working directory.",
                 "\n", "Add a name for this data set as a prefix.",
                 "\n", "E.g. gleon_lake_chloride.csv",
                 "\n", "Where gleon_ is the data set name, and lake_chloride.csv is the data table name.",
                 "\n", "Do this for each data table.",
                 "\n",
                 "Press <enter> when done.",
                 sep = ""))
  writeLines("\n")

  # Create 'datasetName_template.docx'

  readline(paste("Move the EDI metadata template of this dataset to the working directory.",
                 "\n", "Rename the template with the same dataset name (as above) and append with _template.docx.",
                 "\n", "E.g. gleon_template.docx",
                 "\n",
                 "Press <enter> when done.",
                 sep = ""))
  writeLines("\n")

  # Create 'datasetName_methods.docx'

  readline(paste("Create the file datasetName_methods.docx in the working directory.",
                 "\n", "Copy over the methods from template.docx",
                 "\n", "Remove any special formatting or unique characters.",
                 "\n",
                 "Press <enter> when done.",
                 sep = ""))
  writeLines("\n")

  # Manually create 'datasetName_abstract.docx'

  readline(paste("Create the file datasetName_abstract.docx in the working directory.",
                 "\n", "Copy over the abstract from template.docx.",
                 "\n", "Remove any special formatting or unique characters.",
                 "\n",
                 "Press <enter> when done.",
                 sep = ""))
  writeLines("\n")

  # Manually create 'datasetName_personnel.xlsx'

  readline(paste("Create the file datasetName_personnel.xlsx in the working directory.",
                 "\n", "Copy over the investigators information from template.docx.",
                 "\n", "Add principal investigator information (even if duplicating).",
                 "\n", "Add dataset contact information (even if duplicating).",
                 "\n", "Remove any special formatting or unique characters.",
                 "\n",
                 "Press <enter> when done.",
                 sep = ""))
  writeLines("\n")

  # Manually create 'datasetName_intellectual_rights.md'

  readline(paste("Create the file datasetName_intellectual_rights.md in the working directory.",
                 "\n", "Copy over the intellectual rights for this dataset.",
                 "\n", "Remove any special formatting or unique characters.",
                 "\n",
                 "Press <enter> when done.",
                 sep = ""))
  writeLines("\n")

  # Manually create 'datasetName_datatableName_attributes_draft.xlsx'

  readline(paste("Create the file datasetName_datatableName_attributes_draft.xlsx in the working directory.",
                 "\n", "Copy over the data table information from template.docx.",
                 "\n", "Be sure to include the column names.",
                 "\n", "Create a file for each data table of the dataset.",
                 "\n",
                 "Press <enter> when done.",
                 sep = ""))
  writeLines("\n")

  # Manually create 'eml_configuration.R'

  readline(paste("Edit the file eml_configuration.R and save a copy in the working directory.",
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

