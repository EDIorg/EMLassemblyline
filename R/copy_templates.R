#' Copy template files to user specified directory
#'
#' @description  A function for populating the working directory of a dataset with template
#' files to be filled out by the user. These files are inputs to some functions of this 
#' code package.
#'
#' @usage copy_templates(path)
#'
#' @param path A path to the working directory containing data tables and other files 
#' required by the EMLtools package.
#'
#' @return 'datasetname_abstract.docx' - File containing the dataset abstract.
#' 
#' 'datasetname_methods.docx' - File containing the dataset methods.
#' 
#' 'datasetname_*_intellectual_rights.md' - File containing the intellectual rights of
#' the dataset. The two release licenses recommended by the Environmental Data Initiative
#' are available.
#' 
#' 'datasetname_personnel.xlsx' - File containing personnel information relevant to the
#' dataset.
#' 
#' 'datasetname_datatablename_attributes_draft.xlsx' - File containing data table attribute 
#' information. Replicate this file for each data table.
#' 
#' 'datasetname_shapefilesetname_shape_files_attributes_draft.xlsx' - File containing
#' attribute information for a collection of shape files. Replicate this file for each data
#' table.
#' 
#' 'eml_configuration.R' - File containing parameters for functions of this code package.
#' 
#' NOTEs: Replace 'datasetname' with the name of your dataset. Replace datatablename and
#' shapefilesetname with the name of your data tables and shape file sets. Remove the text
#' '_cc0_1' or '_cc_by_4.0' from the intellectual rights file you choose to use.
#'
#' @seealso \code{\link{run_guide}} for guidance on setting up the working directory
#' @seealso \code{\link{write_attributes}} for writing data attributes
#' @seealso \code{\link{write_factors}} for writing data factors
#' @seealso \code{\link{create_eml}} for creating EML
#'


copy_templates <- function(path){
  
  file.copy(from = paste(path.package("EMLtools"),
                         "/templates/eml_configuration.R",
                         sep = ""),
            to = path)
  
  file.copy(from = paste(path.package("EMLtools"),
                         "/templates/datasetname_abstract.docx",
                         sep = ""),
            to = path)
  
  file.copy(from = paste(path.package("EMLtools"),
                         "/templates/datasetname_cc0_1_intellectual_rights.md",
                         sep = ""),
            to = path)
  
  file.copy(from = paste(path.package("EMLtools"),
                         "/templates/datasetname_cc_by_4.0_intellectual_rights.md",
                         sep = ""),
            to = path)
  
  file.copy(from = paste(path.package("EMLtools"),
                         "/templates/datasetname_datatablename_attributes_draft.xlsx",
                         sep = ""),
            to = path)
  
  file.copy(from = paste(path.package("EMLtools"),
                         "/templates/datasetname_methods.docx",
                         sep = ""),
            to = path)
  
  file.copy(from = paste(path.package("EMLtools"),
                         "/templates/datasetname_personnel.xlsx",
                         sep = ""),
            to = path)
  
  file.copy(from = paste(path.package("EMLtools"),
                         "/templates/datasetname_shapefilesetname_shape_files_attributes_draft.xlsx",
                         sep = ""),
            to = path)
  
  
}



