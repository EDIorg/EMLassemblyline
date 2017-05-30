#' Copy metadata templates to the dataset working directory
#'
#' @description  Create a working directory for your dataset and execute this
#'     function. This function populates the dataset working directory with 
#'     templates to be filled out by the data provider. These templates store
#'     information about the data that are later called upon by 
#'     \code{create_eml} when building the EML and writing the .xml file.
#'
#' @usage copy_templates(path)
#'
#' @param path A path to the dataset working directory.
#'
#' @return 
#'     \emph{datasetname_abstract.docx} A file for the abstract of your data.
#'     
#'     \emph{datasetname_additional_info.docx} A file for additional information 
#'     about your data.
#'     
#'     \emph{datasetname_custom_units.xlsx} A file for custom units not defined
#'     in the standard unit dictionary.
#'     
#'     \emph{datasetname_cc_by_4.0_intellectual_rights.docx} One of two 
#'     intellectual rights licenses to consider for your dataset. Do not edit 
#'     this text.
#'     
#'     \emph{datasetname_cc0_1_intellectual_rights.docx} One of two 
#'     intellectual rights licenses to consider for your dataset. Do not edit 
#'     this text.
#'     
#'     \emph{datasetname_methods.docx} A file for methods used in create your 
#'     data.
#'     
#'     \emph{datasetname_personnel.xlsx} A file for information on personnel 
#'     associated with this data.
#'     
#'     \emph{datasetname_datatablename_attributes_draft.xlsx} A file for 
#'     information about your data tables.
#'     
#'     \emph{eml_configuration.R} A file for information about your data 
#'     entities (i.e. data tables, .zip directories of spatial vectors, etc.).
#'     
#' @details 
#'     If template files already exist in the working directory, new templates 
#'     will not be transfered.
#'     
#' @export     
#'     
#' @seealso \code{\link{run_guide}} for guidance on completing the template 
#'     files.


copy_templates <- function(path){
  
  file.copy(from = paste(path.package("emlAssemblyLine"),
                         "/templates/eml_configuration.R",
                         sep = ""),
            to = path)
  
  file.copy(from = paste(path.package("emlAssemblyLine"),
                         "/templates/datasetname_abstract.txt",
                         sep = ""),
            to = path)
  
  file.copy(from = paste(path.package("emlAssemblyLine"),
                         "/templates/datasetname_custom_units.txt",
                         sep = ""),
            to = path)
  
  file.copy(from = paste(path.package("emlAssemblyLine"),
                         "/templates/datasetname_cc0_1_intellectual_rights.txt",
                         sep = ""),
            to = path)
  
  file.copy(from = paste(path.package("emlAssemblyLine"),
                         "/templates/datasetname_cc_by_4.0_intellectual_rights.txt",
                         sep = ""),
            to = path)
  
  file.copy(from = paste(path.package("emlAssemblyLine"),
                         "/templates/datasetname_datatablename_attributes_draft.txt",
                         sep = ""),
            to = path)
  
  file.copy(from = paste(path.package("emlAssemblyLine"),
                         "/templates/datasetname_methods.txt",
                         sep = ""),
            to = path)
  
  file.copy(from = paste(path.package("emlAssemblyLine"),
                         "/templates/datasetname_personnel.txt",
                         sep = ""),
            to = path)
  
  file.copy(from = paste(path.package("emlAssemblyLine"),
                         "/templates/datasetname_additional_info.txt",
                         sep = ""),
            to = path)
  
  
}
