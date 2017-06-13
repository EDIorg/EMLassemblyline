#' Import metadata templates
#'
#' @description  Create a working directory for your dataset then run this function to import metadata templates.
#'     Use these templates to 
#'     provide information about your data.
#'
#' @usage import_templates(path)
#'
#' @param path A path to the dataset working directory.
#'
#' @return 
#'     \emph{datasetname_abstract.txt} A text file for the abstract of your dataset. This file is UTF-8 formatted.
#'     
#'     \emph{datasetname_additional_info.txt} A text file for additional information 
#'     about your dataset. This file is UTF-8 formatted.
#'     
#'     \emph{datasetname_custom_units.txt} A tab delimited table for custom units used in your data 
#'     that are not defined in the standard unit dictionary. This file is UTF-8 formatted.
#'     
#'     \emph{datasetname_cc_by_4.0_intellectual_rights.txt} One of two
#'     intellectual rights licenses to consider for your dataset. Do not edit the text of this file. This file is UTF-8 formatted.
#'     
#'     \emph{datasetname_cc0_1_intellectual_rights.txt} One of two 
#'     intellectual rights licenses to consider for your dataset. Do not edit the text of this file. This file is UTF-8 formatted.
#'     
#'     \emph{datasetname_methods.txt} A text file for methods used in creating your 
#'     data. This file is UTF-8 formatted.
#'     
#'     \emph{datasetname_personnel.txt} A tab delimited table for information about personnel 
#'     associated with this dataset. This file is UTF-8 formatted.
#'     
#'     \emph{datasetname_datatablename_attributes.txt} A tab delimited table for 
#'     information about your data tables. This file is UTF-8 formatted.
#'     
#'     \emph{eml_configuration.R} A file for technical and general information about your data.
#'     
#' @details 
#'     If template files already exist in the working directory, new templates 
#'     will not be imported.
#'     
#' @export     
#'     
#' @seealso \code{\link{view_instructions}} for guidance on completing the templates.


import_templates <- function(path){
  
  value <- file.copy(from = paste(path.package("EMLassemblyline"),
                                  "/templates/eml_configuration.R",
                                  sep = ""),
                     to = path)
  
  if (isTRUE(value)){
    print("Importing eml_configuration.R ... ")
  } else {
    print("eml_configuration.R already exists ... ")
  }
  
  value <- file.copy(from = paste(path.package("EMLassemblyline"),
                                  "/templates/datasetname_abstract.txt",
                                  sep = ""),
                     to = path)
  
  if (isTRUE(value)){
    print("Importing datasetname_abstract.txt ... ")
  } else {
    print("datasetname_abstract.txt already exists ... ")
  }
  
  value <- file.copy(from = paste(path.package("EMLassemblyline"),
                                  "/templates/datasetname_custom_units.txt",
                                  sep = ""),
                     to = path)
  
  if (isTRUE(value)){
    print("Importing datasetname_custom_units.txt ... ")
  } else {
    print("datasetname_custom_units.txt already exists ... ")
  }
  
  value <- file.copy(from = paste(path.package("EMLassemblyline"),
                                  "/templates/datasetname_cc0_1_intellectual_rights.txt",
                                  sep = ""),
                     to = path)
  
  if (isTRUE(value)){
    print("Importing datasetname_cc0_1_intellectual_rights.txt ... ")
  } else {
    print("datasetname_cc0_1_intellectual_rights.txt already exists ... ")
  }
  
  value <- file.copy(from = paste(path.package("EMLassemblyline"),
                                  "/templates/datasetname_cc_by_4.0_intellectual_rights.txt",
                                  sep = ""),
                     to = path)
  
  if (isTRUE(value)){
    print("datasetname_cc_by_4.0_intellectual_rights.txt ... ")
  } else {
    print("datasetname_cc_by_4.0_intellectual_rights.txt already exists ... ")
  }
  
  value <- file.copy(from = paste(path.package("EMLassemblyline"),
                                  "/templates/datasetname_datatablename_attributes.txt",
                                  sep = ""),
                     to = path)
  
  if (isTRUE(value)){
    print("Importing datasetname_datatablename_attributes.txt ... ")
  } else {
    print("datasetname_datatablename_attributes.txt already exists ... ")
  }
  
  value <- file.copy(from = paste(path.package("EMLassemblyline"),
                                  "/templates/datasetname_methods.txt",
                                  sep = ""),
                     to = path)
  
  if (isTRUE(value)){
    print("Importing datasetname_methods.txt ... ")
  } else {
    print("datasetname_methods.txt already exists ... ")
  }
  
  value <- file.copy(from = paste(path.package("EMLassemblyline"),
                                  "/templates/datasetname_personnel.txt",
                                  sep = ""),
                     to = path)
  
  if (isTRUE(value)){
    print("Importing datasetname_personnel.txt ... ")
  } else {
    print("datasetname_personnel.txt already exists ... ")
  }
  
  value <- file.copy(from = paste(path.package("EMLassemblyline"),
                                  "/templates/datasetname_additional_info.txt",
                                  sep = ""),
                     to = path)
  
  if (isTRUE(value)){
    print("Importing datasetname_additional_info.txt ... ")
  } else {
    print("datasetname_additional_info.txt already exists ... ")
  }
  
  
}
