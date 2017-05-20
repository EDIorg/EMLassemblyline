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
#'     datasetname_abstract.docx A file for the abstract of your data.
#'     
#'     datasetname_additional_info.docx A file for additional information about 
#'     your data.
#'     
#'     datasetname_cc_by_4.0_intellectual_rights.docx One of two intellectual 
#'     rights licenses to consider for your dataset. Do not edit this text.
#'     
#'     datasetname_cc0_1_intellectual_rights.docx One of two intellectual 
#'     rights licenses to consider for your dataset. Do not edit this text.
#'     
#'     datasetname_methods.docx A file for methods used in create your data.
#'     
#'     datasetname_personnel.xlsx A file for information on personnel 
#'     associated with this data.
#'     
#'     datatablename_attributes_draft.xlsx A file for information about your 
#'     data tables.
#'     
#'     eml_configuration.R A file for information about your data entities 
#'     (i.e. data tables, .zip directories of spatial vectors, etc.).


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
                         "/templates/datasetname_cc0_1_intellectual_rights.docx",
                         sep = ""),
            to = path)
  
  file.copy(from = paste(path.package("EMLtools"),
                         "/templates/datasetname_cc_by_4.0_intellectual_rights.docx",
                         sep = ""),
            to = path)
  
  file.copy(from = paste(path.package("EMLtools"),
                         "/templates/datatablename_attributes_draft.xlsx",
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
                         "/templates/shapefilesetname_shape_files_attributes_draft.xlsx",
                         sep = ""),
            to = path)
  
  file.copy(from = paste(path.package("EMLtools"),
                         "/templates/datasetname_additional_info.docx",
                         sep = ""),
            to = path)
  
  
}
