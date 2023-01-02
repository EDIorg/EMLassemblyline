#' Describe other entities
#'
#' @description Describes attributes of other entity data objects.
#'
#' @param path (character) Path to the metadata template directory.
#' @param data.path (character) Path to the data directory.
#' @param other.entity (character) Other entity file name, including file 
#' extension. If more than one, supply as a vector of character strings (e.g. 
#' \code{other.entity = c("ancillary_data.zip", "processing_and_analysis.R")}).
#' @param empty (logical) Whether to create an empty template.
#' @param write.file (logical) Whether to write the template to file. If 
#' \code{FALSE}, a list of data frames will be returned.
#'
#' @return 
#' Tables (one for each \code{other.entity}), as tab delimited files if 
#' \code{write.file = TRUE}, or a list of data frames if 
#' \code{write.file = FALSE}. Table columns:
#' \itemize{
#' \item{attributeName: Column name}
#' \item{attributeDefinition: Column definition}
#' \item{class: Column class. Valid options are: "numeric" (Numeric variable), 
#' "categorical" (Categorical variable, i.e. nominal), "character" (Free text 
#' character strings, e.g. notes), "Date" (Date and time variable)}
#' \item{unit: Column unit. Required for numeric classes. Select from EML's 
#' standard unit dictionary, accessible with \code{view_unit_dictionary()}. 
#' Use values in the "id" column. If not found, then define as a custom unit 
#' (see custom_units.txt).}
#' \item{dateTimeFormatString: Format string. Required for Date classes. Valid 
#' format string components are: "Y" (Year), "M" (Month), "D" (Day), "h" 
#' (Hour), "m" (Minute), "s" (Second), Common separators of format string 
#' components (e.g. "-" "/" "\" ":"") are supported.}
#' \item{missingValueCode: Missing value code. Required for columns containing 
#' 
#' a missing value code).}
#' \item{missingValueCodeExplanation: Definition of missing value code.}
#' }
#'     
#' @note Currently, this function is unable to extract metadata from objects 
#' listed in the \code{other.entity} parameter. As a result, the template will 
#' be returned empty. For guidance on manually completing the template, see 
#' documentation on \code{template_table_attributes()} and the associated 
#' vignette on editing templates.
#'     
#' @examples 
#' \dontrun{
#' # Set working directory
#' setwd("/Users/me/Documents/data_packages/pkg_260")
#' 
#' # For 2 other entities
#' template_other_entity_attributes(
#'   path = './metadata_templates',
#'   other.entity = c("ancillary_data.zip", "processing_and_analysis.R"),
#'   empty = TRUE
#' )
#' }
#'     
#' @export     
#'     
template_other_entity_attributes <- function(
    path,
    data.path = path,
    other.entity = NULL,
    empty = FALSE,
    write.file = TRUE) {
  
  message("Templating other entity attributes")
  
  if (missing(path)) {
    path <- NULL
  }
  
  # TODO Validate arguments
  
  if (is.null(other.entity)) {
    return(NULL)
  } else {
    # TODO Read data objects for metadata extraction.
  }
  
  templates <- list()
  for (i in seq_along(other.entity)) {
    templates[[i]] <- init_attributes()
  }
  f <- name_attributes_templates(other.entity)
  names(templates) <- f
  if (write.file) {
    for (i in seq_along(templates)) {
      invisible(
        write_template(
          tmplt = templates[[i]], 
          name = names(templates)[i], 
          path = path
        )
      )
    }
  } else {
    return(templates)
  }
}


