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
#' If \code{write.file = TRUE}, tab delimited files (one for each 
#' \code{other.entity}) are written to \code{path}. If \code{write.file = FALSE}
#' a list of data frames are returned. Columns of this template:
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
#' a missing value code).}
#' \item{missingValueCodeExplanation: Definition of missing value code.}
#' }
#'     
#' @note Currently, this function is unable to extract metadata from objects 
#' listed in the \code{other.entity} parameter. As a result, the template will 
#' be returned empty. For guidance on manual completion of this template, see 
#' the vignette on editing templates.
#'     
#' @examples 
#' \dontrun{
#' # Create a temporary directory with files for this example
#' testdir <- paste0(tempdir(), "/pkg")
#' pkg_files <- copy_test_package(testdir)
#' attributes_files <- dir(testdir, pattern = "attributes", full.names = TRUE)
#' file.remove(attributes_files)
#' 
#' # Return templates as data frames
#' tmplts <- template_other_entity_attributes(
#'   path = testdir, 
#'   other.entity = c("ancillary_data.zip", "processing_and_analysis.R"),
#'   write.file = FALSE
#' )
#' 
#' # Return templates as files
#' tmplts <- template_other_entity_attributes(
#'   path = testdir, 
#'   other.entity = c("ancillary_data.zip", "processing_and_analysis.R"),
#'   write.file = TRUE
#' )
#' 
#' # Clean up files of this example
#' #unlink(testdir, force = TRUE)
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
  
  
  templates <- vector(mode = "list", length = length(other.entity))
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


