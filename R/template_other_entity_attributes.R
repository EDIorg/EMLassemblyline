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
    other.entity,
    empty = FALSE,
    write.file = TRUE) {
  
  message("Templating other entity attributes")
  # TODO Validate arguments
  x <- template_arguments(
    path = path,
    data.path = data.path,
    data.objects = other.entity
  )$x
  
  templates <- vector(mode = "list", length = length(x$data.objects))
  names(templates) <- name_attributes_templates(names(x$data.objects))
  for (i in seq_along(x$data.objects)) {
    mime_type <- x$data.objects[[i]]$mime_type
    if (mime_type == "text/csv") {
      # TODO message "Describe as data table instead."
    } else {
      templates[[i]] <- init_attributes()
    }
  }
  
  # Import custom_units.txt ---------------------------------------------------
  
  # If writing to file ...
  
  if (isTRUE(write.file)){
    
    # Write to path
    
    value <- file.copy(
      from = system.file(
        '/templates/custom_units.txt',
        package = 'EMLassemblyline'
      ),
      to = paste0(
        path,
        "/custom_units.txt"
      )
    )
    
    # Send message
    
    if (isTRUE(value)){
      message("custom_units.txt")
    } else {
      message("custom_units.txt already exists!")
    }
  }
  
  if (write.file) {
    write_templates(templates, names(templates), path)
  } else {
    return(templates)
  }
}


