#' Describe categorical variables of an attributes template
#'
#' Describes categorical variables of an attributes template. Use if any 
#' attributes are classified as "categorical" in the table attributes template.
#'
#' @param path (character) Path to the metadata template directory.
#' @param data.path (character) Path to the data directory.
#' @param empty (logical) Whether to create an empty template.
#' @param write.file (logical) Whether to write the template to file. If 
#' \code{FALSE}, a list of data frames will be returned.
#'
#' @return 
#' Tables (one for each attributes template containing categorical classes), as 
#' tab delimited files if \code{write.file = TRUE}, or a list of data frames if 
#' \code{write.file = FALSE}. Table columns:
#' \itemize{
#' \item{attributeName: Column name}
#' \item{code: Categorical variable}
#' \item{definition: Definition of categorical variable}
#' }
#'     
#' @details \code{template_categorical_variables()} knows which attributes of a 
#' data object are \code{categorical} based on their definition under the 
#' \code{class} column of the attributes template. If any categorical variables
#' are found, this function reads the corresponding data object (if supported)
#' and returns a list of the categorical variables and the unique categorical 
#' codes.
#' 
#' @note Currently, this function is unable to extract metadata from data 
#' objects that are not recognized as data tables. As a result, for these data 
#' types the template will be returned empty. For guidance on manually 
#' completing the template, see documentation on 
#' \code{template_categorical_variables()} and the associated vignette on 
#' editing templates.
#'
#' @examples 
#' \dontrun{
#' # Set working directory
#' setwd("/Users/me/Documents/data_packages/pkg_260")
#' 
#' # For tables containing categorical variables as classified in the 
#' # attributes template
#' template_categorical_variables(
#'   path = "./metadata_templates",
#'   data.path = "./data_objects"
#' )
#' }
#' 
#' @export
#'
template_categorical_variables <- function(
  path, 
  data.path = path, 
  empty = FALSE,
  write.file = TRUE) {
  
  message('Templating categorical variables')
  
  validate_arguments(
    fun.name = 'template_categorical_variables',
    fun.args = as.list(environment())
  )

  # Read templates, then use these to identify the list of data objects and 
  # read these in.
  x <- template_arguments(path, data.path)$x
  attr_files <- list_attribute_templates(path)
  data_objects <- name_data_objects(attr_files, data.path)
  x <- template_arguments(
    path = path,
    data.path = data.path,
    data.objects = data_objects
  )$x
  x <- remove_empty_templates(x)
  
  # If there are no data.objects, there is no more work to be done.
  if (is.null(x$data.objects)) {
    return(NULL)
  }
  
  templates <- names(x$template)
  res <- vector(mode = "list", length = length(x$data.objects))
  # Iterate over the data objects and extract categorical variable codes when
  # possible.
  for (i in seq_along(x$data.objects)) {
    catvars_file <- name_catvars_templates(names(x$data.objects)[i])
    # Don't overwrite existing categorical variables templates.
    if (is.element(catvars_file, templates)) {
      message(catvars_file, " already exists!")
      next
    }
    # An attributes template, corresponding with the current data object, is 
    # required for continued processing.
    attrs_file <- name_attributes_templates(names(x$data.objects)[i])
    if (!is.element(attrs_file, templates)) {
      next
    }
    # Try extracting categorical variables from the current data object using 
    # one of the supported parsing algorithms, indicated by MIME type.
    mime_type <- x$data.objects[[i]]$mime_type
    if (mime_type == "text/csv") {
      res[[i]] <- catvars_from_textcsv(
        data_object = x$data.objects[[i]]$content, 
        attrs_tmplt = x$template[[attrs_file]]$content
      )
    }
    #   - Initialize catvars template (similar to init_attributes())
    #   - iterate over categoricals extracting unique values
    #   - drop missing value codes
  }
  # TODO remove NULLs
  
  r <- lapply(
    seq_along(data_tables),
    function(i, data_tables) {
      
      # Get components
      
      # TODO Should this be preserved?
      # Read cols as char to prevent data.table::fread() parsing numeric "" to 
      # NA which cannot be converted back to "" before writing the template.
      # d <- data.table::fread(
      #   paste0(data.path, "/", data_tables[i]), 
      #   colClasses = "character")
      
      # attributes <- x$template[[names(data_tables)[i]]]$content
      # Do not continue unless data and attributes have made it this far
      # if (is.null(d) | is.null(attributes)) {
      #   return(NULL)
      # }
      
      categorical_variables <- attributes$attributeName[
        attributes$class == "categorical"]
      
      missing_value_codes <- dplyr::select(
        attributes, attributeName, missingValueCode)
      
      categorical_variables_file_name <- stringr::str_replace(
        names(data_tables)[i], 
        "attributes_", 
        "catvars_")
      
      # Continue if categorical variables exist for this data table and if
      # a categorical variables template doesn't already exist
      
      if (length(categorical_variables) == 0) {
        message("No categorical variables found.")
      } else {
        
        if (categorical_variables_file_name %in% names(x$template)) {
          message(categorical_variables_file_name, " already exists!")
        } else {
          message(categorical_variables_file_name)
          
          # Compile components for the categorical variables template
          
          catvars <- dplyr::select(d, categorical_variables)
          catvars <- tidyr::gather(catvars, "attributeName", "code")
          catvars <- dplyr::distinct(catvars)
          catvars <- dplyr::right_join(missing_value_codes, catvars, by = "attributeName")
          
          # Remove missing value codes listed in the table attributes template 
          # since these will be listed in the EML metadata separately. NOTE: 
          # Because EAL templates use "" instead of NA, all "" from the template
          # are converted to NA to facilitate matching.
          
          use_i <- apply(
            catvars, 
            1, 
            function(x) {
              if (x[["missingValueCode"]] == "NA") {
                x[["missingValueCode"]] <- NA_character_
              }
              missing_value_code <- x[["missingValueCode"]] %in% x[["code"]]
              return(missing_value_code)
            })
          
          catvars <- catvars[!use_i, ]
          
          # Tranform contents into the categorical variables template format
          
          catvars$definition <- ""
          catvars <- dplyr::select(catvars, -missingValueCode)
          
          # Order results
          
          catvars <- dplyr::arrange(catvars, attributeName, code)
          
          # Encode extracted metadata in UTF-8
          
          catvars$attributeName <- enc2utf8(as.character(catvars$attributeName))
          catvars$code <- enc2utf8(as.character(catvars$code))
          
          # List under "content" to accord with structure returned by 
          # template_arguments()
          
          catvars <- list(content = catvars)
          
          return(catvars)
          
        }
      }
    },
    data_tables = data_tables)
  
  names(r) <- stringr::str_replace(
    names(data_tables), 
    "attributes_", 
    "catvars_")
  
  # Write to file -------------------------------------------------------------
  
  if (write.file) {
    for (i in names(r)) {
      if (!is.null(r[[i]])) {
        data.table::fwrite(
          x = r[[i]]$content,
          file = paste0(path, "/", enc2utf8(i)),
          sep = "\t",
          quote = FALSE,
          na = "NA")
      }
    }
  }
  
  message("Done.")
  
  # Return --------------------------------------------------------------------

  return(r)
}


#' Create a categorical variables template for a MIME Type text/csv data object
#'
#' @param data_object (data.frame) The data object.
#' @param attrs_tmplt (data.frame) The attributes template corresponding to 
#' \code{data_object}.
#'
#' @return (data.frame or NULL) Returns a \code{data.frame} if categorical variables 
#' are listed in \code{attrs_tmplt} corresponding categorical codes are found
#' in \code{data_object}, otherwise \code{NULL}.
#' 
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' 
#' }
#' 
catvars_from_textcsv <- function(data_object, attrs_tmplt) {
  # If no categorical variables are declared in the attributes template, for 
  # the current data object, then there are none to extract.
  is_categorical <- attrs_tmplt$class %in% "categorical"
  if (!any(is_categorical)) {
    return(NULL)
  }
  df <- data_object[ , which(is_categorical)]
  browser()
  test <- sapply(df, unique)
  # TODO reshape result into long data frame
  stack(test)

}
