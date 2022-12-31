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
#' \code{class} column of the attributes template.
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
  
  # Read templates and data ---------------------------------------------------
  
  # FIXME Read all templates and data objects in a single pass.
  
  
  x <- template_arguments(path = path, data.path)$x
  
  attribute_template_names <- stringr::str_subset(
    names(x$template),
    "(?<=attributes_).*(?=\\.txt)")
  
  data_tables <- sapply(
    attribute_template_names,
    attribute_template_to_table,
    data.path = data.path)
  
  # FIXME Read all templates and data objects in a single pass.
  x <- template_arguments(
    path = path,
    data.path = data.path,
    data.table = data_tables)$x
  
  # TODO identify which objects are data tables and list under the data_tables
  # object
  
  # Validate templates --------------------------------------------------------
  
  x <- remove_empty_templates(x)
  
  # Extract categorical variables ---------------------------------------------
  
  # Categorical variables are classified in each data tables attribute 
  # template. For each categorical variable found, extract unique codes, except
  # for declared missing value codes, and return in a long data frame.
  
  r <- lapply(
    seq_along(data_tables),
    function(i, data_tables) {
      
      # Get components
      
      # Read cols as char to prevent data.table::fread() parsing numeric "" to 
      # NA which cannot be converted back to "" before writing the template.
      d <- data.table::fread(
        paste0(data.path, "/", data_tables[i]), 
        colClasses = "character")
      
      attributes <- x$template[[names(data_tables)[i]]]$content
      # Do not continue unless data and attributes have made it this far
      if (is.null(d) | is.null(attributes)) {
        return(NULL)
      }
      
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








#' Convert attributes file name to the corresponding data table file name
#'
#' @param attributes.template 
#'     (character) Table attributes template file name, including file extension
#' @param data.path
#'     (character) Path to the data directory
#'
#' @return
#'     (character) Data table file name
#' 
#' @keywords internal
#' 
attribute_template_to_table <- function(attributes.template, data.path) {
  table_regex <- paste0(
    "^(?<!^attributes_|^catvars_)",
    stringr::str_extract(
      attributes.template, 
      "(?<=attributes_).*(?=\\.txt)"),
    "\\.[:alpha:]*$")
  table <- stringr::str_subset(dir(data.path), table_regex)
  return(table)
}
