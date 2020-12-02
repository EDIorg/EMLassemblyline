#' Create categorical variables template
#'
#' @description  
#'     Use this function after table attributes templates are complete. It uses information in attribute templates to extract the unique categorical variables from the corresponding data table.
#'     table and return for user supplied definitions. 
#'     \href{https://ediorg.github.io/EMLassemblyline/articles/edit_metadata_templates.html}{Instructions for editing this template.}
#'
#' @param path 
#'     (character) Path to the metadata template directory.
#' @param data.path
#'     (character) Path to the data directory.
#' @param write.file
#'     (logical; optional) Whether to write the template file.
#'
#' @return 
#' \item{catvars_tablename.txt}{The tab delimited categorical variable template, where "tablename" is the table name from which the variables were extracted. This file is written to \code{path}}.
#' \item{list of data frames}{A list of data frames. One for each categorical variables template.}
#'     
#' @details 
#'     \code{template_categorical_variables()} knows which columns of a table
#'     are \code{categorical} based on their definition under the \code{class} 
#'     column of the attributes_*.txt template.
#'     
#'     An existing categorical variables template will not be overwritten 
#'     by subsequent calls to \code{template_categorical_variables()}.
#'     
#'     Character encoding of metadata extracted directly from the tables are 
#'     converted to UTF-8 via \code{enc2utf8()}.
#'
#' @examples 
#' # Initialize data package directory for template_categorical_variables()
#' file.copy(
#'   from = system.file('/examples/pkg_255', package = 'EMLassemblyline'),
#'   to = tempdir(),
#'   recursive = TRUE
#' )
#' 
#' # Set working directory
#' setwd(paste0(tempdir(), '/pkg_255'))
#' 
#' # Template categorical variables
#' catvars <- template_categorical_variables(
#'   path = './metadata_templates',
#'   data.path = './data_objects')
#' catvars
#' 
#' # View directory contents (NOTE: catvars_*.txt files exist)
#' dir('./metadata_templates')
#' 
#' # Clean up
#' unlink('.', recursive = TRUE)
#'
#' @export
#'
template_categorical_variables <- function(
  path, 
  data.path = path, 
  write.file = TRUE) {
  
  message('Templating categorical variables ...')
  
  # Validate arguments --------------------------------------------------------
  
  validate_arguments(
    fun.name = 'template_categorical_variables',
    fun.args = as.list(environment()))
  
  # Read templates and data ---------------------------------------------------
  
  # Read all templates in path then parse the table attribute file names to get 
  # the corresponding data table names. Once all file names are known, re-read
  # all templates and data files.
  
  x <- template_arguments(path = path)$x
  
  attribute_template_names <- stringr::str_subset(
    names(x$template),
    "(?<=attributes_).*(?=\\.txt)")
  
  data_tables <- sapply(
    attribute_template_names,
    attribute_template_to_table,
    data.path = data.path)
  
  x <- template_arguments(
    path = path,
    data.path = data.path,
    data.table = data_tables)$x
  
  # Validate templates --------------------------------------------------------
  
  x <- remove_empty_templates(x)
  x <- validate_templates("template_categorical_variables", x)
  
  # Extract categorical variables ---------------------------------------------
  
  # Categorical variables are classified in each data tables attribute 
  # template. For each categorical variable found, extract unique codes, except
  # for declared missing value codes, and return in a long data frame.
  
  r <- lapply(
    seq_along(data_tables),
    function(i, data_tables) {
      
      # Get components
      
      d <- x$data.table[[data_tables[i]]]$content
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
          
          # Tranform contents into the categorical variales template format
          
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
          quote = FALSE)
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
attribute_template_to_table <- function(attributes.template, data.path) {
  table_regex <- paste0(
    "(?<!^attributes_)",
    stringr::str_extract(
      attributes.template, 
      "(?<=attributes_).*(?=\\.txt)"),
    "\\.[:alpha:]*$")
  table <- stringr::str_subset(dir(data.path), table_regex)
  return(table)
}








#' Convert data table file name to the corresponding attributes file name
#'
#' @param data.table
#'     (character) File name of data table, including file extension
#'
#' @return
#'     (character) Table attributes file name
#'     
table_to_attribute_template <- function(data.table) {
  attribute_template <- paste0(
    "attributes_",
    stringr::str_remove(data.table, "\\.[:alpha:]*"),
    ".txt")
  return(attribute_template)
}