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
  
  # Iterate over the data objects and extract categorical variable codes when
  # possible.
  templates <- names(x$template)
  res <- vector(mode = "list", length = length(x$data.objects))
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
  }
  names(res) <- name_catvars_templates(names(x$data.objects))
  res <- Filter(Negate(is.null), res)
  
  browser()
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
  
  # Construct a data frame of the categorical variables as a basis for the 
  # catvars template.
  df <- data_object[ , which(is_categorical), drop = FALSE]
  
  # Remove missing value codes declared in the attributes template, since
  # these will be listed separately in the EML. This matching requires 
  # comparison of column values against a missing value code represented in 
  # character type, so as a precaution, each column is first coerced to 
  # character.
  df <- as.data.frame(lapply(df, as.character))
  for (colname in names(df)) {
    i <- attrs_tmplt$attributeName == colname
    mv_code <- attrs_tmplt$missingValueCode[i]
    is_mv_code <- df[[colname]] == mv_code
    df[[colname]][is_mv_code] <- NA_character_
  }
  
  # Continue shaping the catvars data frame.
  univals <- sapply(X = df, FUN = unique)
  df <- stack(univals)
  df <- as.data.frame(lapply(df, as.character))
  colnames(df)[colnames(df) == "ind"] <- "attributeName"
  colnames(df)[colnames(df) == "values"] <- "code"
  df$definition <- ""
  df <- df[, c("attributeName", "code", "definition")]
  
  # Finish removal of missing value codes (now represented as NA)
  df <- df[complete.cases(df), ]
  return(df)
}
