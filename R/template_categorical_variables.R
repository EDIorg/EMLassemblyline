#' Describes categorical variables of data objects
#'
#' Use this function if data objects contain any attributes classified
#' "categorical" in an attributes template.
#'
#' @param path (character) Path to the metadata template directory.
#' @param data.path (character) Path to the data directory.
#' @param empty (logical) Whether to create an empty template.
#' @param write.file (logical) Whether to write the template to file. If
#' \code{FALSE}, a list of data frames will be returned.
#'
#' @return
#' If \code{write.file = TRUE}, tab delimited files (one for each attributes
#' template containing categorical classes) are written to \code{path}. If
#' \code{write.file = FALSE} a list of data frames are returned. Columns of
#' this template:
#' \itemize{
#' \item{attributeName: Column name}
#' \item{code: Categorical variable}
#' \item{definition: Definition of categorical variable}
#' }
#'
#' @details \code{template_categorical_variables()} knows which attributes of a
#' data object are \code{categorical} based on their definition under the
#' \code{class} column of the attributes template. If any categorical variables
#' are found, this function reads the corresponding data object (when
#' supported) and returns a list of the categorical variables and their unique
#' codes.
#'
#' @note Currently, the only supported data object type is "text/csv". For all
#' other types, returned templates will be empty. For guidance on manual
#' completion of this template, see the vignette on editing templates.
#'
#' @examples
#' \dontrun{
#' # Create a temporary directory with files for this example
#' testdir <- paste0(tempdir(), "/pkg")
#' pkg_files <- copy_test_package(testdir)
#' catvars_files <- dir(testdir, pattern = "catvars", full.names = TRUE)
#' file.remove(catvars_files)
#'
#' # Return templates as data frames
#' tmplts <- template_categorical_variables(path = testdir, write.file = FALSE)
#'
#' # Return templates as files
#' template_categorical_variables(path = testdir, write.file = TRUE)
#'
#' # Clean up files of this example
#' unlink(testdir, force = TRUE)#' }
#'
#' @export
#'
template_categorical_variables <- function(
  path, 
  data.path = path,
  empty = FALSE,
  write.file = TRUE) {
  
  message("Templating categorical variables")
  validate_arguments(
    fun.name = 'template_categorical_variables',
    fun.args = as.list(environment())
  )
  
  # Data objects, relevant to the categorical variable extraction process, are
  # indirectly specified by the presence of an attributes template. Read the
  # attributes templates and then the corresponding data objects.
  x <- template_arguments(path, data.path)$x
  attr_files <- list_attribute_templates(path)
  data_objects <- name_data_objects(attr_files, data.path)  
  x <- template_arguments(
    path = path,
    data.path = data.path,
    data.objects = data_objects
  )$x
  if (is.null(x$data.objects)) {
    return(NULL)
  }

  x <- remove_empty_templates(x)

  # Iterate over the data objects, extract categorical variables and their
  # unique codes when a supported parser exists. MIME types indicate which
  # parser to use.
  templates <- vector(mode = "list", length = length(x$data.objects))
  names(templates) <- name_catvars_templates(names(x$data.objects))
  for (i in names(x$data.objects)) {
    attributes_file <- name_attributes_templates(i)
    mime_type <- x$data.objects[[i]]$mime_type
    eml_type <- x$data.objects[[i]]$eml_type
    if (mime_type == "text/csv" | mime_type == "text/tab-separated-values") {
      templates[[name_catvars_templates(i)]] <- catvars_from_textcsv_tsv(
        data_object = x$data.objects[[i]]$content,
        attrs_tmplt = x$template[[attributes_file]]$content,
        empty = empty
      )
    } else if (mime_type == "application/x-netcdf") {
      templates[[name_catvars_templates(i)]] <- catvars_from_netcdf(
        data_path = x$data.objects[[i]]$file_path,
        attrs_tmplt = x$template[[attributes_file]]$content,
        empty = empty
      )
    } else if (eml_type == "spatialRaster") {
      templates[[name_catvars_templates(i)]] <- catvars_from_raster(
        data_object = x$data.objects[[i]]$content,
        attrs_tmplt = x$template[[attributes_file]]$content,
        empty = empty | mime_type == "image/tiff"
      )
    } else if (eml_type == "spatialVector") {
      templates[[name_catvars_templates(i)]] <- catvars_from_vector(
        data_object = x$data.objects[[i]]$content,
        attrs_tmplt = x$template[[attributes_file]]$content,
        empty = empty
      )
    } else {
      templates[[name_catvars_templates(i)]] <- init_catvars()
    }
  }

  if (write.file) {
    write_templates(templates, names(templates), path)
  } else {
    return(templates)
  }
}


#' Create a categorical variables template for a MIME Type text/csv or
#' text/tab-separated-values data object
#'
#' @param data_object (data.frame) The data object.
#' @param attrs_tmplt (data.frame) The attributes template corresponding to
#' \code{data_object}.
#'
#' @return (data.frame or NULL) Returns a \code{data.frame} if categorical
#' variables are listed in \code{attrs_tmplt} corresponding categorical codes
#' are found in \code{data_object}, otherwise \code{NULL}.
#'
#' @keywords internal
#'
catvars_from_textcsv_tsv <- function(data_object, attrs_tmplt, empty = FALSE) {
  # If no categorical variables are declared in the attributes template, for
  # the current data object, then there are none to extract.
  is_categorical <- attrs_tmplt$class %in% "categorical"
  if (!any(is_categorical)) {
    return(NULL)
  }

  # Return an empty template if instructed.
  if (empty) {
    df <- init_catvars()
    return(df)
  }

  # Get categorical variables into a data frame for further manipulation.
  catvars <- data_object[, which(is_categorical), drop = FALSE]

  # Remove missing value codes from the categorical variables as declared in
  # the attributes template (these will be listed separately in the EML). This
  # matching requires comparison of column values against a missing value codes
  # represented as character type, so as a precaution, each column is first
  # coerced to character.
  catvars <- as.data.frame(lapply(catvars, as.character))
  for (catvar in names(catvars)) {
    i_catvar <- attrs_tmplt$attributeName == catvar
    mv_code <- attrs_tmplt$missingValueCode[i_catvar]
    is_mv_code <- catvars[[catvar]] == mv_code
    catvars[[catvar]][is_mv_code] <- NA_character_
  }

  # Continue shaping the data frame into the catvars template
  univals <- lapply(X = catvars, FUN = unique)
  catvars <- stack(univals)
  catvars <- as.data.frame(lapply(catvars, as.character)) # undo factors created by stack()
  colnames(catvars)[colnames(catvars) == "ind"] <- "attributeName"
  colnames(catvars)[colnames(catvars) == "values"] <- "code"
  catvars$definition <- ""
  catvars <- catvars[, c("attributeName", "code", "definition")]

  # Finish removal of missing value codes (now represented as NA)
  catvars <- catvars[complete.cases(catvars), ]
  return(catvars)
}


#' Create a categorical variables template for a MIME Type application/x-netcdf
#' data object
#'
#' @param data_path (character) Path to the data object.
#' @param attrs_tmplt (data.frame) The attributes template corresponding to
#' the data object.
#' @param empty (logical) Whether to create an empty template.
#'
#' @return (data.frame or NULL) Returns a \code{data.frame} if categorical
#' variables are listed in \code{attrs_tmplt} corresponding categorical codes
#' are found in the data object, otherwise \code{NULL}.
#'
#' @keywords internal
#'
catvars_from_netcdf <- function(data_path, attrs_tmplt, empty = FALSE) {
  # If no categorical variables are declared in the attributes template, for
  # the current data object, then there are none to extract.
  is_categorical <- attrs_tmplt$class %in% "categorical"
  if (!any(is_categorical)) {
    return(NULL)
  }

  # Return an empty template if instructed.
  if (empty) {
    df <- init_catvars()
    return(df)
  }

  catvars_names <- attrs_tmplt$attributeName[is_categorical]
  nc <- ncdf4::nc_open(data_path)
  
  catvars <- data.frame()
  
  for (catvar in catvars_names) {
    if (catvar %in% names(nc$var)) {
      values <- ncdf4::ncvar_get(nc, catvar)
    } else if (catvar %in% names(nc$dim)) {
      values <- nc$dim[[catvar]]$vals
    }
    
    # Remove missing value codes from the categorical variables as declared in
    # the attributes template (these will be listed separately in the EML). This
    # matching requires comparison of column values against a missing value codes
    # represented as character type, so as a precaution, each column is first
    # coerced to character.
    values <- as.data.frame(as.character(values))
    names(values) <- catvar
    mv_code <- attrs_tmplt$missingValueCode[attrs_tmplt$attributeName == catvar]
    is_mv_code <- values[[catvar]] == mv_code
    values[[catvar]][is_mv_code] <- NA_character_
    
    univals <- unique(values)
    # Stack all catvars in the same data frame
    catvars <- rbind(catvars, stack(univals))
  }
  # Close NetCDf file
  ncdf4::nc_close(nc)
  
  # Continue shaping the data frame into the catvars template
  catvars <- as.data.frame(lapply(catvars, as.character))
  colnames(catvars)[colnames(catvars) == "ind"] <- "attributeName"
  colnames(catvars)[colnames(catvars) == "values"] <- "code"
  catvars$definition <- ""
  catvars <- catvars[, c("attributeName", "code", "definition")]

  # Finish removal of missing value codes (now represented as NA)
  catvars <- catvars[complete.cases(catvars), ]
  
  return(catvars)
}


#' Create a categorical variables template for a vector data object
#'
#' @param data_object (data.frame) The data object.
#' @param attrs_tmplt (data.frame) The attributes template corresponding to
#' \code{data_object}.
#'
#' @return (data.frame or NULL) Returns a \code{data.frame} if categorical
#' variables are listed in \code{attrs_tmplt} corresponding categorical codes
#' are found in \code{data_object}, otherwise \code{NULL}.
#'
#' @keywords internal
#'
catvars_from_vector <- function(data_object, attrs_tmplt, empty = FALSE) {
  # If no categorical variables are declared in the attributes template, for
  # the current data object, then there are none to extract.
  is_categorical <- attrs_tmplt$class %in% "categorical"
  if (!any(is_categorical)) {
    return(NULL)
  }
  
  # Return an empty template if instructed.
  if (empty) {
    df <- init_catvars()
    return(df)
  }
  
  # Get categorical variables into a data frame for further manipulation.
  catvars <- data_object[, which(is_categorical), drop = FALSE]
  
  # Remove missing value codes from the categorical variables as declared in
  # the attributes template (these will be listed separately in the EML). This
  # matching requires comparison of column values against a missing value codes
  # represented as character type, so as a precaution, each column is first
  # coerced to character.
  catvars <- as.data.frame(lapply(catvars, as.character))
  for (catvar in names(catvars)) {
    i_catvar <- attrs_tmplt$attributeName == catvar
    mv_code <- attrs_tmplt$missingValueCode[i_catvar]
    is_mv_code <- catvars[[catvar]] == mv_code
    catvars[[catvar]][is_mv_code] <- NA_character_
  }
  
  # Continue shaping the data frame into the catvars template
  univals <- lapply(X = catvars, FUN = unique)
  catvars <- stack(univals)
  catvars <- as.data.frame(lapply(catvars, as.character)) # undo factors created by stack()
  colnames(catvars)[colnames(catvars) == "ind"] <- "attributeName"
  colnames(catvars)[colnames(catvars) == "values"] <- "code"
  catvars$definition <- ""
  catvars <- catvars[, c("attributeName", "code", "definition")]
  
  # Finish removal of missing value codes (now represented as NA)
  catvars <- catvars[complete.cases(catvars), ]
  return(catvars)
}


#' Create a categorical variables template for a raster data object
#'
#' @param data_object (data.frame) The data object.
#' @param attrs_tmplt (data.frame) The attributes template corresponding to
#' \code{data_object}.
#'
#' @return (data.frame or NULL) Returns a \code{data.frame} if categorical
#' variables are listed in \code{attrs_tmplt} corresponding categorical codes
#' are found in \code{data_object}, otherwise \code{NULL}.
#'
#' @keywords internal
#'
catvars_from_raster <- function(data_object, attrs_tmplt, empty = FALSE) {
  # If no categorical variables are declared in the attributes template, for
  # the current data object, then there are none to extract.
  is_categorical <- attrs_tmplt$class %in% "categorical"
  if (!any(is_categorical)) {
    return(NULL)
  }
  
  # Return an empty template if instructed.
  if (empty) {
    df <- init_catvars()
    return(df)
  }
  
  # Get categorical variables into a data frame for further manipulation.
  catvars <- data_object[, which(is_categorical), drop = FALSE]
  
  # Remove missing value codes from the categorical variables as declared in
  # the attributes template (these will be listed separately in the EML). This
  # matching requires comparison of column values against a missing value codes
  # represented as character type, so as a precaution, each column is first
  # coerced to character.
  catvars <- as.data.frame(lapply(catvars, as.character))
  for (catvar in names(catvars)) {
    i_catvar <- attrs_tmplt$attributeName == catvar
    mv_code <- attrs_tmplt$missingValueCode[i_catvar]
    is_mv_code <- catvars[[catvar]] == mv_code
    catvars[[catvar]][is_mv_code] <- NA_character_
  }
  
  # Continue shaping the data frame into the catvars template
  univals <- lapply(X = catvars, FUN = unique)
  catvars <- stack(univals)
  catvars <- as.data.frame(lapply(catvars, as.character)) # undo factors created by stack()
  colnames(catvars)[colnames(catvars) == "ind"] <- "attributeName"
  colnames(catvars)[colnames(catvars) == "values"] <- "code"
  catvars$definition <- ""
  catvars <- catvars[, c("attributeName", "code", "definition")]
  
  # Finish removal of missing value codes (now represented as NA)
  catvars <- catvars[complete.cases(catvars), ]
  return(catvars)
}
