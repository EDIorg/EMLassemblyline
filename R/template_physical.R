#' Describe physical attributes of data objects
#' 
#' @description This is an optional template that can be used to define the 
#' physical attributes of data objects. The \code{make_eml()} function will 
#' automatically calculate these values if the physical template is absent or 
#' empty.
#' 
#' Use this template to define physical attributes of remote data that cannot 
#' be accessed by \code{make_eml()}.
#'
#' @param path 
#'     (character) Path to the metadata template directory.
#' @param data.path
#'     (character) Path to the data directory.
#' @param data.table
#'     (character; optional) Table file name. If more than one, then supply 
#'     as a vector of character strings (e.g. 
#'     \code{data.table = c("nitrogen.csv", "decomp.csv")}).
#' @param other.entity
#'     (character; optional) Name of \code{other.entity}(s) in this 
#'     dataset. Use \code{other.entity} for all non-\code{data.table} files. 
#'     \code{other.entity}(s) should be stored at \code{data.path}. If more 
#'     than one, then supply as a vector of character strings (e.g. 
#'     \code{other.entity = c('ancillary_data.zip', 'quality_control.R')}).
#' @param empty
#'     (logical) Whether to write an empty template file.
#' @param write.file
#'     (logical; optional) Whether to write the template file. Default is \code{TRUE}.
#'
#' @return
#' \item{physical}{Columns:
#'     \itemize{
#'     \item{type: Object type. Can be: dataTable, otherEntity.}
#'     \item{entityName: A short descriptive name for the object.}
#'     \item{entityDescription: Description of the object.}
#'     \item{objectName: File name of the object.}
#'     \item{size: File size in bytes}
#'     \item{authentication: File hash value.}
#'     \item{authentication_method: Method of calculating the file has value. Defaults to "MD5"}
#'     \item{numHeaderLines: Number of header lines. Defaults to 1.}
#'     \item{recordDelimiter: Record delimiter (i.e. newline character).}
#'     \item{attributeOrientation: Can be "column" or "row". Defaults to "column".}
#'     \item{fieldDelimiter: Field delimiter.}
#'     \item{quoteCharacter: Quote character used to enclose character type data.}
#'     \item{formatName: Format of data object. Only for non-tabular data.}
#'     \item{url: The publicly accessible URL from which the data object can be downloaded.}
#'     \item{numberOfRecords: Number of records in the data object}
#'     }
#' }
#'     
#' @details \code{make_eml()} will calculate values for empty fields and ignore
#' fields containing NA. Note, some fields only apply to one object type.
#'     
#'
#' @examples 
#' \dontrun{
#' 
#' }
#'
#' @export
#'
template_physical <- function(
  path, 
  data.path = path, 
  data.table = NULL, 
  other.entity = NULL,
  empty = FALSE, 
  write.file = TRUE) {
  
  message('Templating physical attributes for data objects ...')
  
  # TODO Validate arguments
  
  if (file.exists(paste0(path, "/", "physical.txt"))) {
    message("physical.txt already exists!")
    return(NULL)
  }
  
  # Create empty template -----------------------------------------------------
  
  # TODO Add "empty" type to get_physical() to return an empty ... or read blank from /inst
  res <- data.frame(
        type = character(0),
        entityName = character(0),
        entityDescription = character(0),
        objectName = character(0),
        size = character(0),
        authentication = character(0),
        authentication_method = character(0),
        numHeaderLines = character(0),
        recordDelimiter = character(0),
        attributeOrientation = character(0),
        fieldDelimiter = character(0),
        quoteCharacter = character(0),
        formatName = character(0),
        url = character(0),
        numberOfRecords = character(0),
        stringsAsFactors = FALSE
  )
  
  # Return empty --------------------------------------------------------------
  
  if (empty) {
    if (write.file){
      suppressWarnings(
        utils::write.table(
          res,
          paste0(path, "/", "physical.txt"),
          sep = "\t",
          row.names = F,
          quote = F,
          fileEncoding = "UTF-8"
        )
      )
    }
    return(res)
  }
  
  # Process objects -----------------------------------------------------------
  
  if (!is.null(data.table)) {
    data.table <- lapply(
      paste0(data.path, "/", data.table),
      get_physical,
      type = "dataTable"
    )
  }
  
  if (!is.null(other.entity)) {
    other.entity <- lapply(
      paste0(data.path, "/", other.entity),
      get_physical,
      type = "otherEntity"
    )
  }
  
  res <- do.call(rbind, c(data.table, other.entity))
  
  # Return --------------------------------------------------------------------
  
  if (write.file){
    suppressWarnings(
      utils::write.table(
        res,
        paste0(path, "/", "physical.txt"),
        sep = "\t",
        row.names = F,
        quote = F,
        fileEncoding = "UTF-8"
      )
    )
  }
  message("Done.")
  return(res)
}






#' Get physical metadata for a data object
#'
#' @param path (character) Full path to file
#' @param type (character) Object type. Can be: "dataTable", "otherEntity"
#'
#' @return (data.frame) One row data frame with columns of the physical 
#' attributes template
#' @noRd
#'
get_physical <- function(path, type) {
  eml <- suppressWarnings(suppressMessages(EML::set_physical(path)))
  res <- list()
  res$type <- type
  res$entityName <- ""
  res$entityDescription <- ""
  res$objectName <- basename(path)
  res$size <- eml$size$size
  res$authentication <- eml$authentication$authentication
  res$authentication_method <- eml$authentication$method
  if (type == "dataTable") {
    res$numHeaderLines <- "1"
    res$recordDelimiter <- get_eol(dirname(path), basename(path))
    res$attributeOrientation <- "column"
    fdlim <- detect_delimeter(dirname(path), basename(path), detect_os())
    if (fdlim == "\t") {
      fdlim <- "\\t" # requires escape char to be written, otherwise is blank
    }
    res$fieldDelimiter <- fdlim
    res$quoteCharacter <- NA_character_
    res$formatName <- NA_character_
    res$url <- NA_character_
    x <- template_arguments(
      data.path = dirname(path), 
      data.table = basename(path)
    )$x
    res$numberOfRecords <- as.character(nrow(x$data.table[[1]]$content))
  } else if (type == "otherEntity") {
    res$numHeaderLines <- NA_character_
    res$recordDelimiter <- NA_character_
    res$attributeOrientation <- NA_character_
    res$fieldDelimiter <- NA_character_
    res$quoteCharacter <- NA_character_
    res$formatName <- mime::guess_type(
      file = basename(path), 
      unknown = "Unknown", 
      empty = "Unknown"
    )
    res$url <- NA_character_
    res$numberOfRecords <- NA_character_
  }
  return(as.data.frame(res))
}

















