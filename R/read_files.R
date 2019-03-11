#' Read templates and data files into list (alternative input to 
#' `EMLassemblyline` functions)
#'
#' @description  
#'     This function is under development.
#'
#' @usage read_file_list(path, data.path = path, data.table = NULL, 
#' other.entity = NULL)
#'
#' @param path 
#'     (character) Path to where the template(s) will be imported.
#' @param data.path
#'     (character) Path to where the data tables are stored.
#' @param data.table
#'     (character) Data table name(s). If more than one, then supply as a 
#'     vector of character strings (e.g. 
#'     `data.table = c("concentrations.csv", "characteristics.csv")`).
#' @param other.entity
#'     (character) Name(s) of `other.entity`(s) in this dataset. Use
#'     `other.entity` for all non-`data.table` files. `other.entity`(s) should 
#'     be stored at `data.path`.
#'
#' @return 
#'     (named list) Named list containing these elements
#'     \itemize{
#'         \item{`name` Name of file}
#'         \item{`path` Where the file was read from}
#'         \item{`contents` Contents of the file (e.g. data.frame, XML, etc.)}
#'     }
#'     
#' @details 
#'     Salient details of this function
#'

read_files <- function(path){
  
  if (!is.null(data.path)){
    
    sep <- EDIutils::detect_delimeter(
      path = data.path,
      data.files = file.name,
      os = EDIutils::detect_os()
    )
    
    x <- read.table(
      paste0(data.path, "/", file.name),
      header = T,
      sep = sep,
      as.is = T,
      quote = "\"",
      comment.char = ""
    )
    
  } else if (!is.null(package.id)){
    
    eml <- suppressMessages(
      EDIutils::api_read_metadata(package.id)
    )
    
    files <- unlist(
      xmlApply(
        eml[
          "//dataset/dataTable/entityName"
          ],
        xmlValue
      )
    )
    
    use_i <- match(file.name, files)
    
    sep <- unlist(
      xmlApply(
        eml[
          "//dataset/dataTable/physical/dataFormat/textFormat/simpleDelimited/fieldDelimiter"
          ],
        xmlValue
      )
    )[use_i]
    
    data_url <- c(
      unlist(
        xmlApply(
          eml["//dataset/dataTable/physical/distribution/online/url"], 
          xmlValue
        )
      )
    )[use_i]
    
    x <- read.table(
      data_url,
      header = T,
      sep = sep,
      as.is = T,
      quote = "\"",
      comment.char = ""
    )
    
  }
  
  list(data = x, fname = file.name)
  
}