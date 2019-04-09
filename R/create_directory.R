#' Create directories for `EMLassemblyline` inputs and outputs
#'
#' @description  
#'     Create a commonly used directory structure for `EMLassemblyline` inputs 
#'     and outputs. You're welcome to organize content in any way you choose,
#'     this is just one approach.
#'
#' @usage create_directories(path, dir.name)
#'
#' @param path 
#'     (character) Path to where the directory structure will be created.
#' @param dir.name
#'     (character) Name of parent directory. Subdirectories have predefined 
#'     names.
#'
#' @return
#'     A directory with the following structure:
#'     \itemize{
#'         \item{`name` Parent directory with given name}
#'         \itemize{
#'             \item{`data` Where data and other entities are stored}
#'             \item{`scripts` Where data processing and packaging scripts are
#'             kept.}
#'             \item{`metadata_templates` Where `EMLassemblyline` template 
#'             files are imported and edited}
#'             \item{`eml` Where EML files are exported to by the 
#'             `EMLassemblyline`}
#'         }
#'     }
#'     
#' @note
#'     Existing `dir.name`s at `path` will not be overwritten.
#'
#' @export
#'

create_directories <- function(path, dir.name){
  
  # Stop if directory exists
  
  if (dir.exists(paste0(path, '/', dir.name))){
    stop(paste0(path, '/', dir.name, ' already exists.'))
  }

  # Create parent directory
  
  message(
    paste0(
      'Creating ',
      path,
      '/',
      dir.name
    )
  )
  
  dir.create(
    path = paste0(
      path,
      '/',
      dir.name
    )
  )
  
  # Create subdirectory data
  
  dir.create(
    path = paste0(
      path,
      '/',
      dir.name,
      '/data'
    )
  )
  
  # Create subdirectory eml
  
  dir.create(
    path = paste0(
      path,
      '/',
      dir.name,
      '/eml'
    )
  )
  
  # Create subdirectory metadata_templates
  
  dir.create(
    path = paste0(
      path,
      '/',
      dir.name,
      '/metadata_templates'
    )
  )
  
  # Create subdirectory scripts
  
  dir.create(
    path = paste0(
      path,
      '/',
      dir.name,
      '/scripts'
    )
  )
  
}
