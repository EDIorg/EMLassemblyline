#' Create directories for \code{EMLassemblyline} inputs and outputs
#'
#' @description  
#'     Create a commonly used directory structure for \code{EMLassemblyline} 
#'     inputs and outputs. You're welcome to organize content in any way you 
#'     choose, this is just one approach.
#'
#' @usage template_directories(path, dir.name)
#'
#' @param path 
#'     (character) Path to where the directory structure will be created.
#' @param dir.name
#'     (character) Name of parent directory. Subdirectories have predefined 
#'     names.
#'
#' @return
#'     A directory with the following structure and contents:
#'     \itemize{
#'         \item{\strong{name} Name of parent directory}
#'         \itemize{
#'             \item{\strong{data_objects} Directory for data and other 
#'             objects to be packaged.}
#'             \item{\strong{metadata_templates} Directory for 
#'             \code{EMLassemblyline} template files.}
#'             \item{\strong{eml} Directory for EML files created by 
#'             \code{EMLassemblyline}.}
#'             \item{\strong{run_EMLassemblyline.R} An empty R file for 
#'             scripting an \code{EMLassemblyline} workflow.}
#'         }
#'     }
#'     
#' @note
#'     Existing directories named with \code{dir.name} at \code{path} will not 
#'     be overwritten.
#'
#' @examples
#' # Set working directory
#' setwd(tempdir())
#' 
#' # Template data package directory "edi_301"
#' template_directories(
#'   path = '.',
#'   dir.name =  'edi_301'
#' )
#' 
#' # View directory contents
#' dir('./edi_301')
#'
#' @export
#'

template_directories <- function(path, dir.name){
  
  # Stop if directory exists
  
  if (dir.exists(paste0(path, '/', dir.name))){
    stop(paste0(path, '/', dir.name, ' already exists.'))
  }

  # Create parent directory ---------------------------------------------------
  
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
  
  # Create subdirectories -----------------------------------------------------
  
  # Create subdirectory objects
  
  dir.create(
    path = paste0(
      path,
      '/',
      dir.name,
      '/data_objects'
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
  
  # Create EMLassemblyline R script -------------------------------------------
  
  value <- file.copy(
    from = system.file(
      '/templates/run_EMLassemblyline.R',
      package = 'EMLassemblyline'
    ),
    to = paste0(
      path,
      '/',
      dir.name,
      '/run_EMLassemblyline_for_',
      dir.name,
      '.R'
    )
  )
  
}
