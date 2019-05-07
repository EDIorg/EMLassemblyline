#' Create data package directory
#'
#' @description  
#'     Create a directory structure for data package contents and 
#'     \code{EMLassemblyline} files. Any organization scheme is supported. This
#'     is one recommendation.
#'
#' @usage 
#'     template_directories(
#'       path, 
#'       dir.name
#'     )
#'
#' @param path 
#'     (character) Path to where the data package directory will be created.
#' @param dir.name
#'     (character) Name of the directory.
#'
#' @return
#'     A directory with the following structure and contents:
#'     \itemize{
#'         \item{\strong{name} Name supplied via \code{dir.name}}
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
#' @details 
#'     Existing directories named with \code{dir.name} at \code{path} will not 
#'     be overwritten.
#'
#' @examples
#' # Set working directory
#' setwd(tempdir())
#' 
#' # Template data package directory "pkg_301"
#' template_directories(
#'   path = '.',
#'   dir.name =  'pkg_301'
#' )
#' 
#' # View directory contents
#' dir('./pkg_301')
#'
#' # Clean up
#' unlink('./pkg_301', recursive = TRUE)
#'
#' @export
#'

template_directories <- function(path, dir.name){
  
  # Stop if directory exists
  
  if (dir.exists(paste0(path, '/', dir.name))){
    stop(paste0(path, '/', dir.name, ' already exists!'))
  }

  # Create parent directory ---------------------------------------------------
  
  message(
    paste0(
      'Templating ',
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
  
  message('Done.')
  
}
