#' Describe spatial raster files
#'
#' @description Describes characteristics of spatial raster files. Use this template to facilitate the creation of the spatialRaster element in EML.
#' 
#' @param path 
#'     (character) Path to the metadata template directory.
#' @param data.path
#'     (character) Path to the data directory.
#' @param raster.files
#'     (character) File name. If more than one, then supply as a 
#'     vector of character strings (e.g. 
#'     \code{raster.files = c('change.tif', 'mean_votes.tif')}).
#' @param empty
#'     (logical) Whether to write an empty template file. Default is \code{FALSE}.
#' @param write.file
#'     (logical; optional) Whether to write the template file. Default is \code{TRUE}.
#' @param return.obj
#'     (logical; optional) Whether to return the provenance template as a data frame. Default is \code{FALSE}.
#'     
#' @return
#' \item{raster_attributes.txt}{Columns:
#'     \itemize{
#'     \item{filename: Filename of a spatial raster file}
#'     \item{description: Description of the raster file}
#'     \item{geoDescription: Description of the geographic coverage of the raster file}
#'     \item{url: Accessible url for download of the file}
#'     \item{definition: Definition of the raster attribute}
#'     \item{unit: Unit of the raster attribute}
#'     \item{numberType: ('real', 'natural', 'integer', 'whole')}
#'     }
#' }
#'
#' @examples
#' \dontrun{
#' # Set working directory
#' setwd("/Users/me/Documents/data_packages/pkg_260")
#' 
#' # For an empty template to be filled manually
#' template_raster_attributes(
#'     path = "./metadata_templates",
#'     data.path = "./data_objects",
#'     empty = TRUE)
#'     
#' # For a template with file names
#' template_raster_attributes(
#'     path = "./metadata_templates",
#'     data.path = "./data_objects",
#'     raster.files = c('change.tif', 'mean_votes.tif'))
#' }
#' 
#' @export
template_raster_attributes <- function(
  path = NULL,
  data.path = path,
  raster.files = NULL,
  empty = FALSE,
  write.file = TRUE,
  return.obj = FALSE) {
  
  message("Templating raster attributes ...")
  
  # Validate arguments --------------------------------------------------------
  
  validate_arguments(
    fun.name = 'tempate_raster_attributes',
    fun.args = as.list(environment()))

  # Create template table
  
    empty_output <- data.frame(
       filename = character(),
       description = character(),
       geoDescription = character(),
       url = character(),
       definition = character(),
       unit = character(),
       numberType = character(),
       stringsAsFactors = F
    )
    
    named_output <- data.frame(
      filename = raster.files,
      description = rep("", length(raster.files)),
      geoDescription = rep("", length(raster.files)),
      url = rep("", length(raster.files)),
      definition = rep("", length(raster.files)),
      unit = rep("", length(raster.files)),
      numberType = rep("", length(raster.files)),
      stringsAsFactors = F
    )
    
    write_raster_template <- function(output) {
      suppressWarnings(
        utils::write.table(
          output,
          paste0(path, "/", "raster_attributes.txt"),
          sep = "\t",
          row.names = F,
          quote = F,
          fileEncoding = "UTF-8"))
    }
    
  # Write table to file -----------------------------------------------------
  
  if (!is.null(path)) {
    
    if (is.null(raster.files) | isTRUE(empty)) {
      
      # (empty)
      write_raster_template(empty_output)  
    
    } else {
      
      write_raster_template(named_output)
    }
  }
  
  # Return values -------------------------------------------------------------
  
  message("Done.")
  
  if (isTRUE(return.obj)){
    
    if (is.null(raster.files) | isTRUE(empty)) {
      
      # (empty)
      return(empty_output)
      
    } else {
      
      return(named_output)
      
    }
  }
}