#' Describe spatial files
#'
#' @description Describes characteristics of spatial raster or spatial files. Use this template to facilitate the creation of the \code{<spatialRaster>} or \code{<spatialVector>} element in EML.
#' 
#' @param path 
#'     (character) Path to the metadata template directory.
#' @param data.path
#'     (character) Path to the data directory.
#' @param spatial.type
#'     (character) Type of spatial data.
#'     \item{Currently Supported}{
#'         \itemize{
#'         \item{raster: Filename of raster data}
#'         \item{shape: Filename (without extension) of a shapefile}
#'         }
#'     }
#' @param spatial.files
#'     (character) File name. If more than one, then supply as a 
#'     vector of character strings (e.g. 
#'     \code{spatial.files = c('change.tif', 'mean_votes.tif')}).
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
#'     \item{filename: Filename of a raster (raster only)}
#'     \item{extname: Filename without extension of a shapefile (shapefile only)}
#'     \item{description: Description of the spatial file}
#'     \item{geoDescription: Description of the geographic coverage of the spatial file}
#'     \item{url: Accessible url for download of the file}
#'     \item{definition: Definition of an attribute (raster only)}
#'     \item{unit: Unit of an attribute (raster only)}
#'     \item{numberType: ('real', 'natural', 'integer', 'whole', 'categorical') (raster only)}
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
template_spatial_attributes <- function(
  path = NULL,
  data.path = path,
  spatial.type = "raster",
  spatial.files = NULL,
  empty = FALSE,
  write.file = TRUE,
  return.obj = FALSE) {
  
  message(paste0("Templating ", spatial.type, " attributes ..."))
  
  # Validate arguments --------------------------------------------------------
  
  validate_arguments(
    fun.name = 'tempate_spatial_attributes',
    fun.args = as.list(environment()))
  
  # Create template table
  
  if (spatial.type == "raster") {
    
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
      filename = spatial.files,
      description = rep("", length(spatial.files)),
      geoDescription = rep("", length(spatial.files)),
      url = rep("", length(spatial.files)),
      definition = rep("", length(spatial.files)),
      unit = rep("", length(spatial.files)),
      numberType = rep("", length(spatial.files)),
      stringsAsFactors = F
    )
    
  } else if (spatial.type == "shape") {
    
    empty_output <- data.frame(
      extname = character(),
      description = character(),
      geoDescription = character(),
      url = character(),
      stringsAsFactors = F
    )
    
    named_output <- data.frame(
      extname = tools::file_path_sans_ext(spatial.files),
      description = rep("", length(spatial.files)),
      geoDescription = rep("", length(spatial.files)),
      url = rep("", length(spatial.files)),
      stringsAsFactors = F
    )
  }
  
  write_spatial_template <- function(output) {
    suppressWarnings(
      utils::write.table(
        output,
        paste0(path, "/", spatial.type, "_attributes.txt"),
        sep = "\t",
        row.names = F,
        quote = F,
        fileEncoding = "UTF-8"))
  }
  
  # Write table to file -----------------------------------------------------
  
  if (!is.null(path)) {
    
    if (is.null(spatial.files) | isTRUE(empty)) {
      
      # (empty)
      write_spatial_template(empty_output)  
      
    } else {
      
      write_spatial_template(named_output)
    }
  }
  
  # Return values -------------------------------------------------------------
  
  message("Done.")
  
  if (isTRUE(return.obj)){
    
    if (is.null(spatial.files) | isTRUE(empty)) {
      
      # (empty)
      return(empty_output)
      
    } else {
      
      return(named_output)
      
    }
  }
}