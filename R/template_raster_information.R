#' Create template for information need to create EML element of raster file
#' 
#' @description Create a template containing all mandatory information need to create spatialRaster EML element.
#' 
#' @param path
#'     (character) Path to the metadata template directory.
#' @param data.path
#'     (character) Path to the data directory.
#' @param raster.file
#'     (character) File names of raster files. If more than one, then supply as
#'     a vector.
#' @param write.file
#'     (logical; optional) Whether to write the template file.
#' 
#' @return
#' \item{information_*}{Columns:
#'     \itemize{
#'     \item{spatialReference: Coordinate system used.}
#'     \item{horizontalAccuracy: Accuracy of horizontal locational mesurements}
#'     \item{verticalAccuracy: Accuracy of vertical locational mesurements}
#'     \item{cellSizeXDirection: Width of cells in the x direction}
#'     \item{cellSizeYDirection: Width of cells in the y direction}
#'     \item{numberOfBands: Number of bands in the raster}
#'     \item{rasterOrigin: Location of the pixel with the minimum x and y values. 
#'           Valid options are : "Upper Left", "Lower Left", "Upper Right", "Lower Right"}
#'     \item{rows: Maximum number of objects in the y axis}
#'     \item{columns: Maximum number of objects in the x axis}
#'     \item{verticals: Maximum number of objects in the z axis}
#'     \item{cellGeometry: Geometric representation of the cell's content. 
#'           Valid options are : "pixel" (cell value representative of the entire cell) and 
#'           "matrix" (cell value representative of a single point within the cell).}
#'     }
#' }
#'
#' @examples
#' \dontrun{
#' # Set working directory
#' setwd("/Users/me/Documents/data_packages/pkg_260")
#' 
#' template_vector_information(
#'   path = "./metadata_templates",
#'   data.path = "./data_objects",
#'   vector.file = "geotiff_test_file.tif")
#' }
#' 
#' @export
#'
template_raster_information <- function(
  path = NULL,
  data.path = path,
  raster.file = NULL,
  write.file = TRUE) {
  
  message('Templating raster information ...')
  
  # Validate arguments --------------------------------------------------------
  
  # Validate path usage before passing arguments to validate_arguments()
  if (missing(path)){
    stop('Input argument "path" is missing.')
  }
  
  validate_arguments(
    fun.name = 'template_raster_information',
    fun.args = as.list(environment()))
  
  # If files are absent ...
  
  if (is.null(raster.file)){
    
    stop('Input argument "raster.file" is missing.')
    
  }
  
  # Import information_*.txt ---------------------------------------------------
  
  # Extract information of each raster file
  
  information <- list()
  
  for (i in 1:length(raster.file)){
    
    # Read file
    
    rasterFile <- terra::rast(paste(data.path, raster.file[i], sep = "/"))
    
    # Initialize information table

    information[[i]] <- data.frame(
      spatialReference = "",
      horizontalAccuracy = "Unknown",
      verticalAccuracy = "Unknown",
      cellSizeXDirection = terra::xres(rasterFile),
      cellSizeYDirection = terra::yres(rasterFile),
      numberOfBands = terra::nlyr(rasterFile),
      rasterOrigin = "",
      rows = terra::nrow(rasterFile),
      columns = terra::ncol(rasterFile),
      verticals = 1,
      cellGeometry = "pixel",
      stringsAsFactors = FALSE
    )
    
    # Guess spatialReference
    
    projection <- terra::crs(rasterFile, proj = TRUE)
    
    if (grepl("+datum=WGS84", terra::crs(rasterFile, proj = TRUE))){
      information[[i]]$spatialReference <- "GCS_WGS_1984"
    } else if (projection == "+proj=lcc +lat_0=46.5 +lon_0=3 +lat_1=49 +lat_2=44 +x_0=700000 +y_0=6600000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"){
      information[[i]]$spatialReference <- "RGF_1993_Lambert_93"
    }

    # Guess rasterOrigin
    
    # Get position of cell with lower coordinates
    origin_cell <- c(
      # column of cell with lower x coordinate
      terra::colFromX(rasterFile, terra::xmin(rasterFile)),
      # row number of cell with lower y coordinate
      terra::rowFromY(rasterFile, terra::ymin(rasterFile))
    )
    
    if (origin_cell[1] == 1){
      if (origin_cell[2] == 1){
        information[[i]]$rasterOrigin <- "Upper Left"
      } else if (origin_cell[2] == terra::nrow(rasterFile)){
        information[[i]]$rasterOrigin <- "Lower Left"
      }
    } else if (origin_cell[1]) {
      if (origin_cell[2] == 1){
        information[[i]]$rasterOrigin <- "Upper Right"
      } else if (origin_cell[2] == terra::nrow(rasterFile)){
        information[[i]]$rasterOrigin <- "Lower Right"
      }
    }
    
    
    if (isTRUE(write.file)){
      
      # TODO: check for other raster file type (not only .tif)
      if (mime::guess_type(raster.file[i]) == "image/tiff"){
        filename <- gsub(".tif$|.tiff$", "", raster.file[i])
      }
      
      value <- file.exists(
        paste0(
          path,
          "/",
          "information_",
          filename,
          ".txt"
        )
      )
      
      if (!isTRUE(value)){
        
        message(
          paste0(
            "information_",
            filename,
            ".txt."
          )
        )
        
        utils::write.table(
          information[[i]],
          paste0(
            path,
            "/",
            "information_",
            enc2utf8(
              filename),
            ".txt"
          ),
          sep = "\t",
          row.names = F,
          quote = F,
          fileEncoding = "UTF-8"
        )
        
      } else {
        
        message(
          paste0(
            "information_",
            filename,
            ".txt already exists!"
          )
        )
        
      }
      
    }
    
  }

  # Return --------------------------------------------------------------------
  
  if (!isTRUE(write.file)){
    message('No templates were written to file (write.file = FALSE).')
  }
  
  message("Done.")
  
}