#' Describe entities of data objects
#' 
#' Use this function to describe data entity metadata. This file will be used
#' in the make_eml function to set the values defined in this template. 
#' Currently, only the entityType metadata variable for otherEntity 
#' is supported. 
#' 
#' @param path (character) Path to the metadata template directory.
#' @param data.path (character) Path to the data directory.
#' @param data.objects (character) File names of data objects. If \code{NULL},
#'   data objects with an attribute template will be used.
#' @param empty (logical) Whether to create an empty template.
#' @param write.file (logical) Whether to write the template to file. If
#' \code{FALSE}, a data frames will be returned.
#' 
#' @return
#' If \code{write.file = TRUE}, tab delimited file is written to \code{path}. If
#' \code{write.file = FALSE} a data frames is returned. Columns of
#' this template:
#' \itemize{
#' \item{objectName: name of the described object}
#' \item{variable: described metadata}
#' \item{value: value for the metadata set in the \code{variable} field for the 
#' object \code{objectName}}
#' }
#' 
#' @note Currently, only the entityType metadata variable for otherEntity 
#' is supported.
#' 
#' @examples
#' \dontrun{
#' # Create a temporary directory with files for this example
#' testdir <- paste0(tempdir(), "/pkg")
#' pkg_files <- copy_test_package(testdir)
#'
#' # Return template as data frames
#' tmplt <- template_entities(path = testdir, write.file = FALSE)
#'
#' # Return template as file
#' template_entities(path = testdir, write.file = TRUE)
#' 
#' # With the data.objects argument
#' template_entities(
#'   path = testdir, 
#'   data.objects = c("ancillary_data.zip", "processing_and_analysis.R"), 
#'   write.file = TRUE
#' )
#'
#' # Clean up files of this example
#' unlink(testdir, force = TRUE)#' }
#'
#' @export
#'
template_entities <- function(
  path,
  data.path = path,
  data.objects = NULL,
  empty = FALSE,
  write.file = TRUE) {
  
  message("Templating entities")
  
  validate_arguments(
    fun.name = "template_entities",
    fun.args = as.list(environment())
  )
  
  if (is.null(data.objects)) {
    attr_files <- list_attribute_templates(path)
    data_objects <- name_data_objects(attr_files, data.path)
  } else {
    data_objects <- data.objects
  }
  
  x <- template_arguments(
    path = path,
    data.path = data.path,
    data.objects = data_objects
  )$x

  entities_table <- data.frame()
  
  for (file in names(x$data.objects)){
    eml_type <- x$data.objects[[file]]$eml_type
    if (eml_type == "otherEntity") {
      entities_table <- rbind(
        entities_table, 
        template_otherentity_entities(x$data.objects[file], empty)
      )
    } else if (eml_type == "spatialRaster") {
      entities_table <- rbind(
        entities_table, 
        template_raster_entities(x$data.objects[file], empty)
      )
    } else if (eml_type == "spatialVector") {
      entities_table <- rbind(
        entities_table, 
        template_vector_entities(x$data.objects[file], empty)
      )
    }
  }
  
  if (write.file) {
    write_template(entities_table, "entities.txt", path)
  } else {
    return(entities_table)
  }
}


#' Describe entities of otherEntity data objects
#' 
#' @param data.object (list) data object to describe
#' @param empty (logical) Whether to create an empty template.
#' 
#' @return (data.frame) Return a \code{data.frame} corresponding to the 
#' metadata described.
#' 
#' @keywords internal
#' 
template_otherentity_entities <- function(data.object, empty) {
  
  if (empty) {
    return(init_entities())
  }
  
  entity_table <- data.frame(
    objectName = names(data.object),
    variable = "entityType",
    value = "!Add value here!"
  )
  
  return(entity_table)
}


template_raster_entities <- function(data.object, empty) {
  
  if (empty) {
    return(init_entities())
  }
  
  # Initialize entities table for raster files
  
  variables <- c("spatialReference", "horizontalAccuracy", "verticalAccuracy", 
    "cellSizeXDirection", "cellSizeYDirection", "numberOfBands", "rasterOrigin", 
    "rows", "columns", "verticals", "cellGeometry")
  
  entity_table <- data.frame(
    objectName = rep(names(data.object), length(variables)),
    variable = variables,
    value = rep("!Add value here!", length(variables))
  )
  
  # Fill table with file information
  
  raster_file <- terra::rast(data.object[[1]]$file_path)
  
  entity_table[entity_table$variable == "horizontalAccuracy",]$value <- "Unknown"
  entity_table[entity_table$variable == "verticalAccuracy",]$value <- "Unknown"
  entity_table[entity_table$variable == "cellSizeXDirection",]$value <- terra::xres(raster_file)
  entity_table[entity_table$variable == "cellSizeYDirection",]$value <- terra::yres(raster_file)
  entity_table[entity_table$variable == "numberOfBands",]$value <- terra::nlyr(raster_file)
  entity_table[entity_table$variable == "rows",]$value <- terra::nrow(raster_file)
  entity_table[entity_table$variable == "columns",]$value <- terra::ncol(raster_file)
  entity_table[entity_table$variable == "verticals",]$value <- 1
  entity_table[entity_table$variable == "cellGeometry",]$value <- "pixel"
  
  # Guess spatialReference
  
  projection <- terra::crs(raster_file, proj = TRUE)
  
  if (grepl("+datum=WGS84", terra::crs(raster_file, proj = TRUE))){
    entity_table[entity_table$variable == "spatialReference",]$value <- "GCS_WGS_1984"
  } else if (projection == "+proj=lcc +lat_0=46.5 +lon_0=3 +lat_1=49 +lat_2=44 +x_0=700000 +y_0=6600000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"){
    entity_table[entity_table$variable == "spatialReference",]$value <- "RGF_1993_Lambert_93"
  }
  
  # Guess rasterOrigin
  
  # Get position of cell with lower coordinates
  origin_cell <- c(
    # column of cell with lower x coordinate
    terra::colFromX(raster_file, terra::xmin(raster_file)),
    # row number of cell with lower y coordinate
    terra::rowFromY(raster_file, terra::ymin(raster_file))
  )
  
  if (origin_cell[1] == 1){
    if (origin_cell[2] == 1){
      entity_table[entity_table$variable == "rasterOrigin",]$value <- "Upper Left"
    } else if (origin_cell[2] == terra::nrow(raster_file)){
      entity_table[entity_table$variable == "rasterOrigin",]$value <- "Lower Left"
    }
  } else if (origin_cell[1] == terra::ncol(raster_file)) {
    if (origin_cell[2] == 1){
      entity_table[entity_table$variable == "rasterOrigin",]$value <- "Upper Right"
    } else if (origin_cell[2] == terra::nrow(raster_file)){
      entity_table[entity_table$variable == "rasterOrigin",]$value <- "Lower Right"
    }
  }
  
  return(entity_table)
}


template_vector_entities <- function(data.object, empty) {
  
  if (empty) {
    return(init_entities())
  }
  
  # Initialize entities table for vector files
  
  variables <- c("geometry", "geometricObjectCount", "spatialReference")
  
  entity_table <- data.frame(
    objectName = rep(names(data.object), length(variables)),
    variable = variables,
    value = rep("!Add value here!", length(variables))
  )
  
  # Fill table with file information
  
  vector_file <- terra::vect(data.object[[1]]$file_path)
  
  entity_table[entity_table$variable == "geometricObjectCount",]$value <- nrow(vector_file)
  
  # Get geomtype
  
  vector_geomtype <- terra::geomtype(vector_file)
  
  if (vector_geomtype == "points"){
    entity_table[entity_table$variable == "geometry",]$value <- "Point"
  } else if (vector_geomtype == "polygons"){
    entity_table[entity_table$variable == "geometry",]$value <- "Polygon"
  } else if (vector_geomtype == "lines"){
    entity_table[entity_table$variable == "geometry",]$value <- "LineString"
  }
  
  # Get spatialReference
  
  projection <- terra::crs(vector_file, proj = TRUE)
  
  if (grepl("+datum=WGS84", projection)){
    entity_table[entity_table$variable == "spatialReference",]$value <- "GCS_WGS_1984"
  } else if (projection == "+proj=lcc +lat_0=46.5 +lon_0=3 +lat_1=49 +lat_2=44 +x_0=700000 +y_0=6600000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"){
    entity_table[entity_table$variable == "spatialReference",]$value <- "RGF_1993_Lambert_93"
  }
  
  return(entity_table)
}