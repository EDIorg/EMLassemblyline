#' Create EML \code{<spatialVector>} elements
#'
#' @description Creates a list of <spatialVector> elements by parsing user-provided information and physical files. Can be called from \code{make_eml()} or as a stand-alone function.
#'
#' @param path 
#'     (character) Path to the metadata template directory.
#' @param data.path
#'     (character) Path to the data directory.
#' @param vector_attributes 
#'     (data.frame; optional) Alternative to a vector attributes template; enter attributes into a data.frame object and provide as an argument.
#' @param shape_attributes 
#'     (data.frame; optional) Alternative to a shape attributes template; enter attributes into a data.frame object and provide as an argument.
#'
#'
#'
#' @return
#' \item{spatialVector}{\code{<spatialVector>} element (list) that can be inserted into an EML file or object via \code{make_eml()}.}
#'
#' @examples
#' \dontrun{
#' # Set working directory
#' setwd("/Users/me/Documents/data_packages/pkg_260")
#' 
#' # For use with shape_attributes.txt
#' spatialVector <- create_spatialVector(
#'   path = "./metadata_templates",
#'   data.path = "./data_objects")
#' 
#' 
#' # For use without a template
#' 
#' my_attributes <- data.frame(
#'   extname = "my_shape_layer",
#'   root_dir = "Shapefile Folder",
#'   description  = "Experimental Plots",
#'   geoDescription = 'Greater Rio Grande valley area',
#'   overwrite = TRUE
#'   )
#' 
#' spatialVector <- create_spatialVector(
#'   path = "./metadata_templates",
#'   data.path = "./data_objects",
#'   shape_attributes = my_attributes)
#' }
#' 
#' @export
#'
create_spatialVector <- function(path, data.path = path, vector_attributes = NULL, shape_attributes = NULL) {
  
  # If the user does not provide a direct path to a raster_attributes.txt template
  # AND does not have a raster_attributes.txt template at the path directory,
  # then this function can not proceed
  
  if (is.null(c(vector_attributes, shape_attributes)) & !any(c('shape_attributes.txt', 'vector_attributes.txt') %in% list.files(path))) { # TODO eventually, this function should handle any vector data
    
    stop("A vector_attributes template is required.", call. = FALSE)}
  
  if (!is.null(vector_attributes)) {

    vector_template = vector_attributes
    
  } else if ('vector_attributes.txt' %in% list.files(path)) {
    
    vector_template <- data.table::fread(
      paste0(path, '/vector_attributes.txt'),
      colClasses = "character",
      sep = '\t')}
  
  if (!is.null(shape_attributes)) {
    
    shape_template = shape_attributes
    
  } else if ('shape_attributes.txt' %in% list.files(path)) {
    
    shape_template <- data.table::fread(
      paste0(path, '/shape_attributes.txt'),
      colClasses = "character",
      sep = '\t')}
  
  # Validate template -------------------------------------------------------
  
  # Check that files exist

  missing_shapes <- mapply(
    function(x, y) {
      if (y == ""){

        if(!any(stringr::str_detect(list.files(data.path), x))) {x}

      } else {

        # Check in specified root_dir

        if(!any(stringr::str_detect(list.files(paste0(data.path, "/", y)), x))) {x}



      }
    },
    x = shape_template$extname,
    y = shape_template$root_dir
  )
  
  # Warn that files don't exist
  
  if (!is.null(unlist(missing_shapes))) {
    
    warning(paste0("Could not locate '", paste(unlist(missing_shapes)), "' at '", data.path, "'\n"), call. = F)
  }
  
  # Remove missing files from template
  
  shape_template <- shape_template[!(shape_template$extname %in% missing_shapes),]
  
  # Check that attributeDefinitions exist
  
  missing_descs <- shape_template$extname[shape_template$description == ""]
  
  # Warn that files don't exist
  
  if (length(missing_descs) != 0) {
    
    warning(paste0("File '", paste(unlist(missing_descs)), "' does not have a description in the vector_attributes template (Required).\n"), call. = F)
  }
  
  # Remove files with missing definitions from template
  
  shape_template <- shape_template[!(shape_template$extname %in% missing_descs),]
  
  if (nrow(shape_template) == 0) {
    stop("See warning messages:\n")
  }
  
  sv <- vector("list", nrow(shape_template))
  for (i in 1:nrow(shape_template)) {
    sv[[i]] <- build_shape_element(
      s = shape_template[i,],
      path = path,
      data.path = data.path)
    
  }
  
}

#' Build the <spatialVector> elements for shapefiles
#'
#' @param s
#'     (list) A single row from the shape_template object.
#' @param path 
#'     (character) Path to the metadata template directory.
#' @param data.path
#'     (character) Path to the data directory.
#'
#' @return
#' \item{spatialVector}{A single \code{<spatialVector>} element (list).}
#' 
#'
build_shape_element <- function(s, path = path, data.path = data.path) {
  
  message(paste0('  <spatialVector> (', s$extname, ')'))

  # TODO revisit "overwrite" what is the best way to do this?
  
  # create a target directory in the working directory to house shapefiles
  
  new_dir <- paste0(path.expand(data.path), '/', s$extname)
  
  if (dir.exists(new_dir) && s$overwrite == FALSE) {
    
    stop("directory to be created (", shQuote(s$extname, type = "sh"), ") already exists at ", path, " (set overwrite to TRUE)")
    
  }
  
  if (dir.exists(new_dir) && s$overwrite == TRUE) {
    
    system(paste0("rm -r ", shQuote(new_dir, type = "sh")))
    
  }
  
  system(paste0("mkdir ", shQuote(new_dir, type = "sh")))
  
  # identify shapefiles and copy to target directory
  
  shape_files <- list.files(
    path = paste0(data.path, '/', s$root_dir),
    pattern = s$extname,
    full.names = TRUE
  )
  
  shape_extensions <- c("cpg","dbf","prj","sbn","sbx","shp","xml","shx")
  
  excluded_files <- shape_files[!tools::file_ext(shape_files) %in% shape_extensions]
  shape_files <- shape_files[tools::file_ext(shape_files) %in% shape_extensions]
  
  lapply(
    shape_files,
    function(x) {
      system(paste0("cp ", shQuote(x, type = "sh"), " ", shQuote(new_dir, type = "sh")))})
  
  
  # zip directory housing shapefiles
  
  if (file.exists(paste0(new_dir, ".zip")) && s$overwrite == FALSE) {
    
    stop("zip file to be created (", paste0(new_dir, ".zip"), ") already exists in working directory (set overwrite to TRUE)")
    
  }
  
  if (file.exists(paste0(new_dir, ".zip")) && s$overwrite == TRUE) {
    
    system(paste0("rm ", shQuote(new_dir, type = "sh"), ".zip"))
    
  }
  
  invisible(system(
    paste0(
      "zip -jXDr ",
      paste0(shQuote(new_dir, type = "sh"), ".zip"),
      " ",
      shQuote(new_dir, type = "sh")
    ), intern = TRUE
  ))
  
  zipped_name <- paste0(s$extname, ".zip")
  
  # remove (unzipped) target directory
  
  system(paste0("rm -r ", shQuote(new_dir, type = "sh")))
  
  #}}}
  
  # read data
  
  this_vector <- sf::st_read(
    dsn = paste0(data.path, s$root_dir), quiet = TRUE)
  
  # construct EML -----------------------------------------------------
  
  # geographic coverage
  
  if (s$geoDescription == "") warning("Entity ", s$extname," does not have a geographic description.", call. = FALSE)
  
  spatialCoverage <- EML::set_coverage(
    geographicDescription = s$geoDescription,
    westBoundingCoordinate =  sf::st_bbox(this_vector)[["xmin"]],
    eastBoundingCoordinate =  sf::st_bbox(this_vector)[["xmax"]],
    northBoundingCoordinate = sf::st_bbox(this_vector)[["ymax"]],
    southBoundingCoordinate = sf::st_bbox(this_vector)[["ymin"]]
  )
  
  # attributes ---------------------------------------------------------------
  
  # TODO not currently supported. Not sure what this would look like.
  
  
  # set physical ------------------------------------------------------------
  
  # distribution

  fileDistribution <- EML::eml$distribution(
    EML::eml$online(url = s$url)
  )

  
  # data format
  
  fileDataFormat <- EML::eml$dataFormat(
    externallyDefinedFormat = EML::eml$externallyDefinedFormat(
      formatName = "Esri Shapefile (zipped)")
  )
  
  # file size
  
  fileSize <- EML::eml$size(unit = "byte")
  fileSize$size <- deparse(file.size(paste0(data.path, zipped_name)))
  
  # authentication
  
  fileAuthentication <- EML::eml$authentication(method = "MD5")
  fileAuthentication$authentication <- tools::md5sum(paste0(data.path, zipped_name))
  
  # construct physical
  
  spatialVectorPhysical <- EML::eml$physical(
    objectName = zipped_name,
    authentication = fileAuthentication,
    size = fileSize,
    dataFormat = fileDataFormat,
    distribution = fileDistribution
  )
  
  # create spatialVector ----------------------------------------------------
  
  sv <- EML::eml$spatialVector(
    entityName = zipped_name,
    entityDescription = s$description,
    physical = spatialVectorPhysical,
    coverage = spatialCoverage,
    #attributeList = attributes,
    geometricObjectCount = nrow(this_vector)
  )
  
  # add geometry type -------------------------------------------------------
  
  sfGeometry <- attr(this_vector$geometry, "class")[[1]]
  
  if (grepl("polygon", sfGeometry, ignore.case = TRUE)) {
    
    objectGeometry <- "Polygon"
    
  } else if (grepl("point", sfGeometry, ignore.case = TRUE)) {
    
    objectGeometry <- "Point"
    
  } else if (grepl("linestring", sfGeometry, ignore.case = TRUE)) {
    
    objectGeometry <- "LineString"
    
  } else {
    
    stop(paste0("undetermined geometry: ", attr(this_vector$geometry, "class")[[1]]))
    
  }
  
  sv$geometry <- objectGeometry
  
  # add spatial reference  --------------------------------------------------
  
  # TODO needs to be handled with a mapping function
  
  if (raster::crs(this_vector)@projargs == "+proj=utm +zone=3 +datum=WGS84 +units=m +no_defs") {
    coord_sys = "WGS_1984_UTM_Zone_3N"
  }
  
  spatial_ref <- EML::eml$spatialReference(
    horizCoordSysName = coord_sys
  )
  
  sv$spatialReference <- spatial_ref
  
  
  # return ------------------------------------------------------------------
  
  return(sv)
  
}

build_vector_element <- function()
