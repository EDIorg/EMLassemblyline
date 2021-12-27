create_spatialVector <- function(path, data.path = path, vector_attributes = NULL, overwrite) {
  
  # If the user does not provide a direct path to a raster_attributes.txt template
  # AND does not have a raster_attributes.txt template at the path directory,
  # then this function can not proceed
  
  if (is.null(vector_attributes) & !file.exists(paste0(path, '/shape_attributes.txt'))) { # TODO eventually, this function should handle any vector data
    
    stop("A vector_attributes template is required.", call. = FALSE)
    
  } else if (!is.null(vector_attributes)) {
    
    # Use the argument vector_attributes if provided
    
    vector_template = vector_attributes
  } else {
    
    # read table
    
    vector_template <- data.table::fread(
      paste0(path, '/shape_attributes.txt'),
      colClasses = "character",
      sep = '\t') 
    
  }
  
  # Validate template -------------------------------------------------------
  
  # Check that files exist

  missing_files <- mapply(
    function(x, y) {
      if (y == ""){

        if(!any(stringr::str_detect(list.files(data.path), x))) {x}

      } else {

        # Check in specified root_dir

        if(!any(stringr::str_detect(list.files(paste0(data.path, "/", y)), x))) {x}



      }
    },
    x = vector_template$extname,
    y = vector_template$root_dir
  )
  
  # Warn that files don't exist
  
  if (!is.null(unlist(missing_files))) {
    
    warning(paste0("Could not locate '", paste(unlist(missing_files)), "' at '", data.path, "'\n"), call. = F)
  }
  
  # Remove missing files from template
  
  vector_template <- vector_template[!(vector_template$extname %in% missing_files),]
  
  # Check that attributeDefinitions exist
  
  missing_descs <- vector_template$extname[vector_template$description == ""]
  
  # Warn that files don't exist
  
  if (length(missing_descs) != 0) {
    
    warning(paste0("File '", paste(unlist(missing_descs)), "' does not have a description in the vector_attributes template (Required).\n"), call. = F)
  }
  
  # Remove files with missing definitions from template
  
  vector_template <- vector_template[!(vector_template$extname %in% missing_descs),]
  
  sv <- vector("list", nrow(vector_template))
  for (i in 1:nrow(vector_template)) {
    sv[[i]] <- build_shape_element(
      s = vector_template[i,],
      path = path,
      data.path = data.path)
    
  }
  
}

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
