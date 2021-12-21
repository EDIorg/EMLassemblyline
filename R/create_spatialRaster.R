
#' Create EML \code{<spatialRaster>} elements
#'
#' @description Creates a list of <spatialRaster> elements by parsing user-provided information and physical files. Can be called from \code{make_eml()} or as a stand-alone function.
#'
#' @param path 
#'     (character) Path to the metadata template directory.
#' @param data.path
#'     (character) Path to the data directory.
#' @param raster_attributes 
#'     (data.frame; optional) Alternative to the raster attributes template; enter attributes into a data.frame object and provide as an argument.
#' @param raster_catvars
#'     (data.frame; optional) Alternative to the raster variables template; enter codes and definitions into a data.frame object and provide as an argument.
#'
#' @return
#' \item{spatialRaster}{\code{<spatialRaster>} element (list) that can be inserted into an EML file or object via \code{make_eml()}.}
#'
#' @examples
#' \dontrun{
#' # Set working directory
#' setwd("/Users/me/Documents/data_packages/pkg_260")
#' 
#' # For use with raster_attributes.txt (and optionally raster_variables.txt)
#' spatialRaster <- create_spatialRaster(
#'   path = "./metadata_templates",
#'   data.path = "./data_objects")
#' 
#' 
#' # For use without a template
#' 
#' my_attributes <- data.frame(
#'   filename = 'my_raster_file.tiff',
#'   geoDescription = 'Greater Rio Grande valley area',
#'   definition = 'Normalized Difference Vegetation Index',
#'   unit = 'dimensionless',
#'   numberType = 'real')
#' 
#' spatialRaster <- create_spatialRaster(
#'   path = "./metadata_templates",
#'   data.path = "./data_objects",
#'   raster_attributes = my_attributes)
#' }
#' 
#' @export
#'
create_spatialRaster <- function(path, data.path = path, raster_attributes = NULL, raster_catvars = NULL) {
  
  # If the user does not provide a direct path to a raster_attributes.txt template
  # AND does not have a raster_attributes.txt template at the path directory,
  # then this function can not proceed
  
  if (is.null(raster_attributes) & !file.exists(paste0(path, '/raster_attributes.txt'))) {
    
    stop("A raster_attributes template is required.", call. = FALSE)
    
  } else if (!is.null(raster_attributes)) {

    # Use the argument raster_attributes if provided
    
    raster_template = raster_attributes
  } else {
    
    # read table
    
    raster_template <- data.table::fread(
      paste0(path, '/raster_attributes.txt'),
      colClasses = "character",
      sep = '\t')    
    
  }
  

# Validate template -------------------------------------------------------

  # Check that files exist
  
  missing_files <- lapply(raster_template$filename, function(x) {
    if(!any(stringr::str_detect(list.files(data.path), x))) x })
  
  # Warn that files don't exist
  
  if (!is.null(unlist(missing_files))) {
    
    warning(paste0("Could not locate '", paste(unlist(missing_files)), "' at '", data.path, "'\n"), call. = F)
  }
  
  # Remove missing files from template
  
  raster_template <- raster_template[!(raster_template$filename %in% missing_files),]
  
  # Check that attributeDefinitions exist
  
  missing_defs <- raster_template$filename[raster_template$definition == ""]
  
  # Warn that files don't exist
  
  if (length(missing_defs) != 0) {
    
    warning(paste0("File '", paste(unlist(missing_defs)), "' does not have a definition in the raster_attributes template (Required).\n"), call. = F)
  }
  
  # Remove files with missing definitions from template
  
  raster_template <- raster_template[!(raster_template$filename %in% missing_defs),]
  

# Validate categoricals ---------------------------------------------------

  # check to see if any remaining files are "categorical"
  
  if ("categorical" %in% raster_template$numberType) {
    
    cat_files <- raster_template$filename[raster_template$numberType == "categorical"]
    
  # bring in and validate Raster factors
    if (is.null(raster_catvars) & !file.exists(paste0(path, '/raster_catvars.txt'))) {
      
    # if raster_catvars doesnt exist, stop
      
      stop('raster_catvars.txt does not exist. Expected file named "raster_catvars.txt" to exist.')
      
    } else if (!is.null(raster_catvars)) {
      
      # Use the argument raster_attributes if provided
      
      raster_template = raster_attributes
      
    } else {
      
      raster_var <- data.table::fread(
        paste0(path, '/raster_catvars.txt'),
        colClasses = "character",
        sep = '\t')
    }    
    
    # Only keep raster_var values that are still in raster_template, labelled as categorical
    
    raster_var <- raster_var[(raster_var$filename %in% cat_files),]
    
  # validate that every code is paired with a definition (and vice versa)
    
    if (any(raster_var$code == "") | any(raster_var$definition == "")) {
      
      missing_catvars <- raster_var$filename[raster_var$code == ""|raster_var$definition == ""]
      
      if (!is.null(missing_catvars)) {
        
        warning(paste0("Categorical variable for '", paste(unlist(missing_defs)), "' has an incomplete code/definition pair (Required).\n"), call. = F)
      
        # Remove missing_catvars from _var
        raster_var <- raster_var[!(raster_var$filename %in% missing_catvars),]
        
      
        # Change numberType to blank in raster_template if missing_catvars
        # TODO clean this up
        missing_catvars_sub <- raster_template[(raster_template$filename %in% missing_catvars),]
        missing_catvars_sub$numberType <- ""
        raster_template <- raster_template[!(raster_template$filename %in% missing_catvars),]
        raster_template <- rbind(raster_template, missing_catvars_sub)
        
      }
    }
  }
  sr <- vector("list", nrow(raster_template))
  for (i in 1:nrow(raster_template)) {
    sr[[i]] <- build_raster_element(
      r = raster_template[i,],
      rv = raster_var,
      path = path,
      data.path = data.path)
    
  }
  

  return(sr)
}

#' Build the <spatialRaster> elements
#'
#' @param r
#'     (list) A single row from the raster_template
#' @param rv 
#'     (data.frame) The raster_var object in its entirety
#' @param path 
#'     (character) Path to the metadata template directory.
#' @param data.path
#'     (character) Path to the data directory.
#'
#' @return
#' \item{spatialRaster}{A single \code{<spatialRaster>} element (list).}
#' 
#'
build_raster_element <- function(r, rv = raster_var, path = path, data.path = data.path) {
  
  message(paste0('  <spatialRaster> (', r$filename, ')'))
  
  # Load raster -------------------------------------------------------------
  
  raster_object <- raster::raster(paste0(data.path, r$filename))
  
  # Get spatial reference and convert to EML standard--------------------------
  
  # Read the proj4 string
  
  proj4str <- raster::crs(raster_object, asText = TRUE)[[1]]
  
  # Assign EML-compliant name
  # Allowed values for EML seem to be enumerated here: 
  # https://eml.ecoinformatics.org/schema/eml-spatialReference_xsd.html#SpatialReferenceType
  
  # TODO this should be mapped

    if (proj4str=="+proj=utm +zone=13 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"){
      eml_projection <- "NAD_1983_UTM_Zone_13N"
    } else if (proj4str=="+proj=utm +zone=13 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"){
      eml_projection <- "NAD_1983_CSRS98_UTM_Zone_13N"
    } else if (proj4str == "+proj=utm +zone=19 +datum=WGS84 +units=m +no_defs") {
      eml_projection <- "WGS_1984_UTM_Zone_19N"
    } else if (proj4str == "+proj=utm +zone=18 +datum=WGS84 +units=m +no_defs") {
      eml_projection <- "WGS_1984_UTM_Zone_18N"
    }else if (proj4str == "+proj=longlat +datum=WGS84 +no_defs") {
      eml_projection <- "WGS_1984_UTM_Zone_12N"
    }
  
  
  # Determine coverage (bbox) of raster ---------------------------------------
  
  # For EML, this apparently needs to be in decimal degrees, so convert
  # to SpatialPolygons and then reproject with spTransform
  
  # Convert to SpatialPolygons
  
  extent <- as(raster::extent(raster_object), "SpatialPolygons")
  
  # Assign CRS to SpatialPolygon object
  
  sp::proj4string(extent) <- proj4str
  
  # Reproject with spTransform
  
  extent_geo <- sp::spTransform(extent, sp::CRS("+proj=longlat +datum=WGS84 +no_defs 
                                            +ellps=WGS84 +towgs84=0,0,0"))
  
  # TODO sort out mapply
  
  spatialCoverage <- EML::set_coverage(geographicDescription = enc2utf8(r$geoDescription),
                                       west = extent_geo@bbox["x", "min"],
                                       east = extent_geo@bbox["x", "max"],
                                       north = extent_geo@bbox["y", "max"],
                                       south = extent_geo@bbox["y", "min"])
  
  
  # projections----------------------------------------------------------------
  projections <- list(section = list(
    paste0("<title>Raster derived coordinate reference system</title>\n<para>",
           proj4str, "</para>")
  ))
  
  
  # Create attributes table----------------------------------------------------

  
   if (r$numberType == 'categorical') {
     
   # Set categorical-type attributes
     # extract the relevant catvar info
     
     r_factor <- subset(rv, filename == r$filename)
     
     # Change filename to attributeName 
     
     names(r_factor)[1] <- 'attributeName'
     
     r_factor$attributeName <- 'raster_value'
     
     
     attr_list <- EML::set_attributes(
       attributes=data.frame(
         attributeName = "raster_value",
         attributeDefinition = r$definition),
       factors=r_factor, 
       col_classes = "factor")
     
    } else {
      
    # Set numeric-type attributes
      
      attr_list <- EML::set_attributes(
       attributes=data.frame(
        attributeName = "raster_value",
        attributeDefinition = r$definition,
        unit = r$unit,
        numberType = r$numberType),
      col_classes="numeric")
   }
  
  # set authentication (md5)---------------------------------------------------
  
  fileAuthentication <- EML::eml$authentication(method = "MD5")
  fileAuthentication$authentication <- tools::md5sum(paste0(data.path, r$filename))

  
  
  # set file size--------------------------------------------------------------
  fileSize <- EML::eml$size(unit = "byte")
  fileSize$size <- deparse(file.size(paste0(data.path, r$filename)))

  
  
  # set file format------------------------------------------------------------

  fileDataFormat <- EML::eml$dataFormat(
    externallyDefinedFormat=EML::eml$externallyDefinedFormat(
      formatName=tools::file_ext(paste0(data.path, r$filename))))

  # Create rasterPhysical pieces-----------------------------------------------
  directoryName <- dirname(paste0(data.path, r$filename)) #dirname(rasterFname)
  directoryNameFull <- sub("/$", "", path.expand(dirname(paste0(data.path, r$filename))))
  pathToFile <- path.expand(paste0(data.path, r$filename))
  
  # set distribution
  distribution <- EML::eml$distribution(
            EML::eml$online(url = r$url))

  
  
  # build physical
  spatialRasterPhysical <- EML::eml$physical(
    objectName = r$filename,
    authentication = fileAuthentication,
    size = fileSize,
    dataFormat = fileDataFormat,
    distribution = distribution
  )
  
  # build spatialRaster--------------------------------------------------------
  
  sr <- EML::eml$spatialRaster(
    entityName = r$filename,
    entityDescription = r$description,
    physical = spatialRasterPhysical,
    coverage = spatialCoverage,
    additionalInfo = projections,
    attributeList = attr_list,
    spatialReference = EML::eml$spatialReference(
      horizCoordSysName = eml_projection),
    numberOfBands = raster::bandnr(raster_object),
    rows = nrow(raster_object),
    columns = ncol(raster_object),
    horizontalAccuracy = EML::eml$horizontalAccuracy(accuracyReport="Unknown"),
    verticalAccuracy = EML::eml$verticalAccuracy(accuracyReport="Unknown"),
    cellSizeXDirection = raster::xres(raster_object),
    cellSizeYDirection = raster::yres(raster_object),
    rasterOrigin = "Upper Left",
    verticals = 1,
    cellGeometry = "pixel",
    id = r$filename
  )
  
  return(sr)
}
  
  
