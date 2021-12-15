
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

  # Load raster -------------------------------------------------------------
  
  raster_object <- lapply(raster_template$filename, function(x) raster::raster(paste0(data.path, x)))
  
  # Get spatial reference and convert to EML standard--------------------------
  
  # Read the proj4 string
  
  P <- lapply(raster_object, raster::crs)
  
  proj4str <- lapply(proj4str, function(x) paste(x@projargs))
  
  # Assign EML-compliant name
  # Allowed values for EML seem to be enumerated here: 
  # https://eml.ecoinformatics.org/schema/eml-spatialReference_xsd.html#SpatialReferenceType
  
  # TODO this should be mapped
  
  eml_projection <- lapply(proj4str, function(x) {
    
    if (x=="+proj=utm +zone=13 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"){
      emlProjection <- "NAD_1983_UTM_Zone_13N"
    } else if (x=="+proj=utm +zone=13 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"){
      emlProjection <- "NAD_1983_CSRS98_UTM_Zone_13N"
    } else if (x == "+proj=utm +zone=19 +datum=WGS84 +units=m +no_defs") {
      emlProjection <- "WGS_1984_UTM_Zone_19N"
    } else if (x == "+proj=utm +zone=18 +datum=WGS84 +units=m +no_defs") {
      emlProjection <- "WGS_1984_UTM_Zone_18N"
    }else if (x == "+proj=longlat +datum=WGS84 +no_defs") {
      emlProjection <- "WGS_1984_UTM_Zone_12N"
    }
    
  })
  
  
  # Determine coverage (bbox) of raster ---------------------------------------
  
  # For EML, this apparently needs to be in decimal degrees, so convert
  # to SpatialPolygons and then reproject with spTransform
  
  # Convert to SpatialPolygons
  
  extent <- lapply(raster_object, function(x) as(raster::extent(x), "SpatialPolygons"))
  
  # Assign CRS to SpatialPolygon object
  
  extent <- mapply(
    function(x,y) {
      sp::proj4string(x) = y
      return(x)
    }, x = extent, y = proj4str)
  
  # Reproject with spTransform
  
  extent.geo <- lapply(extent, function(x) sp::spTransform(x, sp::CRS("+proj=longlat +datum=WGS84 +no_defs 
                                            +ellps=WGS84 +towgs84=0,0,0")))
  
  message('Determining spatial coverage...')
  
  
  spatialCoverage <- mapply(
    function(x,y) {
      EML::set_coverage(geographicDescription = y,
        west = x@bbox["x", "min"],
        east = x@bbox["x", "max"],
        north = x@bbox["y", "max"],
        south = x@bbox["y", "min"])
    }, x = extent.geo, y = raster_template$geoDescription)
  
  # projections----------------------------------------------------------------
  
  projections <- lapply(proj4str, function(x) {
    list(section = list(
      paste0("<title>Raster derived coordinate reference system</title>\n<para>",
             x, "</para>")))})
  
  # Create attributes table----------------------------------------------------

  message('Building attributes...')
  
  attr_list <- lapply(
    raster_template$filename, 
      function(x) {
         
         working_template <- raster_template[raster_template$filename == x]
         
         if (working_template$numberType == 'categorical') {
           
         # Set categorical-type attributes
           # extract the relevant catvar info
           
           working_factor <- raster_var[raster_var$filename == x]
           
           # Change filename to attributeName 
           
           names(working_factor)[1] <- 'attributeName'
           
           working_factor$attributeName <- 'raster_value'
           
           
           EML::set_attributes(
             attributes=data.frame(
               attributeName = "raster_value",
               attributeDefinition = working_template$definition),
             factors=working_factor, 
             col_classes = "factor")
           
          } else {
            
          # Set numeric-type attributes
            
          EML::set_attributes(
             attributes=data.frame(
              attributeName = "raster_value",
              attributeDefinition = working_template$definition,
              unit = working_template$unit,
              numberType = working_template$numberType),
            col_classes="numeric")
         }
       })
  
  # set authentication (md5)---------------------------------------------------
  
  message('Calculating MD5 sum...')
  
  file_auth <- lapply(
    raster_template$filename,
    function(x) {
      fileAuthentication <- EML::eml$authentication(method = "MD5")
      fileAuthentication$authentication <- tools::md5sum(paste0(data.path, x))
      return(fileAuthentication)
    })
  
  
  # set file size--------------------------------------------------------------
  message('Setting file size...')
  
  file_size <- lapply(
    raster_template$filename,
    function(x) {
      fileSize <- EML::eml$size(unit = "byte")
      fileSize$size <- deparse(file.size(paste0(data.path, x)))
      return(fileSize)
    })
  
  
  # set file format------------------------------------------------------------
  
  message('Setting file format...')
  
  data_format <- lapply(
    raster_template$filename,
    function(x) {
      fileDataFormat <- EML::eml$dataFormat(
        externallyDefinedFormat=EML::eml$externallyDefinedFormat(
          formatName=tools::file_ext(paste0(data.path, x)))
      )
      return(fileDataFormat)
    })
  
  # Create rasterPhysical pieces-----------------------------------------------
  message('Creating rasterPhysical')
  #rasterBaseName <-  raster_template$filename #basename(rasterFname)
  directoryName <- dirname(paste0(data.path, raster_template$filename)) #dirname(rasterFname)
  directoryNameFull <- sub("/$", "", path.expand(directoryName))
  pathToFile <- path.expand(paste0(data.path, raster_template$filename))
  
  # set distribution
  
  
  message('Setting distribution...')
  
  distribution <- lapply(raster_template$url,
         function(x) {
          dist <- EML::eml$distribution(
            EML::eml$online(url = x))
           return(dist)
           })
  
  # build physical
  message('building physical...')
  
  physical <- mapply(function(obj, a, s, f, d) {
    EML::eml$physical(
      objectName = obj,
      authentication = a,
      size = s,
      dataFormat = f,
      distribution = d
    )
  }, obj = raster_template$filename, a = file_auth, s = file_size, f = data_format, d = distribution)

  
  
  
  print('finals')
}
