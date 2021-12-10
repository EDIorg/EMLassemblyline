
create_spatialRaster <- function(path, data.path = path, raster_attributes = NULL, raster_catvars = NULL) {
  
  # If the user does not provide a direct path to a raster_attributes.txt template
  # AND does not have a raster_attributes.txt template at the path directory,
  # then this function can not proceed
  
  if (is.null(raster_template) & !file.exists(paste0(path, '/raster_attributes.txt'))) {
    
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
  
  if (!is.null(unlist(missing_defs))) {
    
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
  
  
  print('finals')
}
