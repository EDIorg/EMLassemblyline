create_spatialVector <- function(path, data.path = path, vector_attributes = NULL) {
  
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
    
    warning(paste0("File '", paste(unlist(missing_descs)), "' does not have a definition in the vector_attributes template (Required).\n"), call. = F)
  }
  
  # Remove files with missing definitions from template
  
  vector_template <- vector_template[!(vector_template$extname %in% missing_descs),]
  
  
  
  
  
  
  
}