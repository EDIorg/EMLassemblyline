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