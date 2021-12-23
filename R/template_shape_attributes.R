# TODO This should really be combined with template_raster_attributes into a single
## function template_spatial_attributes

template_shape_attributes <- function(
  path = NULL,
  data.path = path,
  shape.files = NULL,
  empty = FALSE,
  write.file = TRUE,
  return.obj = FALSE) {
  
  message("Templating shape attributes ...")
  
  # Validate arguments --------------------------------------------------------
  
  validate_arguments(
    fun.name = 'tempate_raster_attributes',
    fun.args = as.list(environment()))
  
  # Create template table
  
  empty_output <- data.frame(
    extname = character(),
    description = character(),
    geoDescription = character(),
    url = character(),
    stringsAsFactors = F
  )
  
  named_output <- data.frame(
    extname = tools::file_path_sans_ext(shape.files),
    description = rep("", length(shape.files)),
    geoDescription = rep("", length(shape.files)),
    url = rep("", length(shape.files)),
    stringsAsFactors = F
  )
  
  write_shape_template <- function(output) {
    suppressWarnings(
      utils::write.table(
        output,
        paste0(path, "/", "shape_attributes.txt"),
        sep = "\t",
        row.names = F,
        quote = F,
        fileEncoding = "UTF-8"))
  }
  # Write table to file -----------------------------------------------------
  
  if (!is.null(path)) {
    
    if (is.null(shape.files) | isTRUE(empty)) {
      
      # (empty)
      write_shape_template(empty_output)  
      
    } else {
      
      write_shape_template(named_output)
    }
  }
  
  # Return values -------------------------------------------------------------
  
  message("Done.")
  
  if (isTRUE(return.obj)){
    
    if (is.null(shape.files) | isTRUE(empty)) {
      
      # (empty)
      return(empty_output)
      
    } else {
      
      return(named_output)
      
    }
  }
  
}