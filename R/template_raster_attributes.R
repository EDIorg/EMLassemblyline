template_raster_attributes <- function(
  path = NULL,
  data.path = path,
  raster.files = NULL,
  empty = FALSE,
  write.file = TRUE,
  return.obj = FALSE) {
  
  message("Templating raster attributes ...")
  
  # Validate arguments --------------------------------------------------------
  
  validate_arguments(
    fun.name = 'tempate_raster_attributes',
    fun.args = as.list(environment()))

  # Create template table
  
    empty_output <- data.frame(
       filename = character(),
       desc = character(),
       geoDesc = character(),
       url = character(),
       def = character(),
       unit = character(),
       numberType = character(),
       stringsAsFactors = F
    )
    
    named_output <- data.frame(
      filename = raster.files,
      desc = rep("", length(raster.files)),
      geoDesc = rep("", length(raster.files)),
      url = rep("", length(raster.files)),
      def = rep("", length(raster.files)),
      unit = rep("", length(raster.files)),
      numberType = rep("", length(raster.files)),
      stringsAsFactors = F
    )
    
    write_raster_template <- function(output) {
      suppressWarnings(
        utils::write.table(
          output,
          paste0(path, "/", "raster_attributes.txt"),
          sep = "\t",
          row.names = F,
          quote = F,
          fileEncoding = "UTF-8"))
    }
    
  # Write table to file -----------------------------------------------------
  
  if (!is.null(path)) {
    
    if (is.null(raster.files) | isTRUE(empty)) {
      
      # (empty)
      write_raster_template(empty_output)  
    
    } else {
      
      write_raster_template(named_output)
    }
  }
  
  # Return values -------------------------------------------------------------
  
  message("Done.")
  
  if (isTRUE(return.obj)){
    
    if (is.null(raster.files) | isTRUE(empty)) {
      
      # (empty)
      return(empty_output)
      
    } else {
      
      return(named_output)
      
    }
  }
}