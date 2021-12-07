template_raster_attributes <- function(
  path = NULL, raster.files = NULL, empty = TRUE, write.file = TRUE, return.obj = FALSE) {
  
  message("Templating raster attributes ...")
  
  # Validate arguments --------------------------------------------------------
  
  validate_arguments(
    fun.name = 'tempate_raster_attributes',
    fun.args = as.list(environment()))
  
  # Write to file -------------------------------------------------------------
  
  if (!is.null(path)) {
    invisible(
      file.copy(
        from = system.file(
          "/templates/raster_attributes.txt", 
          package = "EMLassemblyline"),
        to = path, 
        overwrite = FALSE))
  }
  
  # Return values -------------------------------------------------------------
  
  message("Done.")
  
  if (isTRUE(return.obj)){
    return(
      data.table::fread(
        system.file(
          "/templates/raster_attributes.txt", 
          package = "EMLassemblyline")))
  }
  
}