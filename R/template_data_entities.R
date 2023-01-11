template_data_entities <- function(
    path,
    data.path = path,
    data.table,
    spatial.raster,
    spatial.vector,
    other.entity,
    write.file = TRUE) {
  
  message("Templating data entities")
  # TODO assign data object parameters NULL value if missing
  validate_arguments(
    fun.name = "template_data_entities",
    fun.args = as.list(environment())
  )
  
  # TODO Add spatial.raster and spatial.vector parameters to template_arguments()
  x <- template_arguments(
    path = path,
    data.path = data.path,
    data.table = data.table,
    spatial.raster = spatial.raster,
    spatial.vector = spatial.vector,
    other.entity = other.entity,
  )$x
  
}


template_data_entity <- function() {
  # TODO The superset of entity level metadata is attempted for each MIME Type.
  # Drop elements from superset that are not in the EML entity set.
  # Return list
}


template_data_entity_attributes <- function() {
  # TODO Retrieval of attributes is informed by MIME Type
  # Return data.frame
}


template_data_entity_custom_units <- function() {
  # TODO Return an empty template
}