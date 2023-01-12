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
  
  # TODO compile data_entities
  data_entities <- something
  
  # TODO Data entity
  entity_templates <- vector(mode = "list", length = data_entities)
  names(entity_templates) <- names(data_entities)
  for (entity in data_entities) {
    entity_templates[[i]] <- template_data_entity()
  }
  
  # TODO Data entity attributes
  attribute_templates <- vector(mode = "list", length = data_entities)
  names(attribute_templates) <- names(data_entities)
  for (entity in data_entities) {
    attribute_templates[[i]] <- template_data_entity_attributes()
  }
  
  # TODO Custom units
  custom_units_template <- vector(mode = "list", length = 1)
  names(custom_units_template) <- "custom_units.txt"
  custom_units_template[[1]] <- template_data_entity_custom_units()
  
  # TODO compile templates into a list for return
  templates <- something
  
  if (write.file) {
    write_templates(templates, names(templates), path)
  } else {
    return(templates)
  }
}


template_data_entity <- function() {
  # TODO The superset of entity level metadata is attempted for each MIME Type.
  # Drop elements from superset that are not in the EML entity set.
  # Return data.frame
}


template_data_entity_attributes <- function() {
  # TODO Retrieval of attributes is informed by MIME Type
  # Return data.frame
}


template_data_entity_custom_units <- function() {
  # TODO Return an empty template. Alternatively, call this function after
  # attributes templates are completely filled out, then extract non-standard
  # units and put them in this file.
}