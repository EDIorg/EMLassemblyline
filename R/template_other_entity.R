#' Create other entity attributes template
#'
#' @description  
#'     Implementation of this function is not yet complete.
#'     Use this function to extract attributes from data objects classified as
#'     "other entity" (i.e. data that are not tables, spatial raster, or
#'     spatial vector types) and return for user supplied definitions. 
#'
#' @usage 
#'     template_other_entity_attributes(
#'       path,
#'       data.path = path,
#'       other.entity = NULL,
#'       write.file = TRUE,
#'       x = NULL
#'     )
#'
#' @param path 
#'     (character) Path to the metadata template directory.
#' @param data.path
#'     (character) Path to the data directory.
#' @param other.entity
#'     (character) Other entity name. If more than one, then supply as a 
#'     vector of character strings (e.g. 
#'     \code{other.entity = c('maps.zip', 'analysis_script.R')}).
#' @param write.file
#'     (logical; optional) Whether to write the template file.
#' @param x
#'     (named list; optional) Alternative input to 
#'     \code{template_table_attributes()}. Use \code{template_arguments()} 
#'     to create \code{x}.
#'
#' @return 
#'     \itemize{
#'         \item{\strong{attributes_dataset.txt} The tab delimited dataset 
#'         attributes template updated with other entity IDs, file names, and 
#'         additional rows for specifying other entity names and descriptions.
#'         This file is written to \code{path} unless using \code{x}, 
#'         in which case the updated template is added to 
#'         \strong{/x/templates/attributes_*.txt}.}
#'         \item{\strong{attributes_other_entity.txt} (coming soon!) The tab 
#'         delimited other entity template for defining other entity attributes,
#'         written to \code{path} unless using \code{x}, in which case the 
#'         template is added to 
#'         \strong{/x/templates/attributes_other_entity.txt}.}
#'     }
#'     
#' @details 
#'     An existing attributes template will not be overwritten by subsequent 
#'     calls to \code{template_other_entity_attributes()}.
#'     
#' @examples 
#'     
#' @export     
#'     

template_other_entity_attributes <- function(
  path, 
  data.path = path, 
  other.entity = NULL, 
  write.file = TRUE,
  x = NULL){
  
  message('Templating other entity attributes ...')
  
  # Validate arguments --------------------------------------------------------
  
  # Validate path usage before passing arguments to validate_arguments()
  # When not using x, inputs are expected from path and data.path. When using x, 
  # only data.path is required.
  
  if (is.null(x) & missing(path)){
    stop('Input argument "path" is missing.')
  } else if (!is.null(x) & missing(path)){
    path <- NULL
    if (missing(data.path)){
      data.path <- NULL
    }
  }
  
  # Pass remaining arguments to validate_arguments().
  
  validate_arguments(
    fun.name = 'template_other_entity_attributes',
    fun.args = as.list(environment())
  )
  
  # Read metadata templates and data ------------------------------------------
  
  # If x doesn't exist ...
  
  if (is.null(x)){
    
    # If other entities are absent ...
    
    if (is.null(other.entity)){
      
      # Use NULL values
      
      x <- template_arguments()
      
      x <- x$x
      
      # If other entities are present ...
      
    } else if (!is.null(other.entity)){
      
      # Validate other.entity
      
      other.entity <- suppressWarnings(
        EDIutils::validate_file_names(
          path = data.path, 
          data.files = other.entity
        )
      )
      
      # Read other entities
      
      x <- template_arguments(
        data.path = data.path,
        other.entity = other.entity
      )
      
      x <- x$x
      
    }
    
    # Indicate files have been read
    
    data_read_2_x <- TRUE
    
  }

  # Import/Update attributes_dataset.txt --------------------------------------
  # The attributes_dataset.txt template may not exist yet, create it if it 
  # doesn't exist.
  
  if (isTRUE(write.file)) {
    
    # Write attributes_dataset.txt to file if it doesn't exist
    
    if (any(is.na(x$template[['attributes_dataset.txt']]$content))){
      
      value <- file.copy(
        from = system.file(
          '/templates/attributes_dataset.txt',
          package = 'EMLassemblyline'
        ),
        to = paste0(
          path,
          '/attributes_dataset.txt'
        )
      )
      
      if (isTRUE(value)){
        message('attributes_dataset.txt')
      } else {
        message(paste0('attributes_dataset.txt', ' already exists!'))
      }
      
    }
    
    # Add other entities to attributes_dataset.txt
    
    df <- as.data.frame(
      data.table::fread(
        file = paste0(path, '/attributes_dataset.txt'),
        colClasses = rep(
          "character",
          max(
            utils::count.fields(
              paste0(path, '/attributes_dataset.txt'),
              sep = "\t"
            )
          )
        ),
        fill = TRUE,
        blank.lines.skip = TRUE
      )
    )
    
    for (i in 1:length(other.entity)) {
      if (!(other.entity[i] %in% df$value)) {
        id <- uuid::UUIDgenerate(use.time = TRUE)
        df[(nrow(df)+1):(nrow(df)+3), ] <- data.frame(
          id = rep(id, 3),
          element = c("otherEntity", "otherEntityName", "otherEntityDescription"),
          value = c(other.entity[i], "", ""),
          stringsAsFactors = FALSE
        )
      }
    }
    
    data.table::fwrite(
      x = df,
      file = paste0(path, '/attributes_dataset.txt'),
      sep = "\t",
      quote = FALSE
    )
    
    message("Updating attributes_dataset.txt")
    
    # If adding to x ...
    
  } else if (!exists('data_read_2_x')){
    
    # if (any(is.na(x$template$attributes_dataset.txt$content))) {
    #   
    #   df <- as.data.frame(
    #     data.table::fread(
    #       file = system.file(
    #         '/templates/attributes_dataset.txt',
    #         package = 'EMLassemblyline'
    #       ),
    #       colClasses = rep(
    #         "character",
    #         max(
    #           utils::count.fields(
    #             system.file(
    #               '/templates/attributes_dataset.txt',
    #               package = 'EMLassemblyline'
    #             ),
    #             sep = "\t"
    #           )
    #         )
    #       ),
    #       fill = TRUE,
    #       blank.lines.skip = TRUE
    #     )
    #   )
    #   
    #   for (i in length(other.entity)) {
    #     id <- uuid::UUIDgenerate(use.time = TRUE)
    #     df <- rbind(
    #       df,
    #       data.frame(
    #         id = rep(id, 3),
    #         element = c("data table", "data table name", "data table description"),
    #         value = c(other.entity[i], "", ""),
    #         stringsAsFactors = FALSE
    #       )
    #     )
    #   }
    #   
    #   x$template$attributes_dataset.txt$content <- df
    #   
    #   # Send message
    #   
    #   message("attributes_dataset.txt")
    #   
    # } else {
    #   
    #   message("attributes_dataset.txt already exists!")
    #   
    # }
    
  }
  
  
  # Return --------------------------------------------------------------------
  
  if ((!isTRUE(write.file)) & is.null(x)){
    message('No templates were written to file (write.file = FALSE).')
  }
  
  message("Done.")
  
  if (!exists('data_read_2_x')){
    return(x)
  }
  
}


