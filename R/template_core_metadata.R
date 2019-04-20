#' Create core metadata templates
#'
#' @description  
#'     Import template files for storage of core metadata. Some templates are 
#'     populated with content detected by automated metadata extraction 
#'     methods. The remainder will have to be manually entered by a human. 
#'     Instructions for filling out the templates are at
#'     \url{https://clnsmth.github.io/EMLassemblyline/articles/instructions.html}.
#'
#' @usage 
#'     template_core_metadata(
#'       path,
#'       license,
#'       x = NULL,
#'       write.file = TRUE
#'     )
#'
#' @param path 
#'     (character) Path to where the templates will be imported.
#' @param license
#'     (character) License under which the data will be released. Use "CC0" 
#'     (\url{https://creativecommons.org/publicdomain/zero/1.0/}), 
#'     or "CCBY" (\url{https://creativecommons.org/licenses/by/4.0/}).
#' @param x
#'     (named list) Alternative input/output to `EMLassemblyline` functions. 
#'     Use \code{template_arguments} to create `x`.
#' @param write.file
#'     (logical) Write template files to `path`.
#'
#' @return 
#'     \itemize{
#'         \item{`abstract.txt`} Template for the dataset abstract.
#'         \item{`additional_info.txt`} Template for miscellaneous dataset
#'         information.
#'         \item{`bounding_boxes.txt`} Template for the dataset geographic 
#'         bounding coordinates.
#'         \item{`custom_units.txt`} Template for defining non-standard units 
#'         used in the dataset.
#'         \item{`intellectual_rights.txt`} The selected intellectual rights 
#'         license.
#'         \item{`keywords.txt`} Template for dataset keywords.
#'         \item{`methods.txt`} Template for dataset methods.
#'         \item{`personnel.txt`} Template for dataset personnel and funding 
#'         metadata.
#'         \item{If using `x`, then content of each above listed template file 
#'         is added to `x` under `/x/templates/`}
#'     }
#'     
#' @details 
#'     Existing templates will not be overwritten by `template_core_metadata`.
#'     
#' @export     
#'     

template_core_metadata <- function(path, license, x = NULL, write.file = TRUE){
  
  message('Creating core metadata templates')
  
  # Validate arguments --------------------------------------------------------
  
  # Validate path usage before passing arguments to validate_arguments()
  # When not using x, inputs are expected from path and data.path. When using x, 
  # only data.path is required.
  
  if (is.null(x) & missing(path)){
    stop('Input argument "path" is missing.')
  }
  
  # Pass remaining arguments to validate_arguments().
  
  validate_arguments(
    fun.name = 'template_core_metadata',
    fun.args = as.list(environment())
  )
  
  # Read metadata templates and data ------------------------------------------
  
  # If x doesn't exist ...
  
  if (is.null(x)){
      
    # Use NULL values
    
    x <- template_arguments()
    
    x <- x$x

    # Indicate files have been read
    
    data_read_2_x <- TRUE
    
  }
  
  # Import abstract.txt -------------------------------------------------------
  
  # If writing to file ...
  
  if (isTRUE(write.file)){
    
    # Write to path
    
    value <- file.copy(
      from = system.file(
        '/templates/abstract.txt',
        package = 'EMLassemblyline'
      ),
      to = paste0(
        path,
        "/abstract.txt"
      )
    )
    
    # Send message
    
    if (isTRUE(value)){
      message("Importing abstract.txt.")
    } else {
      message("abstract.txt already exists!")
    }
    
    # If adding to x ...
    
  } else if (!exists('data_read_2_x')){
    
    if (any(is.na(x$template$abstract.txt$content))){
      
      # Add to content
      
      x$template$abstract.txt$content <- EML::set_TextType(
        file = system.file(
          '/templates/abstract.txt',
          package = 'EMLassemblyline'
        )
      )
      
      # Send message
      
      message("Importing abstract.txt.")
      
    } else {
      
      message("abstract.txt already exists!")
      
    }
    
  }
  
  # Import additional_info.txt ------------------------------------------------
  
  # If writing to file ... 
  
  if (isTRUE(write.file)){
    
    # Write to path
    
    value <- file.copy(
      from = system.file(
        '/templates/additional_info.txt',
        package = 'EMLassemblyline'
      ),
      to = paste0(
        path,
        "/additional_info.txt"
      )
    )
    
    # Send message
    
    if (isTRUE(value)){
      message("Importing additional_info.txt.")
    } else {
      message("additional_info.txt already exists!")
    }
    
    # If adding to x ...
    
  } else if (!exists('data_read_2_x')){
    
    if (any(is.na(x$template$additional_info.txt$content))){
      
      # Add to content
      
      x$template$additional_info.txt$content <- EML::set_TextType(
        file = system.file(
          '/templates/additional_info.txt',
          package = 'EMLassemblyline'
        )
      )
      
      # Send message
      
      message("Importing additional_info.txt")
      
    } else {
      
      message("additional_info.txt already exists!")
      
    }
    
  }
  
  # Import bounding_boxes.txt -------------------------------------------------
  
  # If writing to file ...
  
  if (isTRUE(write.file)){
    
    # Write to path
    
    value <- file.copy(
      from = system.file(
        '/templates/bounding_boxes.txt',
        package = 'EMLassemblyline'
      ),
      to = paste0(
        path,
        "/bounding_boxes.txt"
      )
    )
    
    # Send message
    
    if (isTRUE(value)){
      message("Importing bounding_boxes.txt.")
    } else {
      message("bounding_boxes.txt already exists!")
    }
    
    # If adding to x ...
    
  } else if (!exists('data_read_2_x')){
    
    if (any(is.na(x$template$bounding_boxes.txt$content))){
      
      # Add to content
      
      x$template$bounding_boxes.txt$content <- utils::read.table(
        file = system.file(
          '/templates/bounding_boxes.txt',
          package = 'EMLassemblyline'
        ), 
        header = T,
        sep = '\t',
        as.is = T
      )
      
      # Send message
      
      message("Importing bounding_boxes.txt.")
      
    } else {
      
      message("bounding_boxes.txt already exists!")
      
    }
    
  }
  
  # Import custom_units.txt ---------------------------------------------------
  
  # If writing to file ...
  
  if (isTRUE(write.file)){
    
    # Write to path
    
    value <- file.copy(
      from = system.file(
        '/templates/custom_units.txt',
        package = 'EMLassemblyline'
      ),
      to = paste0(
        path,
        "/custom_units.txt"
      )
    )
    
    # Send message
    
    if (isTRUE(value)){
      message("Importing custom_units.txt.")
    } else {
      message("custom_units.txt already exists!")
    }
    
    # If adding to x ...
    
  } else if (!exists('data_read_2_x')){
    
    if (any(is.na(x$template$custom_units.txt$content))){
      
      # Add to content
      
      x$template$custom_units.txt$content <- utils::read.table(
        file = system.file(
          '/templates/custom_units.txt',
          package = 'EMLassemblyline'
        ), 
        header = T,
        sep = '\t',
        as.is = T
      )
      
      # Send message
      
      message("Importing custom_units.txt.")
      
    } else {
      
      message("custom_units.txt already exists!")
      
    }
    
  }
  
  # Import intellectual_rights.txt --------------------------------------------
  
  # If license is CC0 ...
  
  if (tolower(license) == "cc0"){
    
    # If writing to file ...
    
    if (isTRUE(write.file)){
      
      # Write to path
      
      value <- file.copy(
        from = system.file(
          '/templates/intellectual_rights_cc0.txt',
          package = 'EMLassemblyline'
        ),
        to = paste0(
          path,
          "/intellectual_rights.txt"
        )
      )
      
      # Send message
      
      if (isTRUE(value)){
        message("Importing intellectual_rights.txt.")
      } else {
        message("intellectual_rights.txt already exists!")
      }
      
      # If adding x ...
      
    } else if (!exists('data_read_2_x')){
      
      if (any(is.na(x$template$intellectual_rights.txt$content))){
        
        # Add to content
        
        x$template$intellectual_rights.txt$content <- EML::set_TextType(
          file = system.file(
            '/templates/intellectual_rights_cc0.txt',
            package = 'EMLassemblyline'
          )
        )
        
        # Send message
        
        message("Importing intellectual_rights.txt.")
        
      } else {
        
        message("intellectual_rights.txt. already exists!")
        
      }
      
    }
    
    # If license is CCBY ...
    
  } else if (tolower(license) == "ccby"){
    
    # If writing to file ...
    
    if (isTRUE(write.file)){
      
      # Write to path
      
      value <- file.copy(
        from = system.file(
          '/templates/intellectual_rights_ccby4.0.txt',
          package = 'EMLassemblyline'
        ),
        to = paste0(
          path,
          "/intellectual_rights.txt"
        )
      )
      
      # Send message
      
      if (isTRUE(value)){
        message("Importing intellectual_rights.txt.")
      } else {
        message("intellectual_rights.txt already exists!")
      }
      
      # If adding to x ...
      
    } else if (!exists('data_read_2_x')){
      
      if (any(is.na(x$template$intellectual_rights.txt$content))){
        
        # Add to content
        
        x$template$intellectual_rights.txt$content <- EML::set_TextType(
          file = system.file(
            '/templates/intellectual_rights_ccby4.0.txt',
            package = 'EMLassemblyline'
          )
        )
        
        # Send message
        
        message("Importing intellectual_rights.txt.")
        
      } else {
        
        message("intellectual_rights.txt already exists!")
        
      }
      
    }
    
  }
  
  # Import keywords.txt -------------------------------------------------------
  
  # If writing to file ...
  
  if (isTRUE(write.file)){
    
    # Write to path
    
    value <- file.copy(
      from = system.file(
        '/templates/keywords.txt',
        package = 'EMLassemblyline'
      ),
      to = paste0(
        path,
        "/keywords.txt"
      )
    )
    
    # Send message
    
    if (isTRUE(value)){
      message("Importing keywords.txt.")
    } else {
      message("keywords.txt already exists!")
    }
    
    # If adding to x ...
    
  } else if (!exists('data_read_2_x')){
    
    if (any(is.na(x$template$keywords.txt$content))){
      
      # Add to content
      
      x$template$keywords.txt$content <- utils::read.table(
        file = system.file(
          '/templates/keywords.txt',
          package = 'EMLassemblyline'
        ), 
        header = T,
        sep = '\t',
        as.is = T
      )
      
      # Send message
      
      message("Importing keywords.txt.")
      
    } else {
      
      message("keywords.txt already exists!")
      
    }
    
  }
  
  # Import methods.txt --------------------------------------------------------
  
  # If writing to file ...
  
  if (isTRUE(write.file)){
    
    # Write to path
    
    value <- file.copy(
      from = system.file(
        '/templates/methods.txt',
        package = 'EMLassemblyline'
      ),
      to = paste0(
        path,
        "/methods.txt"
      )
    )
    
    # Send message
    
    if (isTRUE(value)){
      message("Importing methods.txt.")
    } else {
      message("methods.txt already exists!")
    }
    
    # If adding to x ...
    
  } else if (!exists('data_read_2_x')){
    
    if (any(is.na(x$template$methods.txt$content))){
      
      # Add to content
      
      x$template$methods.txt$content <- EML::set_methods(
        methods_file = system.file(
          '/templates/methods.txt',
          package = 'EMLassemblyline'
        )
      )
      
      # Send message
      
      message("Importing methods.txt.")
      
    } else {
      
      message("methods.txt already exists!")
      
    }
    
  }
  
  # Import personnel.txt ------------------------------------------------------
  
  # If writing to file ...
  
  if (isTRUE(write.file)){
    
    # Write to path
    
    value <- file.copy(
      from = system.file(
        '/templates/personnel.txt',
        package = 'EMLassemblyline'
      ),
      to = paste0(
        path,
        "/personnel.txt"
      )
    )
    
    # Send message
    
    if (isTRUE(value)){
      message("Importing personnel.txt.")
    } else {
      message("personnel.txt already exists!")
    }
    
    # If adding to x ...
    
  } else if (!exists('data_read_2_x')){
    
    if (any(is.na(x$template$personnel.txt$content))){
      
      # Add to content
      
      x$template$personnel.txt$content <- utils::read.table(
        file = system.file(
          '/templates/personnel.txt',
          package = 'EMLassemblyline'
        ), 
        header = T,
        sep = '\t',
        as.is = T
      )
      
      # Send message
      
      message("Importing personnel.txt.")
      
    } else {
      
      message("personnel.txt already exists!")
      
    }
    
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


