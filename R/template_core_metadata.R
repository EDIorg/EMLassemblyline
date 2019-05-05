#' Create core metadata templates
#'
#' @description  
#'     Import template files for storage of core metadata. Some templates are 
#'     populated with content detected by automated metadata extraction 
#'     methods. The remainder is supplied by the user.
#'     \href{https://ediorg.github.io/EMLassemblyline/articles/edit_metadata_templates.html}{Instructions for editing templates.}
#'
#' @usage 
#'     template_core_metadata(
#'       path,
#'       license,
#'       write.file = TRUE,
#'       x = NULL
#'     )
#'
#' @param path 
#'     (character) Path to the directory where the templates will be written.
#' @param license
#'     (character) License under which the data will be released. Use 
#'     \href{"CC0"}{https://creativecommons.org/publicdomain/zero/1.0/} or 
#'     \href{"CCBY"}{https://creativecommons.org/licenses/by/4.0/}.
#' @param write.file
#'     (logical; optional) Whether to write the templates to \code{path}.
#' @param x
#'     (named list; optional) Alternative input to \code{EMLassemblyline} 
#'     functions. Use \code{template_arguments()} to create \code{x}.
#'
#' @return 
#'     \itemize{
#'         \item{\strong{abstract.txt} Template for the dataset abstract.}
#'         \item{\strong{additional_info.txt} Template for miscellaneous dataset
#'         information.}
#'         \item{\strong{intellectual_rights.txt} The selected intellectual rights 
#'         license.}
#'         \item{\strong{keywords.txt} Template for dataset keywords. A tab 
#'         delimited table.}
#'         \item{\strong{methods.txt} Template for dataset methods.}
#'         \item{\strong{personnel.txt} Template for dataset personnel and funding 
#'         metadata. A tab delimited table.}
#'         \item{If using \code{x}, then content of each above listed template file 
#'         is added to \code{x} under \strong{/x/templates/}}
#'     }
#'     
#' @details 
#'     Existing templates will not be overwritten by \code{template_core_metadata()}.
#' 
#' @examples
#' # Set working directory
#' setwd(tempdir())
#' 
#' # Create data package directory "edi_250"
#' file.copy(
#'   from = system.file('/examples/edi_250', package = 'EMLassemblyline'),
#'   to = '.',
#'   recursive = TRUE
#' )
#' 
#' # View directory contents (NOTE: Directory is empty)
#' dir('./edi_250/metadata_templates')
#' 
#' # Template core metadata
#' template_core_metadata(
#'   path = './edi_250/metadata_templates',
#'   license = 'CC0'
#' )
#' 
#' # View directory contents (NOTE: Templates exist)
#' dir('./edi_250/metadata_templates')
#' 
#' # Rerunning template_core_metadata() does not overwrite files
#' template_core_metadata(
#'   path = './edi_250/metadata_templates',
#'   license = 'CC0'
#' )
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
      
      x$template$abstract.txt$content <- EML103::set_TextType(
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
      
      x$template$additional_info.txt$content <- EML103::set_TextType(
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
        
        x$template$intellectual_rights.txt$content <- EML103::set_TextType(
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
        
        x$template$intellectual_rights.txt$content <- EML103::set_TextType(
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
      
      x$template$methods.txt$content <- EML103::set_methods(
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


