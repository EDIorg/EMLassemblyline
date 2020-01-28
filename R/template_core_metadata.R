#' Create core metadata templates
#'
#' @description  
#'     Use this function to create the core metadata templates required by 
#'     all data packages. Most templates require user supplied information.
#'     \href{https://ediorg.github.io/EMLassemblyline/articles/edit_metadata_templates.html}{Instructions for editing these templates.}
#'
#' @usage 
#'     template_core_metadata(
#'       path,
#'       license,
#'       file.type = '.txt',
#'       write.file = TRUE,
#'       x = NULL
#'     )
#'
#' @param path 
#'     (character) Path to the metadata template directory.
#' @param license
#'     (character) License to publicly release the data package under. Use
#'     \href{https://creativecommons.org/publicdomain/zero/1.0/}{"CC0"} or 
#'     \href{https://creativecommons.org/licenses/by/4.0/}{"CCBY"}.
#' @param file.type
#'     (character; optional) File type for abstract, methods, and additional 
#'     info metadata templates. Can be: '.txt' (plain text), '.docx' (MS Word),
#'     or '.md' (markdown).
#' @param write.file
#'     (logical; optional) Whether to write the template files.
#' @param x
#'     (named list; optional) Alternative input to 
#'     \code{template_core_metadatta()}. Use \code{template_arguments()} 
#'     to create \code{x}.
#'
#' @return 
#'     \itemize{
#'         \item{\strong{abstract} The abstract template.}
#'         \item{\strong{additional_info} The template for miscellaneous
#'         information.}
#'         \item{\strong{intellectual_rights.txt} The intellectual rights 
#'         license with the text of CC0 or CCBY.}
#'         \item{\strong{keywords.txt} The tab delimited keywords template.}
#'         \item{\strong{methods} The methods template.}
#'         \item{\strong{personnel.txt} The tab delimited personnel template 
#'         for information on persons and funding involved in the creation of 
#'         the data package.}
#'         \item{These files is written to \code{path} unless using \code{x},
#'         in which case the templates are added to 
#'         \strong{/x/templates}.}
#'     }
#'     
#' @details 
#'     Existing templates will not be overwritten by \code{template_core_metadata()}.
#' 
#' @examples
#' # Initialize data package directory for template_core_metadata()
#' file.copy(
#'   from = system.file('/examples/pkg_250', package = 'EMLassemblyline'),
#'   to = tempdir(),
#'   recursive = TRUE
#' )
#' 
#' # Set working directory
#' setwd(paste0(tempdir(), '/pkg_250'))
#' 
#' # View directory contents (NOTE: Directory is empty)
#' dir('./metadata_templates')
#' 
#' # Template core metadata
#' template_core_metadata(
#'   path = './metadata_templates',
#'   license = 'CC0'
#' )
#' 
#' # View directory contents (NOTE: Templates exist)
#' dir('./metadata_templates')
#' 
#' # Rerunning template_core_metadata() does not overwrite files
#' template_core_metadata(
#'   path = './metadata_templates',
#'   license = 'CC0'
#' )
#' 
#' # Use docx files for abstract, methods, and additional_info
#' # First remove abstract, methods, and additional_info templates
#' file.remove(
#'   c(
#'     './metadata_templates/abstract.txt',
#'     './metadata_templates/methods.txt',
#'     './metadata_templates/additional_info.txt'
#'   )
#' )
#' 
#' # Create docx templates
#' template_core_metadata(
#'   path = './metadata_templates',
#'   license = 'CC0',
#'   file.type = '.docx'
#' )
#' 
#' # Use md files for abstract, methods, and additional_info
#' # First remove abstract, methods, and additional_info templates
#' file.remove(
#'   c(
#'     './metadata_templates/abstract.docx',
#'     './metadata_templates/methods.docx',
#'     './metadata_templates/additional_info.docx'
#'   )
#' )
#' 
#' # Create md templates
#' template_core_metadata(
#'   path = './metadata_templates',
#'   license = 'CC0',
#'   file.type = '.md'
#' )
#' 
#' # Clean up
#' unlink('.', recursive = TRUE)
#'     
#' @export     
#'     

template_core_metadata <- function(
  path, 
  license, 
  file.type = '.txt', 
  write.file = TRUE,
  x = NULL){
  
  message('Templating core metadata ...')
  
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
    
  } else {
    
    write.file <- FALSE
    
  }
  
  # Import abstract -----------------------------------------------------------
  
  # If writing to file ...
  
  if (isTRUE(write.file)){

    # Write to path
    
    value <- file.copy(
      from = system.file(
        paste0(
          '/templates/abstract',
          file.type
        ),
        package = 'EMLassemblyline'
      ),
      to = paste0(
        path,
        paste0(
          '/abstract',
          file.type
        )
      )
    )
    
    # Send message
    
    if (isTRUE(value)){
      message(paste0('abstract', file.type))
    } else {
      message(paste0('abstract', file.type, ' already exists!'))
    }
    
    # If adding to x ...
    
  } else if (!exists('data_read_2_x')){
    
    if (any(is.na(x$template[['abstract']]$content))){
      
      # Add to content
      
      x$template[['abstract']]$content <- EML::set_TextType(
        file = system.file(
          paste0(
            '/templates/abstract',
            file.type
          ),
          package = 'EMLassemblyline'
        )
      )
      
      # Send message
      
      message(paste0('abstract', file.type))
      
    } else {
      
      message(paste0('abstract', file.type, ' already exists!'))
      
    }
    
  }
  
  # Import additional_info ----------------------------------------------------
  
  # If writing to file ... 
  
  if (isTRUE(write.file)){
    
    # Write to path
    
    value <- file.copy(
      from = system.file(
        paste0(
          '/templates/additional_info',
          file.type
        ),
        package = 'EMLassemblyline'
      ),
      to = paste0(
        path,
        paste0(
          '/additional_info',
          file.type
        )
      )
    )
    
    # Send message
    
    if (isTRUE(value)){
      message(paste0('additional_info', file.type))
    } else {
      message(paste0('additional_info', file.type, ' already exists!'))
    }
    
    # If adding to x ...
    
  } else if (!exists('data_read_2_x')){
    
    if (any(is.na(x$template[['additional_info']]$content))){
      
      # Add to content
      
      x$template[['additional_info']]$content <- EML::set_TextType(
        file = system.file(
          paste0(
            '/templates/additional_info',
            file.type
          ),
          package = 'EMLassemblyline'
        )
      )
      
      # Send message
      
      message(paste0('additional_info', file.type))
      
    } else {
      
      message(paste0('additional_info', file.type, ' already exists!'))
      
    }
    
  }
  
  # Import attributes_dataset.txt ---------------------------------------------
  
  # If writing to file ... 
  
  if (isTRUE(write.file)){
    
    # Write to path
    
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
    
    # Send message
    
    if (isTRUE(value)){
      message('attributes_dataset.txt')
    } else {
      message(paste0('attributes_dataset.txt', ' already exists!'))
    }
    
    # If adding to x ...
    
  } else if (!exists('data_read_2_x')){
    
    if (any(is.na(x$template[['attributes_dataset']]$content))){
      
      # Add to content
      
      x$template$attributes_dataset.txt$content <- as.data.frame(
        data.table::fread(
          file = system.file(
            '/templates/attributes_dataset.txt',
            package = 'EMLassemblyline'
          ),
          colClasses = rep(
            "character",
            max(
              utils::count.fields(
                system.file(
                  '/templates/attributes_dataset.txt',
                  package = 'EMLassemblyline'
                ),
                sep = "\t"
              )
            )
          ),
          fill = TRUE,
          blank.lines.skip = TRUE
        )
      )
      
      # Send message
      
      message('attributes_dataset')
      
    } else {
      
      message(paste0('attributes_dataset', ' already exists!'))
      
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
        message("intellectual_rights.txt")
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
        
        message("intellectual_rights.txt")
        
      } else {
        
        message("intellectual_rights.txt already exists!")
        
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
        message("intellectual_rights.txt")
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
        
        message("intellectual_rights.txt")
        
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
      message("keywords.txt.")
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
      
      message("keywords.txt")
      
    } else {
      
      message("keywords.txt already exists!")
      
    }
    
  }
  
  # Import methods ------------------------------------------------------------
  
  # If writing to file ...
  
  if (isTRUE(write.file)){
    
    # Write to path
    
    value <- file.copy(
      from = system.file(
        paste0(
          '/templates/methods',
          file.type
        ),
        package = 'EMLassemblyline'
      ),
      to = paste0(
        path,
        paste0(
          '/methods',
          file.type
        )
      )
    )
    
    # Send message
    
    if (isTRUE(value)){
      message(paste0('methods', file.type))
    } else {
      message(paste0('methods', file.type, ' already exists!'))
    }
    
    # If adding to x ...
    
  } else if (!exists('data_read_2_x')){
    
    if (any(is.na(x$template[['methods']]$content))){
      
      # Add to content
      
      x$template[['methods']]$content <- EML::set_methods(
        methods_file = system.file(
          paste0(
            '/templates/methods',
            file.type
          ),
          package = 'EMLassemblyline'
        )
      )
      
      # Send message
      
      message(paste0('methods', file.type))
      
    } else {
      
      message(paste0('methods', file.type, ' already exists!'))
      
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
      message("personnel.txt")
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
      
      message("personnel.txt")
      
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


