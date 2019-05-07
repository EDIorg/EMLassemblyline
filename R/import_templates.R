#' Import core metadata and table attribute templates
#'
#' @description  
#'     Import template files for storage of core metadata and table attributes. 
#'     Some templates are populated with content detected by automated metadata 
#'     extraction methods. The remainder is manually entered by the user. 
#'     \href{https://ediorg.github.io/EMLassemblyline/articles/edit_metadata_templates.html}{Instructions for editing the templates.}
#'
#' @usage 
#'     import_templates(
#'       path,
#'       data.path = path,
#'       license,
#'       data.table = NULL,
#'       write.file = TRUE,
#'       x = NULL
#'     )
#'
#' @param path 
#'     (character) Path to the directory where the templates will be written.
#' @param data.path
#'     (character) Path to the directory where the data tables are stored.
#' @param license
#'     (character) License under which the data will be released. Use 
#'     \href{"CC0"}{https://creativecommons.org/publicdomain/zero/1.0/} or 
#'     \href{"CCBY"}{https://creativecommons.org/licenses/by/4.0/}.
#' @param data.table
#'     (character) Name of data table. If more than one, then supply as a 
#'     vector of character strings (e.g. 
#'     \code{data.table = c('concentrations.csv', 'characteristics.csv')}).
#' @param write.file
#'     (logical; optional) Whether to write the templates to \code{path}.
#' @param x
#'     (named list; optional) Alternative input to \code{EMLassemblyline} 
#'     functions. Use \code{template_arguments()} to create \code{x}.
#'
#' @return 
#'     \itemize{
#'         \item{\strong{abstract.txt} Template for the dataset abstract.}
#'         \item{\strong{additional_info.txt`} Template for miscellaneous dataset
#'         information.}
#'         \item{\strong{attributes_*.txt} Template(s) for data table attribute 
#'         metadata, populated with some automatically extracted metadata 
#'         content from the data tables. A tab delimited table.}
#'         \item{\strong{bounding_boxes.txt} Template for the dataset geographic 
#'         bounding coordinates. A tab delimited table.}
#'         \item{\strong{custom_units.txt} Template for defining non-standard units 
#'         used in data tables. A tab delimited table.}
#'         \item{\strong{intellectual_rights.txt} The selected intellectual rights 
#'         license.}
#'         \item{\strong{keywords.txt} Template for dataset keywords. A tab 
#'         delimited table.}
#'         \item{\strong{methods.txt} Template for dataset methods.}
#'         \item{\strong{personnel.txt} Template for dataset personnel and funding 
#'         metadata. A tab delimited table.}
#'         \item{If using \code{x}, then the templates are added to 
#'         \strong{/x/templates}.}
#'     }
#'     
#' @details 
#'     Existing templates will not be overwritten subsequent calls to 
#'     \code{import_templates()}.
#'     
#' @examples 
#' # Set working directory
#' setwd(tempdir())
#' 
#' # Create data package directory "pkg_250"
#' file.copy(
#'   from = system.file('/examples/pkg_250', package = 'EMLassemblyline'),
#'   to = tempdir(),
#'   recursive = TRUE
#' )
#'
#' # View directory contents (NOTE: Directory is empty)
#' dir('./pkg_250/metadata_templates')
#'
#' # Import templates (for data package with no tables)
#' import_templates(
#'   path = './pkg_250/metadata_templates',
#'   license = 'CC0'
#' )
#' 
#' # View directory contents (NOTE: Templates exist)
#' dir('./pkg_250/metadata_templates')
#' 
#' # Rerunning import_templates() does not overwrite files
#' import_templates(
#'   path = './pkg_250/metadata_templates',
#'   license = 'CC0'
#' )
#' 
#' # Remove templates from directory
#' file.remove(
#'   list.files('./pkg_250/metadata_templates', full.names = TRUE)
#' )
#' 
#' # Import templates (for data package with tables)
#' import_templates(
#'   path = './pkg_250/metadata_templates',
#'   data.path = './pkg_250/data_objects',
#'   data.table = c(
#'     'decomp.csv',
#'     'nitrogen.csv'
#'   ),
#'   license = 'CC0'
#' )
#' 
#' # View directory contents (NOTE: Templates, including attributes*_.txt, exist)
#' dir('./pkg_250/metadata_templates')
#' 
#' # Clean up
#' unlink(
#'   './pkg_250',
#'   recursive = TRUE
#' )
#'           
#' @export
#'     

import_templates <- function(path, data.path = path, license, 
                             data.table = NULL, x = NULL, write.file = TRUE,
                             data.files){
  
  message('Importing metadata templates')
  
  # Send deprecation notice ---------------------------------------------------
  
  .Deprecated(
    new = 'template_core_metadata and template_table_attributes',
    package = 'EMLassemblyline',
    old = 'import_templates'
  )

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
    fun.name = 'import_templates',
    fun.args = as.list(environment())
  )
  
  # Handle deprecated arguments
  
  if (!missing(data.files)){
    
    warning(
      'Argument "data.files" is deprecated; please use "data.table" instead.',
      call. = FALSE)
    
    data.table <- data.files
    
  }
  
  # Read metadata templates and data ------------------------------------------
  
  # If x doesn't exist ...
  
  if (is.null(x)){
    
    # If data tables are absent ...
    
    if (is.null(data.table)){
      
      # Use NULL values
      
      x <- template_arguments()
      
      x <- x$x
      
    # If data tables are present ...
      
    } else if (!is.null(data.table)){
      
      # Read data tables

      x <- template_arguments(
        data.path = data.path,
        data.table = data.table
      )
      
      x <- x$x

    }
    
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

  # Import attributes_*.txt ---------------------------------------------------
  
  if (!is.null(data.table)){

    # Validate column names
    
    for (i in 1:length(data.table)){

      column_names <- colnames(x$data.table[[i]]$content)
      
      use_i <- stringr::str_detect(
        string = column_names,
        pattern = "\\."
      )
      
      if (sum(use_i) > 0){
        stop(
          paste(
            "Invalid column names detected in ", 
            names(x$data.table)[i],
            ":  ",
            paste(
              column_names[use_i], 
              collapse = ", "
            ), 
            '  Replace characters located at periods "." in the above listed column names with underscores "_"',
            sep = ""
          )
        )
      }
      
    }

    # Extract attributes of each data file
    
    attributes <- list()
    
    for (i in 1:length(data.table)){

      # Initialize attribute table
      
      rows <- ncol(x$data.table[[i]]$content)
      
      attributes[[i]] <- data.frame(
        attributeName = character(rows),
        attributeDefinition = character(rows),
        class = character(rows),
        unit = character(rows),
        dateTimeFormatString = character(rows),
        missingValueCode = character(rows),
        missingValueCodeExplanation = character(rows),
        stringsAsFactors = FALSE
      )
      
      # Get names
      
      attributes[[i]]$attributeName <- colnames(x$data.table[[i]]$content)
      
      # Guess character and numeric classes
      
      guess <- unname(unlist(lapply(x$data.table[[i]]$content, class)))
      
      guess_map <- c(
        character = "character", 
        logical = "character", 
        factor = "character",
        integer = "numeric",
        numeric = "numeric"
      )
      
      guess <- unname(guess_map[guess])
      
      # Guess Date class
      
      use_i <- guess == "character"
      
      if (sum(use_i) > 0){
        potential_date_cols <- colnames(x$data.table[[i]]$content)[use_i]
        potential_date_i <- stringr::str_detect(tolower(potential_date_cols), "date|time|day")
        guess_datetime <- potential_date_cols[potential_date_i]
        use_i <- match(guess_datetime, attributes[[i]]$attributeName)
        guess[use_i] <- "Date"
      }
      
      # Guess factor class
      
      use_i <- guess == "character"
      if (sum(use_i) > 0){
        potential_fact_cols <- colnames(x$data.table[[i]]$content)[use_i]
        use_i2 <- match(potential_fact_cols, colnames(x$data.table[[i]]$content))
        if (length(use_i2) == 1){
          unique_lengths <- length(unique(x$data.table[[i]]$content[ ,use_i2]))
        } else {
          unique_lengths <- apply(x$data.table[[i]]$content[ ,use_i2], 2, function(x)length(unique(x)))
        }
        potential_facts <- unique_lengths <= dim(x$data.table[[i]]$content)[1]*0.3
        if (sum(potential_facts) > 0){
          potential_facts <- names(potential_facts[potential_facts == TRUE])
          use_i <- match(potential_facts, attributes[[i]]$attributeName)
          guess[use_i] <- "categorical"
        }
      }
      
      # Update attributes class
      
      attributes[[i]]$class <- guess
      
      # Add unit for numeric data
      
      use_i <- attributes[[i]]$class == "numeric"
      
      if (sum(use_i) > 0){
        attributes[[i]]$unit[use_i] <- "!Add units here!"
      }
      
      # Add date time format strings for Date data
      
      use_i <- attributes[[i]]$class == "Date"
      
      if (sum(use_i) > 0){
        attributes[[i]]$dateTimeFormatString[use_i] <- "!Add datetime specifier here!"
      }
      
      # Write template to file or add template to x$template$attributes_*.txt$content
      
      # If writing to file ...
      
      if (isTRUE(write.file)){
        
        value <- file.exists(
          paste0(
            path,
            "/",
            "attributes_",
            substr(data.table[i], 1, nchar(data.table[i]) - 4),
            ".txt"
          )
        )
        
        if (!isTRUE(value)){
          
          message(
            paste0(
              "Importing attributes_",
              substr(data.table[i], 1, nchar(data.table[i]) - 4),
              ".txt."
            )
          )
          
          utils::write.table(
            attributes[[i]],
            paste0(
              path,
              "/",
              "attributes_",
              substr(data.table[i], 1, nchar(data.table[i]) - 4),
              ".txt"
            ),
            sep = "\t",
            row.names = F,
            quote = F,
            fileEncoding = "UTF-8"
          )
          
        } else {
          
          message(
            paste0(
              "attributes_",
              substr(data.table[i], 1, nchar(data.table[i]) - 4),
              ".txt already exists!"
            )
          )
          
        }

      # If adding template to x ...
        
      } else if (!exists('data_read_2_x')){

        value <- stringr::str_detect(
          paste0(
            "attributes_",
            substr(data.table[i], 1, nchar(data.table[i]) - 4),
            ".txt"
          ),
          names(x$template)
        )

        if (!any(value)){
          
          message(
            paste0(
              "Importing attributes_",
              substr(data.table[i], 1, nchar(data.table[i]) - 4),
              ".txt."
            )
          )
          
          missing_template <- list(
            content = attributes[[i]]
          )

          missing_template <- list(
            missing_template
          )

          names(missing_template) <- paste0(
            'attributes_', 
            substr(data.table[i], 1, nchar(data.table[i]) - 4), 
            '.txt'
          )
          
          x$template <- c(
            x$template, 
            missing_template
          )
          
        } else {
          
          message(
            paste0(
              "attributes_",
              substr(data.table[i], 1, nchar(data.table[i]) - 4),
              ".txt already exists!"
            )
          )
          
        }

      }

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


