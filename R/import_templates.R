#' Import templates for core metadata
#'
#' @description  
#'     Import template files for storage of core metadata. Some templates are 
#'     populated with content detected by automated metadata extraction 
#'     methods. The remainder will have to be manually entered by a human. 
#'     Instructions for filling out the templates are at
#'     \url{https://clnsmth.github.io/EMLassemblyline/articles/instructions.html}.
#'
#' @usage 
#'     import_templates(path, data.path = path, data.table = NULL, 
#'     x = NULL, data.files = NULL)
#'
#' @param path 
#'     (character) Path to where the templates will be imported.
#' @param data.path
#'     (character) Path to where the data files are stored.
#' @param license
#'     (character) License under which the data will be released. Use "CC0" 
#'     (\url{https://creativecommons.org/publicdomain/zero/1.0/}), 
#'     or "CCBY" (\url{https://creativecommons.org/licenses/by/4.0/}).
#' @param data.table
#'     (character) Names of tabular data. Names of non-tabular data are
#'     entered as arguments to `make_eml`.
#' @param x
#'     (named list) Alternative input to all `EMLassemblyline` functions (i.e. 
#'     rather than supplying the files themselves). Use 
#'     `EMLassemblyline::read_files` to create this list.
#' @param data.files
#'     NOTE: `data.files` has been deprecated. Use `data.table` instead.
#'
#' @return 
#'     \itemize{
#'         \item{`abstract.txt`} Template for the dataset abstract.
#'         \item{`additional_info.txt`} Template for miscellaneous dataset
#'         information.
#'         \item{`attributes_*.txt`} Template(s) for data table attribute 
#'         metadata, populated with some automatically extracted metadata 
#'         content from the data files.
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
#'     }
#'     
#' @details 
#'     Existing templates will not be overwritten by `import_templates`.
#'
#' @examples
#'     # Create a directory structure for an example dataset
#'     
#'     # Copy example data tables to /dataset/data
#'     
#'     # Import templates to /dataset/metadata_templates
#'     
#' @export     
#'     

import_templates <- function(path, data.path = path, license, 
                             data.table = NULL, x = NULL, data.files = NULL){
  
  message('Importing metadata templates')
  
  # Import content from x -----------------------------------------------------
  
  # If x exists ...
  
  if (!is.null(x)){
    
    # Set path to arbitrary value
    
    path <- 'x'
    
    # Get data.path and data.table
    
    if (!is.null(x$data.table)){
      
      data.path <- x$data.table[[1]]$path
      
      data.table <- names(x$data.table)
      
    }
    
  }

  # Validate arguments and parameterize ---------------------------------------
    
  if (is.null(path)){
    stop('Input argument "path" is missing! Specify the path to your dataset working directory.')
  }

  if (!is.null(data.files)){
    stop('Input argument "data.files" has been deprecated. Use "data.table" instead.')
  }
  
  license.low <- tolower(license)
  if (!stringr::str_detect(license.low, "^cc0$|^ccby$")){
    stop('Invalid value entered for the "license" argument. Please choose "CC0" or "CCBY".')
  }

  # If data tables are present ...
  
  if (!is.null(data.table)){
    
    # Validate table names
    
    data_files <- EDIutils::validate_file_names(
      data.path, 
      data.table
    )
    
    # Validate table fields
    
    EDIutils::validate_fields(
      data.path, 
      data.files = data_files
    )
    
    # Guess field delimiter
    
    if (is.null(x)){
      
      delim_guess <- EDIutils::detect_delimeter(
        data.path, 
        data.files = data_files, 
        EDIutils::detect_os()
      )
      
    }

  }

  # Import abstract.txt -------------------------------------------------------
  
  # If not using x ... 
  
  if (is.null(x)){
    
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
    
    if (isTRUE(value)){
      message("Importing abstract.txt.")
    } else {
      message("abstract.txt already exists!")
    }
    
    # If using x ...
    
  } else if (!is.null(x)){
    
    # If content is NA ...
    
    if (any(is.na(x$template$abstract.txt$content))){
      
      # Add to x
      
      message("Importing abstract.txt.")
      
      x$template$abstract.txt$content <- EML::set_TextType(
        file = system.file(
          '/templates/abstract.txt',
          package = 'EMLassemblyline'
        )
      )
      
      # Otherwise ...
      
    } else {
      
      message("abstract.txt already exists!")
      
    }
    
  }
  
  # Import additional_info.txt ------------------------------------------------
  
  # If not using x ... 
  
  if (is.null(x)){
    
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
    
    if (isTRUE(value)){
      message("Importing additional_info.txt.")
    } else {
      message("additional_info.txt already exists!")
    }
    
    # If using x ...
    
  } else if (!is.null(x)){
    
    # If content is NA ...
    
    if (any(is.na(x$template$additional_info.txt$content))){
      
      # Add to x
      
      message("Importing additional_info.txt")
      
      x$template$additional_info.txt$content <- EML::set_TextType(
        file = system.file(
          '/templates/additional_info.txt',
          package = 'EMLassemblyline'
        )
      )
      
      # Otherwise ...
      
    } else {
      
      message("additional_info.txt already exists!")
      
    }
    
  }
  
  # Import bounding_boxes.txt -------------------------------------------------
  
  # If not using x ... 
  
  if (is.null(x)){
    
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
    
    if (isTRUE(value)){
      message("Importing bounding_boxes.txt.")
    } else {
      message("bounding_boxes.txt already exists!")
    }
    
    # If using x ...
    
  } else if (!is.null(x)){

    # If content is NA ...
    
    if (any(is.na(x$template$bounding_boxes.txt$content))){
      
      # Add to x
      
      message("Importing bounding_boxes.txt.")
      
      x$template$bounding_boxes.txt$content <- utils::read.table(
        file = system.file(
          '/templates/bounding_boxes.txt',
          package = 'EMLassemblyline'
        ), 
        header = T,
        sep = '\t',
        as.is = T
      )
      
      # Otherwise ...
      
    } else {
      
      message("bounding_boxes.txt already exists!")
      
    }
    
  }
  
  # Import custom_units.txt ---------------------------------------------------
  
  # If not using x ... 
  
  if (is.null(x)){
    
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
    
    if (isTRUE(value)){
      message("Importing custom_units.txt.")
    } else {
      message("custom_units.txt already exists!")
    }
    
    # If using x ...
    
  } else if (!is.null(x)){
    
    # If content is NA ...
    
    if (any(is.na(x$template$custom_units.txt$content))){
      
      # Add to x
      
      message("Importing custom_units.txt.")
      
      x$template$custom_units.txt$content <- utils::read.table(
        file = system.file(
          '/templates/custom_units.txt',
          package = 'EMLassemblyline'
        ), 
        header = T,
        sep = '\t',
        as.is = T
      )
     
      # Otherwise ...
       
    } else {
      
      message("custom_units.txt already exists!")
      
    }
    
  }

  # Import intellectual_rights.txt --------------------------------------------
  
  # If license is CC0 ...
  
  if (license.low == "cc0"){
    
    # If not using x ... 
    
    if (is.null(x)){
      
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
      
      if (isTRUE(value)){
        message("Importing intellectual_rights.txt.")
      } else {
        message("intellectual_rights.txt already exists!")
      }
      
      # If using x ...
      
    } else if (!is.null(x)){
      
      # If content is not logical ...
      
      # If content is NA ...
      
      if (any(is.na(x$template$intellectual_rights.txt$content))){
        
        # Add to x
        
        message("Importing intellectual_rights.txt.")
        
        x$template$intellectual_rights.txt$content <- EML::set_TextType(
          file = system.file(
            '/templates/intellectual_rights_cc0.txt',
            package = 'EMLassemblyline'
          )
        )
        
        # Otherwise ...
        
      } else {
        
        message("intellectual_rights.txt. already exists!")
        
      }
      
    }
    
    # If license is CCBY

  } else if (license.low == "ccby"){
    
    # If not using x ... 
    
    if (is.null(x)){
      
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
      
      if (isTRUE(value)){
        message("Importing intellectual_rights.txt.")
      } else {
        message("intellectual_rights.txt already exists!")
      }
      
      # If using x ...
      
    } else if (!is.null(x)){
      
      # If content is NA ...
      
      if (any(is.na(x$template$intellectual_rights.txt$content))){
        
        # Add to x
        
        message("Importing intellectual_rights.txt.")
        
        x$template$intellectual_rights.txt$content <- EML::set_TextType(
          file = system.file(
            '/templates/intellectual_rights_ccby4.0.txt',
            package = 'EMLassemblyline'
          )
        )
        
       # Otherwise ...
         
      } else {
        
        message("intellectual_rights.txt already exists!")
        
      }
      
    }
    
  }
  
  # Import keywords.txt -------------------------------------------------------

  # If not using x ... 
  
  if (is.null(x)){
    
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
    
    if (isTRUE(value)){
      message("Importing keywords.txt.")
    } else {
      message("keywords.txt already exists!")
    }
    
    # If using x ...
    
  } else if (!is.null(x)){
    
    # If content is NA ...
    
    if (any(is.na(x$template$keywords.txt$content))){
      
      # Add to x
      
      message("Importing keywords.txt.")
      
      x$template$keywords.txt$content <- utils::read.table(
        file = system.file(
          '/templates/keywords.txt',
          package = 'EMLassemblyline'
        ), 
        header = T,
        sep = '\t',
        as.is = T
      )
     
      # Otherwise ...
       
    } else {
      
      message("keywords.txt already exists!")
      
    }
    
  }
  
  # Import methods.txt --------------------------------------------------------
  
  # If not using x ... 
  
  if (is.null(x)){
    
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
    
    if (isTRUE(value)){
      message("Importing methods.txt.")
    } else {
      message("methods.txt already exists!")
    }
    
    # If using x ...
    
  } else if (!is.null(x)){
    
    # If content is NA ...
    
    if (any(is.na(x$template$methods.txt$content))){
      
      # Add to x
      
      message("Importing methods.txt.")
      
      x$template$methods.txt$content <- EML::set_methods(
        methods_file = system.file(
          '/templates/methods.txt',
          package = 'EMLassemblyline'
        )
      )
      
      # Otherwise ...
      
    } else {
      
      message("methods.txt already exists!")
      
    }
    
  }

  # Import personnel.txt ------------------------------------------------------
  
  # If not using x ... 
  
  if (is.null(x)){
    
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
    
    if (isTRUE(value)){
      message("Importing personnel.txt.")
    } else {
      message("personnel.txt already exists!")
    }
    
    # If using x ...
    
  } else if (!is.null(x)){
    
    # If content is NA ...
    
    if (any(is.na(x$template$personnel.txt$content))){
      
      # Add to x
      
      message("Importing personnel.txt.")
      
      x$template$personnel.txt$content <- utils::read.table(
        file = system.file(
          '/templates/personnel.txt',
          package = 'EMLassemblyline'
        ), 
        header = T,
        sep = '\t',
        as.is = T
      )
      
      # Otherwise ...
      
    } else {
      
      message("personnel.txt already exists!")
      
    }
    
  }

  # Import attributes templates -----------------------------------------------
  
  if (!is.null(data.table)){
    
    # Read data.tables into x structure if not already done
    
    if (is.null(x)){
      
      x <- read_files(
        path = path,
        data.path = data.path,
        data.table = data.table
      )
      
      data_read_2_x <- TRUE
      
    }

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
      
      # Write table to file or add to list x
      
      # If writing to file ...
      
      if (exists('data_read_2_x')){
        
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
        
        # Otherwise if adding to x ...
        
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
            content = attributes[[i]],
            path = NA_character_
          )

          missing_template <- list(
            missing_template
          )

          names(missing_template) <- paste0(
            'attributes_',
            data.table[i],
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
  
  if (!exists('data_read_2_x')){
    
    return(x)
    
  }
  
  message("Done.")

}


