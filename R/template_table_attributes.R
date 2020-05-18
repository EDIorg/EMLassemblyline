#' Create table attributes template
#'
#' @description  
#'     Use this function to extract column names and classes of a data table 
#'     and return for user supplied column definitions and missing value codes.
#'     \href{https://ediorg.github.io/EMLassemblyline/articles/edit_metadata_templates.html}{Instructions for editing this template.}
#'
#' @usage 
#'     template_table_attributes(
#'       path,
#'       data.path = path,
#'       data.table = NULL,
#'       write.file = TRUE,
#'       x = NULL
#'     )
#'
#' @param path 
#'     (character) Path to the metadata template directory.
#' @param data.path
#'     (character) Path to the data directory.
#' @param data.table
#'     (character) Table name. If more than one, then supply as a 
#'     vector of character strings (e.g. 
#'     \code{data.table = c('nitrogen.csv', 'decomp.csv')}).
#' @param write.file
#'     (logical; optional) Whether to write the template file.
#' @param x
#'     (named list; optional) Alternative input to 
#'     \code{template_table_attributes()}. Use \code{template_arguments()} 
#'     to create \code{x}.
#'
#' @return 
#'     \itemize{
#'         \item{\strong{attributes_*.txt} The tab delimited attributes 
#'         template where * is the table name from which the attributes were 
#'         extracted. This file is written to \code{path} unless using \code{x}, 
#'         in which case the template is added to 
#'         \strong{/x/templates/attributes_*.txt}.}
#'         \item{\strong{custom_units.txt} The tab delimited custom units 
#'         template for defining non-standard units. This file is written to 
#'         \code{path} unless using \code{x}, in which case the template is 
#'         added to \strong{/x/templates/custom_units.txt}.}
#'     }
#'     
#' @details 
#'     An existing attributes template will not be overwritten by subsequent 
#'     calls to \code{template_table_attributes()}.
#'     
#' @examples 
#' # Initialize data package directory for template_table_attributes()
#' file.copy(
#'  from = system.file('/examples/pkg_250', package = 'EMLassemblyline'),
#'  to = tempdir(),
#'  recursive = TRUE
#' )
#' 
#' # Set working directory
#' setwd(paste0(tempdir(), '/pkg_250'))
#' 
#' # View directory contents (NOTE: attributes_*.txt don't exist)
#' dir('./metadata_templates')
#' 
#' # Template table attributes
#' template_table_attributes(
#'   path = './metadata_templates',
#'   data.path = './data_objects',
#'   data.table = c('decomp.csv', 'nitrogen.csv')
#' )
#' 
#' # View directory contents (NOTE: attributes_*.txt and custom_units.txt exist)
#' dir('./metadata_templates')
#' 
#' # Rerunning template_table_attributes() does not overwrite files
#' template_table_attributes(
#'   path = './metadata_templates',
#'   data.path = './data_objects',
#'   data.table = c('decomp.csv', 'nitrogen.csv')
#' )
#' 
#' # Clean up
#' unlink('.', recursive = TRUE)
#'     
#' @export     
#'     

template_table_attributes <- function(
  path, 
  data.path = path, 
  data.table = NULL, 
  write.file = TRUE,
  x = NULL){
  
  message('Templating table attributes ...')
  
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
    fun.name = 'template_table_attributes',
    fun.args = as.list(environment())
  )
  
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
      
      # Validate data.table
      
      data.table <- suppressWarnings(
        EDIutils::validate_file_names(
          path = data.path, 
          data.files = data.table
        )
      )
      
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
        integer64 = "numeric",
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
      
      use_i <- guess == "numeric"
      
      if (sum(use_i) > 0){
        potential_date_cols <- colnames(x$data.table[[i]]$content)[use_i]
        potential_date_i <- stringr::str_detect(tolower(potential_date_cols), "date|time|day|year")
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
              "attributes_",
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
          names(x$template),
          paste0(
            "attributes_",
            substr(data.table[i], 1, nchar(data.table[i]) - 4),
            ".txt"
          )
        )
        
        if (!any(value)){
          
          message(
            paste0(
              "attributes_",
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
      message("custom_units.txt")
    } else {
      message("custom_units.txt already exists!")
    }
    
    # If adding to x ...
    
  } else if (!exists('data_read_2_x')){
    
    if (is.null(x$template$custom_units.txt$content)) {
      x$template$custom_units.txt$content <- utils::read.table(
        file = system.file(
          '/templates/custom_units.txt',
          package = 'EMLassemblyline'
        ), 
        header = T,
        sep = '\t',
        as.is = T)
      message("custom_units.txt")
      
    } else {
      
      message("custom_units.txt already exists!")
      
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


