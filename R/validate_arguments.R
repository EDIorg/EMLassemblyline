#' Validate arguments of EMLassemblyline functions
#'
#' @description
#'     Validate input arguments to `EMLassemblyline` functions.
#'
#' @usage
#'     validate_arguments(
#'       fun.name, 
#'       fun.args
#'     )
#'
#' @param fun.name
#'     (character) Function name passed to `validate_x` with 
#'     `as.character(match.call()[[1]])`.
#' @param fun.args
#'     (named list) Function arguments and values passed to `validate_x` with 
#'     `as.list(environment())`.
#'     
#' @details
#'     Validation checks are function specific.    
#'

validate_arguments <- function(fun.name, fun.args){
  
  # Parameterize --------------------------------------------------------------
  
  use_i <- sapply(fun.args, function(X) identical(X, quote(expr=)))
  fun.args[use_i] <- list(NULL)
  
  # Call from define_catvars() ------------------------------------------------
  
  if (fun.name == 'define_catvars'){
    
    # If not using x ...
    
    if (is.null(fun.args$x)){
      
      # Get attribute file names and data file names
      
      files <- list.files(fun.args$path)
      use_i <- stringr::str_detect(string = files,
                                   pattern = "^attributes")
      if (sum(use_i) == 0){
        stop('There are no attributes.txt files in your dataset working directory. Please fix this.')
      }
      
      attribute_files <- files[use_i]
      table_names_base <- stringr::str_sub(string = attribute_files,
                                           start = 12,
                                           end = nchar(attribute_files)-4)
      data_files <- list.files(fun.args$data.path)
      use_i <- stringr::str_detect(string = data_files,
                                   pattern = stringr::str_c("^", table_names_base, collapse = "|"))
      table_names <- data_files[use_i]
      data_files <- table_names
      
      # Send warning if data table name is repeated more than once
      
      if (length(unique(tools::file_path_sans_ext(data_files))) != length(data_files)){
        stop('Duplicate data file names exist in this directory. Please remove duplicates, even if they are a different file type.')
      }
      
      # Validate fields of data.files
      
      EDIutils::validate_fields(path = fun.args$data.path, data.files = data_files)
    
    # If using x ...
      
    } else if (!is.null(fun.args$x)){
      
      # Get attribute file names and data file names
      
      files <- names(fun.args$x$template)
      use_i <- stringr::str_detect(string = files,
                                   pattern = "^attributes")
      if (sum(use_i) == 0){
        stop('There are no attributes.txt files in your dataset working directory. Please fix this.')
      }
      
      attribute_files <- files[use_i]
      
      table_names_base <- stringr::str_sub(string = attribute_files,
                                           start = 12,
                                           end = nchar(attribute_files)-4)
      
      data_files <- names(fun.args$x$data.table)
      
      use_i <- stringr::str_detect(string = data_files,
                                   pattern = stringr::str_c("^", table_names_base, collapse = "|"))
      
      table_names <- data_files[use_i]
      data_files <- table_names
      
      # Send warning if data table name is repeated more than once
      
      if (length(unique(tools::file_path_sans_ext(data_files))) != length(data_files)){
        stop('Duplicate data file names exist in this directory. Please remove duplicates, even if they are a different file type.')
      }
      
    }

  }
  
  # Call from extract_geocoverage() -------------------------------------------
  
  if (fun.name == 'extract_geocoverage'){
    
    # Handle deprecated arguments
    
    if (!is.null(fun.args$data.file)){
      fun.args$data.table <- fun.args$data.file
    }

    # data.table
    
    if (is.null(fun.args$data.table)){
      stop('Input argument "data.table" is missing! Specify the data file containing the geographic coordinates.')
    }
    
    # lat.col
    
    if (is.null(fun.args$lat.col)){
      stop('Input argument "lat.col" is missing! Specify latitude column name.')
    }
    
    # lon.col
    
    if (is.null(fun.args$lon.col)){
      stop('Input argument "lon.col" is missing! Specify longitude column name.')
    }
    
    # site.col
    
    if (is.null(fun.args$site.col)){
      stop('Input argument "site.col" is missing! Specify site column name.')
    }

  }
  
  # Call from import_templates() ----------------------------------------------
  
  if (fun.name == 'import_templates'){
    
    # Handle deprecated arguments
    
    if (!is.null(fun.args$data.files)){
      fun.args$data.table <- fun.args$data.files
    }

    # license
    
    if (is.null(fun.args$license)){
      stop('Input argument "license" is missing')
    }
    
    license.low <- tolower(fun.args$license)
    
    if (!stringr::str_detect(license.low, "^cc0$|^ccby$")){
      stop('Invalid value entered for the "license" argument. Please choose "CC0" or "CCBY".')
    }
    
    # data.table
    
    if (!is.null(fun.args$data.table)){
      
      # Validate table names
      
      data_files <- EDIutils::validate_file_names(
        path = fun.args$data.path, 
        data.files = fun.args$data.table
      )
      
      # Validate table fields
      
      EDIutils::validate_fields(
        path = fun.args$data.path, 
        data.files = data_files
      )
      
    }
    
  }
  
  # Call from make_eml() ------------------------------------------------------
  
  if (fun.name == 'make_eml'){
    
    # Handle deprecated arguments
    
    if (!is.null(fun.args$affiliation)){
      fun.args$user.domain <- fun.args$affiliation
    }
    
    if (!is.null(fun.args$data.files)){
      fun.args$data.table <- fun.args$data.files
    }
    
    if (!is.null(fun.args$data.files.description)){
      fun.args$data.table.description <- fun.args$data.files.description
    }
    
    if (!is.null(fun.args$data.files.quote.character)){
      fun.args$data.table.quote.character <- fun.args$data.files.quote.character
    }
    
    if (!is.null(fun.args$data.files.url)){
      fun.args$data.url <- fun.args$data.files.url
    }
    
    if (!is.null(fun.args$zip.dir)){
      fun.args$other.entity <- fun.args$zip.dir
    }
    
    if (!is.null(fun.args$zip.dir.description)){
      fun.args$other.entity.description <- fun.args$zip.dir.description
    }
    
    # dataset.title
    
    if (is.null(fun.args$dataset.title)){
      stop('Input argument "dataset.title" is missing.')
    }

    # data.table.description
    
    if (!is.null(fun.args$data.table)){
      if (is.null(fun.args$data.table.description)){
        stop('Input argument "data.table.description" is missing.')
      }
    }

    # temporal.coverage
    
    if (is.null(fun.args$temporal.coverage)){
      stop('Input argument "temporal.coverage" is missing.')
    }
    
    if (length(fun.args$temporal.coverage) != 2){
      stop('The argument "temporal.coverage" requires both a begin date and end date. Please fix this.')
    }
    
    # geographic.coordinates and geographic.description
    
    if (is.null(fun.args$geographic.coordinates) & (!is.data.frame(fun.args$x$template$bounding_boxes.txt$content))){
      stop('Input argument "geographic.coordinates" is missing and the "bounding_boxes.txt" template is missing. Add geographic bounding coordinates for your dataset.')
    }
    
    if (is.null(fun.args$geographic.coordinates)){
      stop('Input argument "geographic.coordinates" is missing.')
    }
    
    if (is.null(fun.args$geographic.description)){
      stop('Input argument "geographic.description" is missing.')
    }

    # maintenance.description
    
    if (is.null(fun.args$maintenance.description)){
      stop('Input argument "maintenance.description" is missing. Indicate whether data collection is "ongoing" or "completed" for your dataset.')
    }

    # user.id and user.domain
    
    if (!is.null(fun.args$user.id) & is.null(fun.args$user.domain)){
      stop('Input argument "user.domain" is missing. Add one.')
    }
    
    if (!is.null(fun.args$user.domain) & is.null(fun.args$user.id)){
      stop('Input argument "user.id" is missing. Add one.')
    }
    
    if ((!is.null(fun.args$user.id)) & (!is.null(fun.args$user.domain))){
      if (length(fun.args$user.id) != length(fun.args$user.domain)){
        stop('The number of values listed in arguments "user.id" and "user.domain" do not match. Each user.id must have a corresponding user.domain')
      }
      if (sum(sum(fun.args$user.domain == 'LTER'), sum(fun.args$user.domain == 'EDI')) != length(fun.args$user.domain)){
        warning('Input argument "user.domain" is not "EDI" or "LTER". If not creating a data package for EDI, then ignore this message. A default value "someuserdomain" will be used.')
      }
    }
    
    # path, data.path, and eml.path
    
    if (is.null(fun.args$x)){
      EDIutils::validate_path(fun.args$path)
    }

    EDIutils::validate_path(fun.args$data.path)  
    
    if (isTRUE(fun.args$write.file)){
      EDIutils::validate_path(fun.args$eml.path)
    }
    
    # data.table
    
    if (!is.null(fun.args$data.table)){
      
      table_names <- EDIutils::validate_file_names(
        path = fun.args$data.path, 
        data.files = fun.args$data.table
      )
      
      EDIutils::validate_fields(
        path = fun.args$data.path, 
        data.files = table_names
      )
      
    }
    
    # data.table.description
    
    if (!is.null(fun.args$data.table)){
      if (length(fun.args$data.table.description) != length(fun.args$data.table)){
        stop('The number of descriptions listed in the argument "data.table.description" does not match the number of files listed in the argument "data.table". These must match.')
      }
    }
    
    # data.table.quote.character
    
    if (!is.null(fun.args$data.table)){
      if (!is.null(fun.args$data.table.quote.character)){
        if (length(fun.args$data.table.quote.character) != length(fun.args$data.table)){
          stop('The number of quote characters listed in the argument "data.table.quote.character" does not match the number of files listed in the argument "data.table". These must match.')
        }
      }
    }
    
    # other.entity and other.entity.description
    
    if ((!is.null(fun.args$other.entity)) & (is.null(fun.args$other.entity.description))){
      stop('The argument "other.entity.description" is missing and "other.entity" is present. Add a description for your zip directory.')
    }
    
    if ((!is.null(fun.args$other.entity.description)) & (is.null(fun.args$other.entity))){
      stop('The argument "other.entity" is missing and "other.entity.description" is present. Add the zip directories you are describing.')
    }
    
    if ((!is.null(fun.args$other.entity.description)) & (!is.null(fun.args$other.entity))){
      if ((length(fun.args$other.entity)) != (length(fun.args$other.entity.description))){
        stop('The number of other.entity and other.entity.descriptions must match.')
      }
    }
    
    # package.id
    
    if (!is.null(fun.args$package.id)){
      if (!isTRUE(stringr::str_detect(fun.args$package.id, '[:alpha:]\\.[:digit:]+\\.[:digit:]'))){
        warning('Input argument "package.id" is not valid for EDI. An EDI package ID must consist of a scope, identifier, and revision (e.g. "edi.100.4"). If not creating a data package for EDI, then ignore this message.')
      }
    }

  }
  
  # Call from make_arguments() ------------------------------------------------
  
  if (fun.name == 'make_arguments'){
    
    # path
    
    if (!is.null(fun.args$path)){
      EDIutils::validate_path(fun.args$path)
    }
    
    # data.path
    
    if (!is.null(fun.args$data.path)){
      EDIutils::validate_path(fun.args$data.path)
    }
    
    # data.table
    
    if (!is.null(fun.args$data.table)){
      output <- EDIutils::validate_file_names(
        path = fun.args$data.path,
        data.files = fun.args$data.table
      )
    }
    
    # other.entity
    
    if (!is.null(fun.args$other.entity)){
      output <- EDIutils::validate_file_names(
        path = fun.args$data.path,
        data.files = fun.args$other.entity
      )
    }
    
  }
  
  # Call from template_core_metadata() ----------------------------------------
  
  if (fun.name == 'template_core_metadata'){
    
    # data.files
    
    if (fun.args$data.files != 'deprecated'){
      stop('Input argument "data.files" has been deprecated. Use "data.table" instead.')
    }
    
    # license
    
    if (is.null(fun.args$license)){
      stop('Input argument "license" is missing')
    }
    
    license.low <- tolower(fun.args$license)
    
    if (!stringr::str_detect(license.low, "^cc0$|^ccby$")){
      stop('Invalid value entered for the "license" argument. Please choose "CC0" or "CCBY".')
    }
    
    # data.table
    
    if (!is.null(fun.args$data.table)){
      
      # Validate table names
      
      data_files <- EDIutils::validate_file_names(
        path = fun.args$data.path, 
        data.files = fun.args$data.table
      )
      
      # Validate table fields
      
      EDIutils::validate_fields(
        path = fun.args$data.path, 
        data.files = data_files
      )
      
    }
    
  }
  
  # Call from template_table_attributes() -------------------------------------
  
  if (fun.name == 'template_table_attributes'){
    
    # data.files
    
    if (fun.args$data.files != 'deprecated'){
      stop('Input argument "data.files" has been deprecated. Use "data.table" instead.')
    }
    
    # license
    
    if (is.null(fun.args$license)){
      stop('Input argument "license" is missing')
    }
    
    license.low <- tolower(fun.args$license)
    
    if (!stringr::str_detect(license.low, "^cc0$|^ccby$")){
      stop('Invalid value entered for the "license" argument. Please choose "CC0" or "CCBY".')
    }
    
    # data.table
    
    if (!is.null(fun.args$data.table)){
      
      # Validate table names
      
      data_files <- EDIutils::validate_file_names(
        path = fun.args$data.path, 
        data.files = fun.args$data.table
      )
      
      # Validate table fields
      
      EDIutils::validate_fields(
        path = fun.args$data.path, 
        data.files = data_files
      )
      
    }
    
  }
  
  
}