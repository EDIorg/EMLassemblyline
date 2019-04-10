#' Validate arguments of `EMLassemblyline` functions
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
  
  browser()

  # If called from make_eml() -------------------------------------------------
  
  if (fun.name == 'make_eml'){
    
    if (!'dataset.title' %in% names(fun.args)){
      stop('Input argument "dataset.title" is missing.')
    }
    
    if (fun.args$data.files != 'deprecated'){
      stop('Input argument "data.files" has been deprecated. Use "data.table" instead.')
    }
    
    if (!is.null(fun.args$data.table)){
      if (fun.args$data.files.description != 'deprecated'){
        stop('Input argument "data.files.description" has been deprecated. Use "data.table.description" instead.')
      }
      if (is.null(fun.args$data.table.description)){
        stop('Input argument "data.table.description" is missing.')
      }
    }
    
    if (!'temporal.coverage' %in% names(fun.args)){
      stop('Input argument "temporal.coverage" is missing.')
    }
    
    if ((!'geographic.coordinates' %in% names(fun.args)) & !file.exists(paste0(fun.args$path, '/', 'bounding_boxes.txt'))){
      stop('Input argument "geographic.coordinates" is missing and the "bounding_boxes.txt" template is missing. Add geographic bounding coordinates for your dataset.')
    }
    
    if ('geographic.coordinates' %in% names(fun.args)){
      if (!'geographic.description' %in% names(fun.args)){
        stop('Input argument "geographic.description is missing.')
      }
    }
    
    if ((!'geographic.coordinates' %in% names(fun.args)) & ('geographic.description' %in% names(fun.args))){
      stop('Remove input argument "geographic.description". Only use this argument when the "geographic.coordinates" argument is present.')
    }
    
    if (!'maintenance.description' %in% names(fun.args)){
      stop('Input argument "maintenance.description" is missing. Indicate whether data collection is "ongoing" or "completed" for your dataset.')
    }
    
    if (fun.args$affiliation != 'deprecated'){
      stop('Input argument "affiliaton" has been deprecated. Use "user.domain" instead.')
    }
    
    if ((!is.null(fun.args$user.id)) & (is.null(fun.args$user.domain))){
      stop('Input argument "user.domain" is missing. Add one.')
    }
    
    if ((!is.null(user.domain)) & (is.null(fun.args$user.id))){
      stop('Input argument "user.id" is missing. Add one.')
    }
    
    if (fun.args$data.files.url != 'deprecated'){
      stop('Input argument "data.files.url" has been deprecated. Use "data.url" instead.')
    }
    
    # Validate paths
    
    EDIutils::validate_path(fun.args$path)
    
    if ('data.path' %in% names(fun.args)){
      EDIutils::validate_path(fun.args$data.path)  
    }
    
    if ('eml.path' %in% names(fun.args)){
      EDIutils::validate_path(fun.args$eml.path)  
    }
    
    # Validate data file names
    
    if (!is.null(fun.args$data.table)){
      
      table_names <- EDIutils::validate_file_names(
        path = fun.args$data.path, 
        data.files = fun.args$data.table
      )
      
    }
    
    # Validate fields of data.table
    
    if (!is.null(fun.args$data.table)){
      
      EDIutils::validate_fields(
        path = fun.args$data.path, 
        data.files = table_names
      )
      
    }
    
    # Validate data.table.description
    
    if (!is.null(fun.args$data.table)){
      if (length(fun.args$data.table.description) != length(fun.args$data.table)){
        stop('The number of descriptions listed in the argument "data.table.description" does not match the number of files listed in the argument "data.table". These must match.')
      }
    }
    
    # Validate data.table.quote.character
    
    if (!is.null(fun.args$data.table)){
      if (fun.args$data.files.quote.character != 'deprecated'){
        stop('Input argument "data.files.quote.character" has been deprecated. Use "data.table.quote.character" instead.')
      }
      if (!is.null(fun.args$data.table.quote.character)){
        if (length(fun.args$data.table.quote.character) != length(fun.args$data.table)){
          stop('The number of quote characters listed in the argument "data.table.quote.character" does not match the number of files listed in the argument "data.table". These must match.')
        }
      }
    }
    
    # Validate temporal.coverage
    
    if (length(fun.args$temporal.coverage) != 2){
      stop('The argument "temporal.coverage" requires both a begin date and end date. Please fix this.')
    }
    if (length(fun.args$temporal.coverage) != 2){
      stop('The argument "temporal.coverage" requires both a begin date and end date. Please fix this.')
    }
    
    # Validate other.entity and other.entity.description
    
    if (fun.args$zip.dir != 'deprecated'){
      stop('The argument "zip.dir" has been deprecated. Use "other.entity" instead.')
    }
    if (fun.args$zip.dir.description != 'deprecated'){
      stop('The argument "zip.dir.description" has been deprecated. Use "other.entity.description" instead.')
    }
    
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
    
    # Validate user.id and user.domain
    
    if ((!is.null(fun.args$user.id)) & (!is.null(user.domain))){
      if (length(fun.args$user.id) != length(fun.args$user.domain)){
        stop('The number of values listed in arguments "user.id" and "user.domain" do not match. Each user.id must have a corresponding user.domain')
      }
      if (sum(sum(fun.args$user.domain == 'LTER'), sum(fun.args$user.domain == 'EDI')) != length(fun.args$user.domain)){
        warning('Input argument "user.domain" is not "EDI" or "LTER". If not creating a data package for EDI, then ignore this message. A default value "someuserdomain" will be used.')
      }
    }
    
    # Validate package.id
    
    if (!is.null(fun.args$package.id)){
      if (!isTRUE(stringr::str_detect(fun.args$package.id, '[:alpha:]\\.[:digit:]+\\.[:digit:]'))){
        warning('Input argument "package.id" is not valid for EDI. An EDI package ID must consist of a scope, identifier, and revision (e.g. "edi.100.4"). If not creating a data package for EDI, then ignore this message.')
      }
    }
    
    return(r)

  }
  
  # If called from ... --------------------------------------------------------
  
}