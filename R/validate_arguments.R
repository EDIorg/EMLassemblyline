#' Validate arguments of `EMLassemblyline` functions
#'
#' @description
#'     Validate input arguments to `EMLassemblyline` functions.
#'
#' @usage validate_arguments(x)
#'
#' @param x
#'     (list) A named list of arguments and corresponding values created by a
#'     call to `as.list(environment())` within the function whos arguments are
#'     being validated.
#'

validate_arguments <- function(x){
  
  # affiliation
  if ('affiliation' %in% names(x)){
    if ((tolower(x[['affiliation']]) != 'lter') & (tolower(x[['affiliation']]) != 'edi')){
      stop('The input argument "affiliation" must be "LTER" or "EDI".')
    }
  }
  
  # environment
  if ('environment' %in% names(x)){
    if ((tolower(x[['environment']]) != 'development') &
        (tolower(x[['environment']]) != 'staging') &
        (tolower(x[['environment']]) != 'production')){
      stop(paste0('The input argument "environment" must be "development", ',
                  '"staging", or "production".'))
    }
  }
  
  # Validate os
  if ('os' %in% names(x)){
    if (isTRUE((tolower(x[['os']]) != "win") & 
               (tolower(x[['os']]) != "mac") &
               (tolower(x[['os']]) != "lin"))){
      stop('The value of input argument "os" is invalid.')
    }
  }
  
  # package.id
  if ('package.id' %in% names(x)){
    if (!isTRUE(stringr::str_detect(x[['package.id']],
                                    '[:alpha:]\\.[:digit:]+\\.[:digit:]+$'))){
      stop(paste0('Input argument "package.id" appears to be malformed. ',
                  'A package ID must consist of a scope, identifier, ',
                  'and revision (e.g. "edi.100.4").'))
    }
  }
  
  # path
  if ('path' %in% names(x)){
    if (!dir.exists(x[['path']])){
      stop('The directory specified by the argument "path" does not exist! Please supply a valid path.')
    }
  }
  
}