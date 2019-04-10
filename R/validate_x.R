#' Validate `x` content
#'
#' @description
#'     Validate contents of `x`, a named list object facilitating 
#'     inputs/outputs to `EMLassemblyline` functions.
#'
#' @usage
#'     validate_x(
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

validate_x <- function(fun.name, fun.args){
  
  # If called from make_eml() -------------------------------------------------
  
  if (fun.name == 'make_eml'){
    
    x <- fun.args$x

    browser()
    
  }
  
  
}