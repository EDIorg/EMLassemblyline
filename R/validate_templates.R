#' Validate metadata template content
#'
#' @description
#'     Validate the content of `EMLassembline` metadata templates.
#'
#' @usage
#'     validate_templates(
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

validate_templates <- function(fun.name, fun.args){
  
  # If called from make_eml() -------------------------------------------------
  
  if (fun.name == 'make_eml'){
    
  }
  
}