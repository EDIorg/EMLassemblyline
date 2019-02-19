#' Open the standard unit dictionary for EML
#'
#' @description  
#'     View the standard unit dictionary of the Ecological Metadata Language
#'     schema in the RStudio source window.
#'
#' @usage 
#'     view_unit_dictionary()
#'     
#' @details
#'     Use the search field to find the unit of interest.
#'     
#' @export     
#'     

view_unit_dictionary <- function(){
  
  standardUnits <- EML::get_unitList()
  utils::View(standardUnits$units)

}
