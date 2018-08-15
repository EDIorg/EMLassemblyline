#' lter_scope
#'
#' @description  
#'     Get the scope description for a term in the LTER Controlled Vocabulary.
#'
#' @usage 
#'     lter_scope(id)
#'
#' @param id 
#'     (numeric) An identification number of a valid term in the LTER 
#'     Controlled Vocabulary.
#'
#' @return 
#'     (character) The scope description for a LTER Controlled Vocabulary 
#'     term. Note, not all terms have descriptions.
#'
#' @export
#'


lter_scope <- function(id){
  
  # Check arguments -----------------------------------------------------------
  
  if (missing(id)){
    stop('Input argument "id" is missing! Specify an identification number to get the scope description for.')
  }
  if (is.numeric(id) != T){
    stop('Input argument "id" is not of class "numeric"!')
  }
  if (length(id) != 1){
    stop('Input argument "id" has a length > 1! Only single identification numbers are allowed.')
  }
  
  # Get the scope description -------------------------------------------------
  
  # Query for input ID
  
  search_output <- xml2::read_xml(
    paste0(
      'http://vocab.lternet.edu/vocab/vocab/services.php/?task=fetchNotes&arg=',
      as.character(id)
    )
  )
  
  # Parse results
  
  if (length(xml2::xml_find_all(search_output, './/result')) != 0){
    
    nodeset <- xml2::xml_find_all(search_output, './/result/term/note_text')
    node_terms <- xml2::xml_text(nodeset)
    # Clean up formatting tags
    node_terms <- stringr::str_replace_all(string = node_terms, pattern = '<p>|</p>', replacement = '')
    node_terms <- stringr::str_replace_all(string = node_terms, pattern = '\n', replacement = ' ')
    node_terms <- stringr::str_replace_all(string = node_terms, pattern = '<.*?>', replacement = '')
    
  } else {
    
    node_terms <- 'No scope description available.'
    
  }
  
  # Return result -------------------------------------------------------------
  
  node_terms
  
}
