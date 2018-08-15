#' lter_id
#'
#' @description  
#'     Get the identification number for a valid term in the LTER Controlled 
#'     Vocabulary.
#'
#' @usage 
#'     lter_id(x)
#'
#' @param x 
#'     (character) A valid term in the LTER Controlled Vocabulary.
#'
#' @return 
#'     (numeric) The identification number for a LTER Controlled Vocabulary 
#'     term.
#'
#' @export
#'


lter_id <- function(x){
  
  # Check arguments -----------------------------------------------------------
  
  if (missing(x)){
    stop('Input argument "x" is missing! Specify a term get the ID number for.')
  }
  if (is.character(x) != T){
    stop('Input argument "x" is not of class "character"!')
  }
  if (length(x) != 1){
    stop('Input argument "x" has a length > 1! Only single terms are allowed.')
  }
  
  # Get the term ID and report ------------------------------------------------
  
  if (isTRUE(lter_term(x = x))){
    
    # Construct the search term and query
    
    term <- str_replace_all(
      string = x, 
      pattern = ' ', 
      replacement = '+'
    )
    
    search_output <- xml2::read_xml(
      paste0(
        'http://vocab.lternet.edu/vocab/vocab/services.php/?task=search&arg=',
        term
      )
    )
    
    nodeset <- xml2::xml_find_all(search_output, './/result/term/term_id')
    node_term_id <- as.numeric(xml2::xml_text(nodeset))
    
    # Report the result
    
    node_term_id

  } else {
    stop(
      paste0('\n"',
             x,
             '" could not be found in the LTER Controlled Vocabulary'
      )
    )
  }
  
}
