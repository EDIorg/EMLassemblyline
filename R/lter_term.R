#' LTER term
#'
#' @description  
#'     Search for a term in the LTER Controlled Vocabulary (Long Term 
#'     Ecological Research Network).
#'
#' @usage 
#'     lter_term(x, messages = FALSE)
#'
#' @param x 
#'     (character) A term to search for.
#' @param messages 
#'     (logical) Display diagnostic messages, e.g. alternative spelling options.
#'
#' @return 
#'     TRUE - If the searched term is found.
#'     FALSE- If the searched term was not found.
#'     messages - Alternative spelling and near misses for searched term.
#'
#' @export
#'


lter_term <- function(x, messages = FALSE){
  
  
  # Check arguments -----------------------------------------------------------
  
  if (missing(x)){
    stop('Input argument "x" is missing! Specify a term to search for.')
  }
  if (is.character(x) != T){
    stop('Input argument "x" is not of class "character"!')
  }
  if (length(x) != 1){
    stop('Input argument "x" has a length > 1! Only single terms are allowed.')
  }
  
  # Construct the query and call the API --------------------------------------
  
  url <- 'http://vocab.lternet.edu'
  path <- '/vocab/vocab/services.php/?task=search&arg='
  term <- str_replace_all(
    string = x, 
    pattern = ' ', 
    replacement = '+'
    )
  
  query <- paste0(
    url,
    path,
    term
    )
  
  reply <- xml2::read_xml(query)
  
  # Was the term found? -------------------------------------------------------
  
  if (length(xml2::xml_find_all(reply, './/result')) == 0){
    term_found <- FALSE
  } else {
    term_found <- TRUE
  }
  
  # Were there any near misses? -----------------------------------------------
  
  if (!isTRUE(term_found)){
    
    # Construct a query to search for similar terms
    
    url <- 'http://vocab.lternet.edu'
    path <- '/vocab/vocab/services.php/?task=fetchSimilar&arg='
    query <- paste0(
      url,
      path,
      term
    )
    
    reply <- xml2::read_xml(query)
    
    # Were similar terms found?
    
    if (length(xml2::xml_find_all(reply, './/result')) == 0){
      near_miss <- FALSE
    } else {
      nodeset <- xml2::xml_find_all(reply, './/result/string')
      near_miss_term <- xml2::xml_text(nodeset)
      near_miss <- TRUE
    }
    
  }
  
  # Report near misses --------------------------------------------------------
  
  if (!missing(messages) & isTRUE(messages)){
    
    msg <- message(
      paste0(
        'The term "',
        x,
        '" could not be found. Possible alternatives:',
        '\n',
        paste0(
          near_miss_term, 
          collapse = '\n'
          )
        )
      )
    
  }
  
  # Output results ------------------------------------------------------------
  
  term_found

}
  