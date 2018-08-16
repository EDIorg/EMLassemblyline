#' lter_term
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
#' @param interactive 
#'     (logical) Query user to select from alternative terms and returns back
#'     selection.
#'
#' @return 
#'     Logical value (TRUE/FALSE) indicating whether the searched term could
#'     be found. 
#'     
#'     If messages = TRUE, then alternative spellings and near misses 
#'     are displayed. 
#'     
#'     If interactive mode = TRUE, then a user selected term is returned.
#'
#' @export
#'


lter_term <- function(x, messages = FALSE, interactive = FALSE){
  
  # The LTER controlled vocabulary produces different results for a standard
  # search and fuzzy (similar) search. Both searches are run and results 
  # combined, then direct matches sought and if not found then all results
  # are presented as near misses.
  
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
  if (!missing(messages) & isTRUE(messages) & !missing(interactive) & isTRUE(interactive)){
    stop('Both arguments "messages" & "interactive" can not be used at the same time. Please select one or the other.')
  }
  
  # Construct the query and search --------------------------------------------
  
  term <- str_replace_all(
    string = x, 
    pattern = ' ', 
    replacement = '+'
    )
  
  # Standard search
  
  search_output <- xml2::read_xml(
    paste0(
      'http://vocab.lternet.edu/vocab/vocab/services.php/?task=search&arg=',
      term
      )
    )
  
  # Fuzzy search
  
  fuzzy_output <- xml2::read_xml(
    paste0(
      'http://vocab.lternet.edu/vocab/vocab/services.php/?task=fetchSimilar&arg=',
      term
    )
  )
  
  # Parse the responses and combine -------------------------------------------
  
  term_list <- c()
  
  # Get standard terms
  
  if (length(xml2::xml_find_all(search_output, './/result')) != 0){
    nodeset <- xml2::xml_find_all(search_output, './/result/term/string')
    node_terms <- xml2::xml_text(nodeset)
    term_list <- c(term_list, node_terms)
  }
  
  # Get fuzzy terms
  
  if (length(xml2::xml_find_all(fuzzy_output, './/result')) != 0){
    nodeset <- xml2::xml_find_all(fuzzy_output, './/result/string')
    node_terms <- xml2::xml_text(nodeset)
    term_list <- c(term_list, node_terms)
  }
  
  # Remove duplicates
  
  term_list <- unique(term_list)
  
  # Is the search term listed? ------------------------------------------------
  
  if (sum(term_list == x) == 1){
    term_found <- T
  } else {
    term_found <- F
  }
  
  # Report near misses --------------------------------------------------------
  
  if (!missing(messages) & isTRUE(messages) & (!isTRUE(term_found)) & (length(term_list) != 0)){
    
    msg <- message(
      paste0(
        'The term "',
        x,
        '" could not be found in the LTER Controlled Vocabulary. Possible alternatives:',
        '\n',
        paste0(
          term_list, 
          collapse = '\n'
          ),
        '\n'
        )
      )
    
  }
  
  # Interactive mode ----------------------------------------------------------
  
  if (!missing(interactive) & isTRUE(interactive) & (!isTRUE(term_found)) & (length(term_list) != 0)){
    
    msg <- message(
      paste0(
        'The term "',
        x,
        '" could not be found in the LTER Controlled Vocabulary. Possible alternatives:',
        '\n'
        )
      )
    
    term_list <- c(term_list, 'NONE OF THE ABOVE')
    
    print.data.frame(as.data.frame(term_list))
    answer <- readline('Enter the row number of the term you would like to use: ')
    alternative_term <- as.character(term_list[as.numeric(answer)])
    message(paste0('You selected ... ', alternative_term, '\n'))
    
  }
  
  # Output results ------------------------------------------------------------
  
  if (!missing(interactive) & isTRUE(interactive) & (!isTRUE(term_found)) & (length(term_list) != 0)){
    
    alternative_term
    
  } else {
    
    term_found
    
  }

}
  