#' resolve_terms
#'
#' @description  
#'     Resolve terms to a controlled vocabulary.
#'
#' @usage 
#'     resolve_terms(x, cv)
#'
#' @param x
#'     (character) Term(s) to resolve to a controlled vocabulary. Can be a 
#'     vector of terms.
#' @param cv
#'     (character) A controlled vocabulary to search. Valid options are:
#'     \itemize{
#'         \item{lter} - The LTER Controlled Vocabulary (http://vocab.lternet.edu/vocab/vocab/index.php)
#'     }
#' @param messages 
#'     (logical) Display diagnostic messages, e.g. alternative spelling options.
#'
#' @return 
#'     (character) Controlled vocabulary names corresponding to successfully
#'     resolved terms.
#'
#' @export
#'


resolve_terms <- function(x, cv, messages = FALSE, interactive = FALSE){
  
  # Check arguments -----------------------------------------------------------
  
  if (missing(x)){
    stop('Input argument "x" is missing! Specify terms to resolve.')
  }
  if (is.character(x) != T){
    stop('Input argument "x" is not of class "character"!')
  }
  if (cv != 'lter'){
    stop('Input argument "cv" is not one of the allowed vocabularies!')
  }
  if (!missing(messages) & isTRUE(messages) & !missing(interactive) & isTRUE(interactive)){
    stop('Both arguments "messages" & "interactive" can not be used at the same time. Please select one or the other.')
  }
  
  # Initialize output ---------------------------------------------------------
  
  output <- data.frame(
    term = x,
    controlled_vocabulary = character(length(x)),
    stringsAsFactors = F)
  
  # Call specified vocabularies -----------------------------------------------
  
  if (cv == 'lter'){
    
    if (!missing(messages) & isTRUE(messages)){
      # Messages
      use_i <- unlist(lapply(x, FUN = lter_term, messages = T))
      output[use_i, 'controlled_vocabulary'] <- 'LTER Controlled Vocabulary'      
    } else if (!missing(interactive) & isTRUE(interactive)){
      # Interactive
      alternative_terms <- unlist(lapply(x, FUN = lter_term, interactive = T))
      use_i <- ((alternative_terms == 'NONE OF THE ABOVE') | (is.na(alternative_terms)))
      output[!use_i, 'term'] <- alternative_terms[!use_i]
      output$term[output$term == 'TRUE'] <- x[output$term == 'TRUE']
      output[!use_i, 'controlled_vocabulary'] <- 'LTER Controlled Vocabulary'      
      use_i <- output$term == FALSE
      output$term[use_i] <- x[use_i]
      output$controlled_vocabulary[use_i] <- ''
    } else {
      # Automatic
      use_i <- unlist(lapply(x, FUN = lter_term))
      output[use_i, 'controlled_vocabulary'] <- 'LTER Controlled Vocabulary'
    }
    
  }
  
  # Return output -------------------------------------------------------------
  
  output
  
}
