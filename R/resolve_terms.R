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
#'
#' @return 
#'     (character) Controlled vocabulary names corresponding to successfully
#'     resolved terms.
#'
#' @export
#'


resolve_terms <- function(x, cv){
  
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
  
  # Initialize output ---------------------------------------------------------
  
  output <- data.frame(
    term = x,
    controlled_vocabulary = character(length(x)),
    stringsAsFactors = F)
  
  # Call specified vocabularies -----------------------------------------------
  
  if (cv == 'lter'){
    use_i <- unlist(lapply(x, FUN = lter_term))
    output[use_i, 'controlled_vocabulary'] <- 'LTER Controlled Vocabulary'
  }
  
  # Return output -------------------------------------------------------------
  
  output
  
}
