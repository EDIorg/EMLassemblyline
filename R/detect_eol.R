#' Detect end of line (EOL) character
#'
#' @description  
#'     Detect EOL character of input file(s).
#'
#' @usage detect_eol(path, file.name, os)
#' 
#' @param path 
#'     (character) A path to the target file directory.
#' @param file.name
#'     (character) The target file name. 
#' @param os
#'     (character) The operating system in which this function is called
#'     called. Valid options are generated from \code{detect_os}.
#' 
#' @return 
#'     A character string representation of the EOL character.
#'
#' @export
#'


detect_eol <- function(path, file.name, os){

  # Check arguments -----------------------------------------------------
  
  if (missing(path)){
    stop('Input argument "path" is missing! Specify the path to the directory containing the target file.')
  }
  if (missing(file.name)){
    stop('Input argument "file.name" is missing! Specify the names of the target file.')
  }
  if (missing(os)){
    stop('Input argument "os" is missing! Specify your operating system.')
  }

  # Validate path

  validate_path(path)

  # Validate file.name

  file_name <- validate_file_names(path, file.name)

  # Validate os

  if (isTRUE((os != "win") & (os != "mac") & (os != "lin"))){
    stop('The value of input argument "os" is invalid.')
  }
  
  # Detect end of line character ----------------------------------------------
  
  if (os == 'mac'){ # Macintosh OS
    
    command <- paste0(
      'od -c ',
      path,
      '/',
      file.name
    )
    
    output <- system(
      command,
      intern = T
    )
    
    use_i <- str_detect(
      output,
      '\\\\r  \\\\n'
    )
    
    if (sum(use_i) > 0){
      eol <- '\\r\\n'
    } else {
      use_i <- str_detect(
        output,
        '\\\\n'
      )
      if (sum(use_i) > 0){
        eol <- '\\n'
      } else {
        eol <- '\\r'
      }
    } 
    
  } else if ((os == 'win') | (os == 'lin')){ # Windows & Linux OS
    
    output <- readChar(
      paste0(
        path,
        '/',
        file.name
      ),
      nchars = 10000
    )
    
    eol <- parse_delim(output)
    
  }
  
  eol
  
}


# Parse delimiter from string -------------------------------------------------

parse_delim <- function(x){
  
  use_i <- str_detect(
    x,
    '\\r\\n'
  )
  
  if (sum(use_i) > 0){
    eol <- '\\r\\n'
  } else {
    use_i <- str_detect(
      x,
      '\\n'
    )
    if (sum(use_i) > 0){
      eol <- '\\n'
    } else {
      eol <- '\\r'
    }
  } 
  
  eol
  
}

