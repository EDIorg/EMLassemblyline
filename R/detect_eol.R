#' Detect end of line (EOL) character
#'
#' @description  
#'     Detect EOL character of input input files and return value
#'
#' @usage detect_eol(path = "", file.name = "", os = "")
#' 
#' @param path 
#'     A character string specifying a path to the target file directory.
#' @param file.name
#'     A character string specifying the target file name. 
#' @param os
#'     A character string specifying the operating system in which this 
#'     function is to be called. Valid options are generated from 
#'     \code{detect_os}.
#' 
#' @return 
#'     A character string representation of the EOL character.
#'     \item{"\\r"}
#'     \item{"\\n"}
#'     \item{"\\r\\n"}
#'
#' @export
#'
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

  if (os == 'mac'){

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

  } else if (os == 'win'){

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
    
  }

  use_i <- str_detect(
    output,
    '\\\\r  \\\\n'
  )

  if (sum(use_i) > 0){
    eol <- '\\\\r\\\\n'
  } else {
    use_i <- str_detect(
      output,
      '\\\\n'
    )
    if (sum(use_i) > 0){
      eol <- '\\\\n'
    } else {
      eol <- '\\\\r'
    }
  } 

  eol

}
