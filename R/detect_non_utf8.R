#' Detect non-UTF-8 encoded character strings
#'
#' @description  
#'     Identify non-UTF-8 encoded character strings and report location 
#'     relative to user supplied inputs.
#'
#' @usage 
#'     detect_non_utf8(x = "", x.type = "")
#'
#' @param x
#'     A variable containing character strings.
#' @param x.source
#'     File name from which x was created. This value is referenced when 
#'     reporting errors to the user and prompting them to fix these errors.
#'     Most often this should be a file name the user can modify.
#'
#' @export
#'


detect_non_utf8 <- function(x, x.type, x.source){
  
  # Check arguments and parameterize ------------------------------------------
  
  message('Checking input arguments.')
  
  if (missing(x)){
    stop('Input argument "x" is missing! Specify the variable to be tested.')
  }
  
  # Helper function to report location of non-utf-8 in table
  
  error_report <- function(input, x.source, x_col){
    paste0('Non-UTF-8 encoded character strings were found in the file "', x.source, '" and in the column "', x_col ,'". Row numbers and suspect values are listed below. Please replace these non-valid UTF-8 encoded character strings and try again.\n',
          paste(capture.output(print(input, row.names = F)), collapse = "\n"))
  }
  
  # Test character vector -----------------------------------------------------
  
  if (is.character(x)){
    info <- stri_enc_isutf8(x)
    if (!isTRUE(info)){
      stop(paste0('This line contains Non-UTF-8 encoded characters: "', x, '" ... Please remove suspect characters.'))
    }
    
  }
  
  
  
  # Test data.frame -----------------------------------------------------------
  
  if (is.data.frame(x)){
    
    message(paste0('Searching ', x.source))
    
    if (missing(x.source)){
      stop('Input argument "x.source" is missing! Specify the data source from which "x" was created.')
    }
    
    # Identify fields to check (character, factor)
    
    use_i <- unlist(lapply(x, class)) == "factor"
    x[use_i] <- lapply(x[use_i], as.character)
    use_i <- unlist(lapply(x, class)) == "character"
    
    # Fail at first field with suspect encodings
    
    x_test <- x[use_i]
    for (i in 1:ncol(x_test)){
      use_i <- stri_enc_isutf8(x_test[ ,i])
      if (sum(!use_i) > 0){
        error_col <- colnames(x_test[i])
        error_df <- data.frame(row = seq(1:nrow(x_test[i]))[!use_i],
                               value = x_test[!use_i, i],
                               row.names = NULL)
        stop(error_report(input = error_df, x.source = x.source, x_col = error_col))
      }
    }

  }
  
  
  message('Done.')
  
}