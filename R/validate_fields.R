#' Validate table fields
#'
#' @description  
#'     This function checks for congruence between fields of a data table.
#'
#' @usage 
#'     validate_fields(path = "", data.files = c("data.file.1", "data.file.2", "etc."))
#'
#' @param path 
#'     A character string specifying a path to the dataset working directory.
#' @param data.files
#'     A list of character strings specifying the names of the data files
#'     of your dataset. It is not necessary to include the file extension.
#'
#' @return 
#'     If incongruence is found an error is returned.
#'     
#' @export     
#'   

validate_fields <- function(path, data.files){
  
  # Check arguments and parameterize ------------------------------------------
  
  if (missing(path)){
    stop('Input argument "path" is missing! Specify the path to your dataset working directory.')
  }
  if (missing(data.files)){
    stop('Input argument "data.files" is missing! Specify the names of all the data files in your dataset.')
  }
  
  # Validate path
  
  validate_path(path)
  
  # Validate data.files
  
  data_files <- validate_file_names(path, data.files)
  
  # Detect operating system
  
  os <- detect_os()
  
  # Detect delimeter
  
  delim_guess <- detect_delimeter(path, data.files = data_files, os)
  
  # Detect inconsistent field lengths -----------------------------------------
  
  data_path <- c()
  for (i in 1:length(data_files)){
    
    data_path[i] <- paste(path,
                          "/",
                          data_files[i],
                          sep = "")
    
    # Which quote character produces the fewest NAs?
    
    count_quote <- count.fields(file = data_path[i],
                                sep = delim_guess[i],
                                quote = "\"")
    
    count_appos <- count.fields(file = data_path[i],
                                sep = delim_guess[i],
                                quote = "\'")
    
    if (sum(is.na(count_quote)) < sum(is.na(count_appos))){
      count_quote <- count_quote[!is.na(count_quote)]
      uni_count <- unique(count_quote)
      if (length(uni_count) > 1){
        stop(paste(data_files[i], "contains an inconsistent number of field delimeters. Please fix this."))
      }
    } else if (sum(is.na(count_appos)) < sum(is.na(count_quote))){
      count_appos <- count_appos[!is.na(count_appos)]
      uni_appos <- unique(count_appos)
      if (length(uni_appos) > 1){
        stop(paste(data_files[i], "contains an inconsistent number of field delimeters. Please fix this."))
      }
    } else if (sum(is.na(count_appos)) == sum(is.na(count_quote))){
      count_appos <- count_appos[!is.na(count_appos)]
      uni_appos <- unique(count_appos)
      if (length(uni_appos) > 1){
        stop(paste(data_files[i], "contains an inconsistent number of field delimeters. Please fix this."))
      }
    }
  }

}