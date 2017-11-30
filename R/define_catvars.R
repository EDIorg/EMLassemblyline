#' Define categorical variables
#'
#' @description  
#'     Identify and define categorical variables for your data tables.
#'
#' @usage define_catvars(path)
#' 
#'     Run this function whenever your data contain attributes of class 
#'     "categorical" listed in \emph{attributes_datatablename.txt} files.
#'
#' @param path 
#'     A path to the dataset working directory containing 
#'     \emph{attributes_datatablename.txt} and the respecive data tables.
#'
#' @return 
#'     A tab delimited UTF-8 file in the dataset working directory titled 
#'     \emph{catvars_datatablename.txt} containing unique codes of attributes 
#'     of class "categorical". Open this file in a spreadsheet editor to 
#'     supply definitions for listed codes.
#'     
#' @details 
#'     This function will not overwrite any 
#'     \emph{catvars_datatablename.txt} files you have created in 
#'     the dataset working directory.
#'     
#'     This function identifies "categorical" class attributes from the file 
#'     \emph{attributes_datatablename.txt} and extracts unique 
#'     values of these attributes to the table for you to define.
#'
#' @export
#'

define_catvars <- function(path) {
  
  # Check arguments
  
  if (missing(path)){
    stop("Specify path to your dataset working directory.")
  }
  
  # Detect users operating system
  
  sysinfo <- Sys.info()["sysname"]
  if (sysinfo == "Darwin"){
    os <- "mac"
  } else {
    os <- "win"
  }
  
  # Get attribute file names and data file names
  
  message("Identifying data table names.")
  
  files <- list.files(path)
  use_i <- str_detect(string = files,
                      pattern = "^attributes")
  attribute_files <- files[use_i]
  table_names_base <- str_sub(string = attribute_files,
                              start = 12,
                              end = nchar(attribute_files)-4)
  use_i <- str_detect(string = files,
                      pattern = str_c("^", table_names_base, collapse = "|"))
  table_names <- files[use_i]
  data_files <- table_names
  
  # Set file names to be written

  fname_table_catvars <- str_c("catvars_", table_names_base, ".txt")

  # Auto detect field delimiters of input data files
  # and construct data file paths
  
  delim_guess <- c()
  file_path <- c()
  for (i in 1:length(data_files)){
    
    file_path[i] <- paste(path,
                          "/",
                          data_files[i],
                          sep = "")
    
    nlines <- length(readLines(file_path[i]))
    
    if (os == "mac"){
      delim_guess[i] <- suppressWarnings(get.delim(file_path[i],
                                  n = 2,
                                  delims = c("\t",
                                             ",",
                                             ";",
                                             "|")))
    } else if (os == "win"){
      delim_guess[i] <- get.delim(file_path[i],
                                  n = nlines/2,
                                  delims = c("\t",
                                             ",",
                                             ";",
                                             "|"))
    }
  }
  
  # Loop through data tables --------------------------------------------------
  
  for (i in 1:length(attribute_files)){
    
    use_i <- str_detect(string = files,
                        pattern = fname_table_catvars[i])
    
    if (sum(use_i) > 0){
      
      message(paste(files[use_i], "already exists! Skipping this one."))
      
    } else {
      
      # Read attributes_datatablename.txt
      
      message(paste("Reading ", attribute_files[i], ".", sep = ""))
      
      df_attributes <- read.table(
        paste(path, 
              "/", 
              attribute_files[i],
              sep = ""),
        header=TRUE,
        sep="\t",
        quote="\"",
        as.is=TRUE,
        comment.char = "",
        colClasses = rep("character", 7))
      
      colnames(df_attributes) <- c("attributeName",
                                   "attributeDefinition",
                                   "class",
                                   "unit",
                                   "dateTimeFormatString",
                                   "missingValueCode",
                                   "missingValueCodeExplanation")
      
      # Build catvars table
      
      catvars_I <- which(df_attributes$class %in% "categorical")
      
      # Read data table
      
      message(paste("Reading ", table_names[i], ".", sep = ""))
      
      df_table <- read.table(file_path[i],
                             header=TRUE,
                             sep=delim_guess[i],
                             quote="\"",
                             as.is=TRUE,
                             comment.char = "")

      # If there are no catvars then skip to the next file
      
      if (length(catvars_I) > 0){
        
        message("Identifying categorical variables.")
        
        rows <- 0
        for (j in 1:length(catvars_I)){
          factor_names <- unique(
            eval(
              parse(
                text = paste(
                  "df_table",
                  "$",
                  df_attributes$attributeName[catvars_I[j]],
                  sep = ""))))
          
          rows <- length(factor_names) + rows
          
        }
        
        catvars <- data.frame(attributeName = character(rows),
                              code = character(rows),
                              definition = character(rows),
                              stringsAsFactors = F)
        
        row <- 1
        for (j in 1:length(catvars_I)){
          
          factor_names <- unique(
            eval(
              parse(
                text = paste(
                  "df_table",
                  "$",
                  df_attributes$attributeName[catvars_I[j]],
                  sep = ""))))
          
          catvars$attributeName[row:(length(factor_names)+row-1)] <-
            df_attributes$attributeName[catvars_I[j]]
          
          catvars$code[row:(length(factor_names)+row-1)] <- factor_names
          
          row <- row + length(factor_names)
          
        }
        
        # Remove rows with empty codes
        
        use_i <- catvars$code == ""
        if (sum(use_i, na.rm = T) > 0){
          use_i <- match("", catvars$code)
          index <- seq(length(catvars$code))
          use_i <- index %in% use_i
          catvars <- catvars[!use_i, ]
        }

        # Write catvar table
        
        message(paste("Writing", fname_table_catvars[i]))
        
        write.table(catvars,
                    paste(path,
                          "/",
                          fname_table_catvars[i],
                          sep = ""),
                    sep = "\t",
                    row.names = F,
                    quote = F,
                    fileEncoding = "UTF-8")

      } else {
        
        message("No categorical variables found.")
        
      }
      
    }
    
  }
  
  message("Done.")

}

