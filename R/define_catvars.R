#' Define categorical variables
#'
#' @description  
#'     Identify and define categorical variables for your data tables.
#'
#' @usage define_catvars(path, data.path = path)
#' 
#'     Run this function whenever your data contain attributes of class 
#'     "categorical" listed in \emph{attributes_datatablename.txt} files.
#'
#' @param path 
#'     (character) A path to your metadata templates directory containing 
#'     \emph{attributes_datatablename.txt}.
#' @param data.path 
#'     (character) A path to your data directory. Don't use this argument
#'     if your data are co-located with your metadata templates.
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
#'     Duplicate data file names are not allowed, even if they are different 
#'     file types.
#'
#' @export
#'

define_catvars <- function(path, data.path = path) {
  
  
  # Check arguments and parameterize ------------------------------------------
  
  if (missing(path)){
    stop('Input argument "path" is missing! Specify the path to your dataset working directory.')
  }
  
  # Validate paths
  
  EDIutils::validate_path(path)
  if (!missing(data.path)){
    EDIutils::validate_path(data.path)  
  }
  
  # Detect operating system
  
  os <- detect_os()
  
  # Get attribute file names and data file names
  
  message("Identifying data table names.")
  
  files <- list.files(path)
  use_i <- str_detect(string = files,
                      pattern = "^attributes")
  if (sum(use_i) == 0){
    stop('There are no attributes.txt files in your dataset working directory. Please fix this.')
  }
  attribute_files <- files[use_i]
  table_names_base <- str_sub(string = attribute_files,
                              start = 12,
                              end = nchar(attribute_files)-4)
  data_files <- list.files(data.path)
  use_i <- str_detect(string = data_files,
                      pattern = str_c("^", table_names_base, collapse = "|"))
  table_names <- data_files[use_i]
  data_files <- table_names
  
  # Send warning if data table name is repeated more than once
  
  if (length(unique(tools::file_path_sans_ext(data_files))) != length(data_files)){
    stop('Duplicate data file names exist in this directory. Please remove duplicates, even if they are a different file type.')
  }
  
  # Validate fields of data.files
  
  validate_fields(path = data.path, data.files = data_files)
  
  # Set file names to be written

  fname_table_catvars <- str_c("catvars_", table_names_base, ".txt")

  # Detect field delimiters of data files
  
  delim_guess <- detect_delimeter(path = data.path, data.files = data_files, os)
  
  
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
        header = T,
        sep="\t",
        quote="\"",
        as.is=TRUE,
        comment.char = "",
        fill = T,
        colClasses = rep("character", 7))
      df_attributes <- df_attributes[ ,1:7]
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
      
      data_path <- paste(data.path,
                            "/",
                            data_files[i],
                            sep = "")
      
      if (delim_guess[i] == ','){
        df_table <- suppressMessages(
          read_csv(
            file = data_path
            )
          )
      } else if (delim_guess[i] == '\t'){
        df_table <- suppressMessages(
          read_tsv(
            file = data_path
          )
        )
      }
      
      # df_table <- read.table(data_path,
      #                        header=TRUE,
      #                        sep=delim_guess[i],
      #                        quote="\"",
      #                        as.is=TRUE,
      #                        comment.char = "")

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
        
        suppressWarnings(write.table(catvars,
                    paste(path,
                          "/",
                          fname_table_catvars[i],
                          sep = ""),
                    sep = "\t",
                    row.names = F,
                    quote = F,
                    fileEncoding = "UTF-8"))

      } else {
        
        message("No categorical variables found.")
        
      }
      
    }

  }
  
  message("Done.")

}

