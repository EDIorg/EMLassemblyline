#' Define categorical variables
#'
#' @description  
#'     Identify and define categorical variables of your data tables.
#'
#' @usage define_catvars(path)
#' 
#'     Run this function whenever your data contain attributes of class 
#'     "categorical" as listed in 
#'     \emph{datasetname_datatablename_attributes.txt} files.
#'
#' @param path 
#'     A path to the dataset working directory containing 
#'     \emph{datasetname_datatablename_attributes.txt}, 
#'     \emph{eml_configuration.R}, and referenced data tables.
#'
#' @return 
#'     A tab delimited UTF-8 file in the dataset working directory titled 
#'     \emph{datasetname_datatablename_catvars.txt} containing unique values 
#'     of attributes of class "categorical" which is translated and written 
#'     to EML with \code{make_eml}.
#'     
#' @details 
#'     This function overwrites any 
#'     \emph{datasetname_datatablename_catvars.txt} files you have created in 
#'     the dataset working directory. To prevent overwriting of these files, 
#'     temporarily move them out of the working directory.
#'     
#'     This function identifies "categorical" class attributes from the file 
#'     \emph{datasetname_datatablename_attributes.txt} and extracts unique 
#'     values of these attributes to the table for you to define. Do not define 
#'     categorical variables with empty field contents. Delete these rows from 
#'     this file.
#'     
#'     When defining categorical variables with unit values, refer to the 
#'     standard unit dictionary "name" column. Enter the unit name in the 
#'     definition column of the categorical variables table. Note these values 
#'     are case sensitive.
#'
#' @export
#'
#' @seealso 
#'     \code{\link{import_templates}} to import metadata templates to the 
#'     dataset working directory.
#' @seealso
#'     \code{\link{view_instructions}} for instructions on completing the 
#'     template files.
#' @seealso 
#'     \code{\link{extract_geocoverage}} to extract detailed geographic 
#'     coordinates of sampling sites.
#' @seealso 
#'     \code{\link{make_eml}} to translate user supplied information into the 
#'     EML file.


define_catvars <- function(path) {
  
  # Check arguments
  
  if (missing(path)){
    stop("Specify path to dataset working directory.")
  }
  
  # Load the configuration file
  
  print("Loading configuration file ...")
  
  source(paste(path, "/eml_configuration.R", sep = ""))
  
  template <- paste(dataset_name,
                    "_template.docx",
                    sep = "")

  # List expected attribute files

  attribute_files <- c()
  
  for (i in 1:length(table_names)){
    attribute_files[i] <- paste(
      substr(table_names[i], 1, nchar(table_names[i]) - 4),
      "_attributes.txt",
      sep = "")
  }

  if (length(attribute_files) == 0){
    stop("No attribute files found ... run copy_templates to import attributes table, then fill them out.")
  }

  # Set file names to be written

  fname_table_catvars <- c()
  for (i in 1:length(table_names)){
    fname_table_catvars[i] <- paste(
      substr(table_names[i], 1, nchar(table_names[i]) - 4),
      "_catvars.txt",
      sep = ""
    )
  }

  # Issue warning

  answer <- readline(
    "Are you sure you want to build new categorical variable tables? This will overwrite your previous work! (y or n):  ")
  
  if (answer == "y"){
    
    # Loop through data tables ------------------------------------------------
    
    for (i in 1:length(attribute_files)){
      
      print(paste("Creating", fname_table_catvars[i]))

      # Read attribute_draft.csv file
      
      print(paste("Reading", attribute_files[i]))
      
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

      print("Identifying categorical variables ...")
      
      catvars_I <- which(df_attributes$class %in% "categorical")
      
      # Read data table
      
      print("Reading data table ...")
      
      if (field_delimeter[i] == "comma"){
        
        df_table <- read.table(
          paste(path, 
                "/", 
                substr(attribute_files[i], 1, nchar(attribute_files[i]) - 15),
                ".csv",
                sep = ""),
          header=TRUE,
          sep=",",
          quote="\"",
          as.is=TRUE,
          comment.char = "")
        
      } else if (field_delimeter[i] == "tab"){
        
        df_table <- read.table(
          paste(path, 
                "/", 
                substr(attribute_files[i], 1, nchar(attribute_files[i]) - 15),
                ".txt",
                sep = ""),
          header=TRUE,
          sep="\t",
          quote="\"",
          as.is=TRUE,
          comment.char = "")
        
      }

      # If there are no catvars then skip to the next file

      if (length(catvars_I) > 0){
        
        print("Compiling catvars table ...")

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

        # Write factor table
        
        print(paste("Writing", fname_table_catvars[i]))
        
        write.table(catvars,
                    paste(path,
                          "/",
                          fname_table_catvars[i],
                          sep = ""),
                    sep = "\t",
                    row.names = F,
                    quote = F,
                    fileEncoding = "UTF-8")

      }
      
      writeLines("\n")

    }
    
    # Prompt the user to manually edit the catvars file and custom unit files.
    
    standardUnits <- get_unitList()
    View(standardUnits$units)
    
    readline(
      prompt = "Open the categorical variables file for this data table and define factor codes. Save, close, and press <enter> when done.")

  }

}

