#' Compile attributes and write to file
#'
#' @description  A function to compile attributes for \emph{define_factors} 
#'     and \emph{create_eml}.
#'
#' @usage compile_attributes(path)
#' 
#'     Run this function whenever any of your 
#'     \emph{datasetname_datatablename_attributes_draft.xlsx} or referenced 
#'     data tables have changed.
#'     
#'     Note: This function overwrites any 
#'     \emph{datasetname_datatablename_attributes.xlsx} files you have created in 
#'     the dataset working directory. To prevent overwriting of a specific 
#'     file, temporarily move the file out of the working directory.
#'
#' @param path A path to the dataset working directory containing the 
#'     completed \emph{datasetname_datatablename_attributes_draft.xlsx}, and
#'     \emph{eml_configuration.R} files as well as the referenced data table. 
#'
#' @return A file in the dataset working directory titled 
#'     \emph{datasetname_datatablename_attributes.xlsx} containing attribute 
#'     information called upon by the functions \emph{define_factors} and 
#'     \emph{create_eml}.
#'     
#' @details 
#'     This function compiles information from 
#'     \emph{datasetname_datatablename_attributes_draft.xlsx}, extracts 
#'     attribute number types and minimum and maximum values from numeric 
#'     attribues of your data table, and formats this meta data for the 
#'     functions \emph{define_factors} and \emph{create_eml}.
#'
#' @export
#'
#' @seealso \code{\link{copy_templates}} to copy metadata templates to the 
#'     dataset working directory.
#' @seealso \code{\link{run_guide}} for guidance on completing the template 
#'     files.
#' @seealso \code{\link{define_factors}} to create the factors table if the 
#'     attributes table contains factors.
#' @seealso \code{\link{extract_geocoverage}} to extract detailed geographic 
#'     coordinates of sampling sites.
#' @seealso \code{\link{create_eml}} to write the EML file.


compile_attributes <- function(path){
  
  # Parameterize function
  
  #library("EML")
  #library("xlsx")
  #library("rmarkdown")
  
  # Load configuration file

  source(paste(path,
               "/eml_configuration.R",
               sep = ""))
  
  template <- paste(dataset_name,
                    "_template.docx",
                    sep = "")

  # # Get system information
  # 
  # sysinfo <- Sys.info()["sysname"]
  # if (sysinfo == "Darwin"){
  #   os <- "mac"
  # } else {
  #   os <- "win"
  # }

  # Issue warning

  answer <- readline(
    "Are you sure you want to build new attributes? This will overwrite your previous work! (y or n):  ")

  if (answer == "y"){

    # Set file names to be written

    fname_table_attributes <- c()

    for (i in 1:length(table_names)){

      fname_table_attributes[i] <- paste(
        substr(table_names[i], 1, nchar(table_names[i]) - 4),
        "_attributes.xlsx",
        sep = "")

    }

    
    # Loop through data tables --------------------------------------------------

    for (i in 1:length(table_names)){

      print(paste("Now compiling attributes for ... ",
                  table_names[i], sep = ""))

      # Read data table

      if (field_delimeter[i] == "comma"){
        
        df_table <- read.table(
          paste(path, "/", table_names[i], sep = ""),
          header=TRUE,
          sep=",",
          quote="\"",
          as.is=TRUE,
          comment.char = "")
        
      } else if (field_delimeter[i] == "tab"){
        
        df_table <- read.table(
          paste(path, "/", table_names[i], sep = ""),
          header=TRUE,
          sep="\t",
          quote="\"",
          as.is=TRUE,
          comment.char = "")
        
      }
      
      # Read attributes_draft table
      
      df_attributes <- xlsx::read.xlsx2(
        paste(
          path,
          "/",
          substr(fname_table_attributes[i],
                 1,
                 nchar(fname_table_attributes[i]) - 5),
          "_draft.xlsx",
          sep = ""),
        sheetIndex = 1)
      
      for (j in 1:dim(df_attributes)[2]){
        df_attributes[ ,j] <- as.character(df_attributes[ ,j])
      }
      
      colnames(df_attributes) <- c("attributeName",
                                   "attributeDefinition",
                                   "class",
                                   "unit",
                                   "dateTimeFormatString",
                                   "missingValueCode",
                                   "missingValueCodeExplanation")
      
      # Initialize outgoing attribute table 

      rows <- ncol(df_table)
      attributes <- data.frame(attributeName = character(rows),
                               formatString = character(rows),
                               unit = character(rows),
                               numberType = character(rows),
                               definition = character(rows),
                               attributeDefinition = character(rows),
                               columnClasses = character(rows),
                               minimum = character(rows),
                               maximum = character(rows),
                               missingValueCode = character(rows),
                               missingValueCodeExplanation = character(rows),
                               stringsAsFactors = FALSE)

      # Set attribute names

      attributes$attributeName <- df_attributes$attributeName

      # Set attribute definition (i.e. "attributeDefinition")
      
      attributes$attributeDefinition <- df_attributes$attributeDefinition
      
      # Set attribute class
      
      attributes$columnClasses <- df_attributes$class
      
      # Set attribute units
      
      attributes$unit <- df_attributes$unit
      
      # Set attribute date time format string
      
      attributes$formatString <- df_attributes$dateTimeFormatString
      
      # Set attribute missing value code
      
      attributes$missingValueCode <- df_attributes$missingValueCode
      
      # Set attribute missing value code explanation
      
      attributes$missingValueCodeExplanation <- df_attributes$missingValueCodeExplanation
      
      # Set attribute number type

      is_numeric <- which(attributes$columnClasses == "numeric")
      
      for (j in 1:length(is_numeric)){
        raw <- df_table[ ,is_numeric[j]]
        rounded <- floor(df_table[ ,is_numeric[j]])
        if (length(raw) - sum(raw == rounded, na.rm = T) > 0){
          attributes$numberType[is_numeric[j]] <- "real"
        } else if (min(df_table[ ,is_numeric[j]], na.rm = T) > 0){
          attributes$numberType[is_numeric[j]] <- "natural"
        } else if (min(df_table[ ,is_numeric[j]], na.rm = T) < 0){
          attributes$numberType[is_numeric[j]] <- "integer"
        } else {
          attributes$numberType[is_numeric[j]] <- "whole"
        }
      }
      
      is_character <- which(attributes$columnClasses == "character") 
      is_factor <- which(attributes$columnClasses == "factor")
      use_i <- c(is_character, is_factor)
      
      attributes$numberType[use_i] <- "character"

      # Set attribute minimum and maximum values
      
      attributes$minimum <- as.numeric(attributes$minimum)
      attributes$maximum <- as.numeric(attributes$maximum)
      
      for (j in 1:length(is_numeric)){
        attributes$minimum[is_numeric[j]] <- round(min(df_table[ ,is_numeric[j]],
                                           na.rm = TRUE),
                                       digits = 2)
      }
      
      for (j in 1:length(is_numeric)){
        attributes$maximum[is_numeric[j]] <- round(max(df_table[ ,is_numeric[j]],
                                           na.rm = TRUE),
                                       digits = 2)
      }

      # Set attribute definition (i.e. "definition")
      
      use_i <- c(is_character, is_factor)
      
      if (length(use_i) > 0){
        attributes$definition[use_i] <- attributes$attributeDefinition[use_i]
      }

      # Write attributes table to file

      xlsx::write.xlsx(attributes,
                 paste(path,
                       "/",
                       fname_table_attributes[i],
                       sep = ""),
                 col.names = T,
                 row.names = F,
                 showNA = F)

    }

  }

}
