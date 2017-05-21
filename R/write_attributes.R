#' Compile attributes and write to file
#'
#' @description  A function to compile attributes for \emph{write_factors} 
#'     and \emph{create_eml}.
#'
#' @usage write_attributes(path)
#' 
#'     Run this function whenever any of your 
#'     \emph{datasetname_datatablename_attributes_draft.xlsx} or referenced 
#'     data tables have changed.
#'
#' @param path A path to the dataset working directory containing the 
#'     completed \emph{datasetname_datatablename_attributes_draft.xlsx}, and
#'     \emph{eml_configuration.R} files as well as the referenced data table. 
#'
#' @return A file in the dataset working directory titled 
#'     \emph{datasetname_datatablename_attributes.xlsx} containing attribute 
#'     information called upon by the functions \emph{write_factors} and 
#'     \emph{create_eml}.
#'     
#' @details 
#'     This function compiles information from 
#'     \emph{datasetname_datatablename_attributes_draft.xlsx}, extracts 
#'     attribute number types and minimum and maximum values from numeric 
#'     attribues of your data table, and formats this meta data for the 
#'     functions \emph{write_factors} and \emph{create_eml}.
#'
#' @seealso \code{\link{copy_templates}} to copy metadata templates to the 
#'     dataset working directory.
#' @seealso \code{\link{run_guide}} for guidance on completing the template 
#'     files.
#' @seealso \code{\link{write_factors}} to create the factors table if the 
#'     attributes table contains factors.
#' @seealso \code{\link{extract_geocoverage}} to extract detailed geographic 
#'     coordinates of sampling sites.
#' @seealso \code{\link{create_eml}} to write the EML file.


write_attributes <- function(path){
  
  # Parameterize function
  
  library("EML")
  library("xlsx")
  #library("rmarkdown")
  
  template <- paste(dataset_name,
                    "_template.docx",
                    sep = "")
  #template <- trimws(list.files(path, pattern = "*_template.docx"))
  
  # Load configuration file

  source(paste(path,
               "/eml_configuration.R",
               sep = ""))

  # Get system information

  sysinfo <- Sys.info()["sysname"]
  if (sysinfo == "Darwin"){
    os <- "mac"
  } else {
    os <- "win"
  }

  # Issue warning

  answer <- readline(
    "Are you sure you want to build new attributes? This will overwrite your previous work! (y or n):  ")

  if (answer == "y"){

    # Set file names

    fname_table_attributes <- c()

    for (i in 1:length(table_names)){

      fname_table_attributes[i] <- paste(
        substr(table_names[i], 1, nchar(table_names[i]) - 4),
        "_attributes.xlsx",
        sep = "")

    }

    # Initialize custom unit files

    custom_units <- data.frame(id = character(1),
                               unitType = character(1),
                               parentSI = character(1),
                               multiplierToSI = character(1),
                               description = character(1),
                               stringsAsFactors = FALSE)

    xlsx::write.xlsx(custom_units,
               paste(path,
                     "/",
                     substr(template, 1, nchar(template) - 14),
                     "_custom_units.xlsx", sep = ""),
               col.names = T,
               row.names = F,
               showNA = F)

    
    # Loop through data tables --------------------------------------------------

    for (i in 1:length(table_names)){

      print(paste("Now building attributes for ... ",
                  table_names[i], sep = ""))

      # Read data table

      if (field_delimeter[i] == ","){
        
        df_table <- read.table(
          paste(path, "/", table_names[i], sep = ""),
          header=TRUE,
          sep=",",
          quote="\"",
          as.is=TRUE,
          comment.char = "")
        
      } else if (field_delimeter[i] == "\\t"){
        
        df_table <- read.table(
          paste(path, "/", table_names[i], sep = ""),
          header=TRUE,
          sep="\t",
          quote="\"",
          as.is=TRUE,
          comment.char = "")
        
      }
      
      # Rename columns

      if (length(new_attribute_names[[i]]) > 0){
        names(df_table) <- new_attribute_names[[i]]
      }

      # Initialize attribute table

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

      # Set names

      attributes$attributeName <- names(df_table)

      # Set number type

      attributes$columnClasses <- sapply(df_table, class)

      is_integer <- which(attributes$columnClasses == "integer")

      is_numeric <- which(attributes$columnClasses == "numeric")

      is_character <- which(attributes$columnClasses == "character")

      for (j in 1:length(is_integer)){
        if (min(df_table[ ,is_integer[j]], na.rm = T) > 0){
          attributes$numberType[is_integer[j]] <- "natural"
        } else if (min(df_table[ ,is_integer[j]], na.rm = T) < 0){
          attributes$numberType[is_integer[j]] <- "integer"
        } else {
          attributes$numberType[is_integer[j]] <- "whole"
        }
      }

      for (j in 1:length(is_numeric)){
        raw <- df_table[ ,is_numeric[j]]
        rounded <- floor(df_table[ ,is_numeric[j]])
        if (length(raw) - sum(raw == rounded, na.rm = T) > 0){
          attributes$numberType[is_numeric[j]] <- "real"
        }
      }

      for (j in 1:length(is_character)){
        attributes$numberType[is_character[j]] <- "character"
        attributes$minimum[is_character[j]] <- ""
        attributes$maximum[is_character[j]] <- ""
      }

      # Set value range

      attributes$minimum <- sapply(df_table, min, na.rm = TRUE)
      attributes$maximum <- sapply(df_table, max, na.rm = TRUE)
      attributes$minimum[attributes$columnClasses != "character"] <- round(
        as.numeric(
          attributes$minimum[attributes$columnClasses != "character"]
        ), digits = 2
      )

      attributes$maximum[attributes$columnClasses != "character"] <- round(
        as.numeric(attributes$maximum[attributes$columnClasses != "character"]
        ), digits = 2
      )

      for (j in 1:length(is_character)){
        attributes$minimum[is_character[j]] <- ""
        attributes$maximum[is_character[j]] <- ""
      }

      # Set missing value code "NA"

      for (j in 1:nrow(attributes)){
        if (sum(is.na(df_table[ ,j])) > 0){
          attributes$missingValueCode[j] <- "NA"
        }
      }

      attributes$missingValueCodeExplanation[
        attributes$missingValueCode == "NA"] <- "not available"

      # Set column classes

      attributes$columnClasses[
        attributes$columnClasses == "integer"] <- "numeric"

      potential_factors <- which(
        attributes$columnClasses %in% "character")  # Identify factors

      for (j in 1:length(potential_factors)){
        if (length(unique(df_table[ ,potential_factors[j]])) < 40){
          attributes$columnClasses[potential_factors[j]] <- "factor"
        }
      }

      # Set definitions (attributeDefinition)

      df_attributes <- xlsx::read.xlsx2(
        paste(
          path,
          "/",
          substr(fname_table_attributes[i],
                 1,
                 nchar(fname_table_attributes[i]) - 5),
          "_draft.xlsx",
          sep = ""),
        sheetIndex = 1,
        colClasses = c(rep("character",4)))

      colnames(df_attributes) <- c("columnName",
                                "description",
                                "unitOrCodeExplanationOrDateFormat",
                                "emptyValueCode")

      attributes$attributeDefinition <- trimws(
        df_attributes$description[
          match(trimws(attributes$attributeName),
                trimws(df_attributes$columnName))])

      use_I <- is.na(match(trimws(attributes$attributeName),
                           trimws(df_attributes$columnName))) # Tag fields where attribute names don't match between the data table and user supplied template

      attributes$attributeDefinition[use_I] <-
        "!!! Non-matching attribute names !!!"

      # Set definition (definition).
      # For nominal variables the attributeDefinition must be placed in definition.

      attributes$definition[attributes$columnClasses == "factor"] <-
        attributes$attributeDefinition[attributes$columnClasses == "factor"]

      # Set units

      attributes$unit <- trimws(df_attributes$unitOrCodeExplanationOrDateFormat[
        match(attributes$attributeName, df_attributes$columnName)])

      attributes$unit[(attributes$numberType == "real") &
                        (attributes$columnClasses == "factor")] <- "number"

      # Adjust attributes column classes

      attributes$minimum <- as.numeric(attributes$minimum)

      attributes$maximum <- as.numeric(attributes$maximum)

      # Write attributes table to file

      xlsx::write.xlsx(attributes,
                 paste(path,
                       "/",
                       fname_table_attributes[i],
                       sep = ""),
                 col.names = T,
                 row.names = F,
                 showNA = F)

      # Prompt the user to manually edit the attributes file and custom unit files.

      standardUnits <- get_unitList()
      View(standardUnits$units)

      if (os == "mac"){
        system(paste("open",
                     paste(path,
                           "/",
                           fname_table_attributes[i],
                           sep = "")))

        system(paste("open ",
                     path,
                     "/",
                     substr(template, 1, nchar(template) - 14),
                     "_custom_units.xlsx",
                     sep = ""))

      } else if (os == "win"){

        shell.exec(paste(path,
                         "/",
                         fname_table_attributes[i],
                         sep = ""))

        shell.exec(paste(path,
                         "/",
                         substr(template, 1, nchar(template) - 14),
                         "_custom_units.xlsx", sep = ""))

      }

      readline(
        prompt = "Press <enter> once attributes file and any custom units files have been edited, saved, and closed.")

    }


  }

}
