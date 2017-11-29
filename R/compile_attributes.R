# This is a helper function for make_eml.R. 
# It compiles attributes, retrieves minimum and maximum values for numeric data
# and reformats the attributes table.


compile_attributes <- function(path){

  # Load the datasets configuration file
  
  source(paste(path, "/configuration.R", sep = ""))
  
  # Set file names to be written 

  files <- list.files(path)
  use_i <- str_detect(string = files,
                      pattern = "^attributes")
  attribute_files <- files[use_i]
  fname_table_attributes <- attribute_files
  table_names_base <- str_sub(string = attribute_files,
                              start = 12,
                              end = nchar(attribute_files)-4)
  use_i <- str_detect(string = files,
                      pattern = str_c("^", table_names_base, collapse = "|"))
  table_names <- files[use_i]
  

  # Loop through data tables --------------------------------------------------
  
  attributes_stored <- list()

  for (i in 1:length(table_names)){

    message(paste("Compiling", fname_table_attributes[i], "..."))

    
    file_path <- paste(path,
                       "/",
                       table_names[i],
                       sep = "")
    
    if (os == "mac"){
      delim_guess <- get.delim(file_path,
                               n = 2)
    } else if (os == "win"){
      delim_guess <- get.delim(file_path,
                               n = 1)
    }
    
    df_table <- read.table(file_path,
                           header = TRUE,
                           sep = delim_guess,
                           quote = "\"",
                           as.is = TRUE,
                           comment.char = "")
    
    # Read attributes_datatablename
    
    df_attributes <- read.table(paste(path,
                                      "/",
                                      fname_table_attributes[i],
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
    
    use_i <- is.na(attributes$missingValueCode)
    attributes$missingValueCode[use_i] <- "NA"
    
    # Set attribute missing value code explanation
    
    attributes$missingValueCodeExplanation <- df_attributes$missingValueCodeExplanation
    
    # Set attribute number type, then minimumm and maximum values

    is_numeric <- which(attributes$columnClasses == "numeric")
    attributes$minimum <- as.numeric(attributes$minimum)
    attributes$maximum <- as.numeric(attributes$maximum)
    
    for (j in 1:length(is_numeric)){
      
      raw <- df_table[ ,is_numeric[j]]
      
      
      if (attributes$missingValueCode[is_numeric[j]] != ""){
        useI <- raw == attributes$missingValueCode[is_numeric[j]]
        raw <- as.numeric(raw[!useI])
      }
      
      
      rounded <- floor(raw)
      if (length(raw) - sum(raw == rounded, na.rm = T) > 0){
        attributes$numberType[is_numeric[j]] <- "real"
      } else if (min(raw, na.rm = T) > 0){
        attributes$numberType[is_numeric[j]] <- "natural"
      } else if (min(raw, na.rm = T) < 0){
        attributes$numberType[is_numeric[j]] <- "integer"
      } else {
        attributes$numberType[is_numeric[j]] <- "whole"
      }
      
      
      attributes$minimum[is_numeric[j]] <- round(min(raw,
                                                     na.rm = TRUE),
                                                 digits = 2)
      
      attributes$maximum[is_numeric[j]] <- round(max(raw,
                                                       na.rm = TRUE),
                                                   digits = 2)

    }
    
    is_character <- which(attributes$columnClasses == "character") 
    is_catvar <- which(attributes$columnClasses == "categorical")
    use_i <- c(is_character, is_catvar)
    
    attributes$numberType[use_i] <- "character"
    
    attributes$columnClasses[is_catvar] <- "factor"

    # Set attribute definition (i.e. "definition")
    
    use_i <- c(is_character, is_catvar)
    
    if (length(use_i) > 0){
      attributes$definition[use_i] <- attributes$attributeDefinition[use_i]
    }
    
    attributes_stored[[i]] <- attributes

  }
  
  list("attributes" = attributes_stored)

}
