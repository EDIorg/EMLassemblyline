# This is a helper function for make_eml.R. 
# It compiles attributes, retrieves minimum and maximum values for numeric data
# and reformats the attributes table.


compile_attributes <- function(path, data.files){

  # Detect users operating system
  
  os <- detect_os()
  
  # Get names of data files with associated attribute files
  
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
  
  # Synchronize ordering of data files and attribute files
  
  table_names <- validate_file_names(path, data.files)
  
  attribute_files_out <- c()
  for (i in 1:length(table_names)){
    attribute_files_out[i] <- paste0('attributes_',
                                     str_c(str_sub(table_names[i], 1, nchar(table_names[i])-4), collapse = "|"),
                                     '.txt')
  }
  fname_table_attributes <- attribute_files_out
  

  # Loop through data tables --------------------------------------------------
  
  attributes_stored <- list()

  for (i in 1:length(table_names)){

    message(paste("Compiling", fname_table_attributes[i]))

    
    file_path <- paste(path,
                       "/",
                       table_names[i],
                       sep = "")
    
    delim_guess <- detect_delimeter(path = path, data.files = table_names[i], os = os)
    
    df_table <- read.table(file_path,
                           header = TRUE,
                           sep = delim_guess,
                           quote = "\"",
                           as.is = TRUE,
                           comment.char = "")
    
    # Read attributes_datatablename
    
    field_count <- count.fields(file = paste0(path,
                                              "/",
                                              fname_table_attributes[i]),
                                              sep = "\t")
    if (sum(field_count > 7) > 0){
      stop(paste0('Some of the information in "',
                 fname_table_attributes[i],
                 '" is not in the correct columns. ',
                 "Please double check the organization of content in this file."))
    }
    
    df_attributes <- read.table(paste(path,
                                      "/",
                                      fname_table_attributes[i],
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
    
    
    
    # Convert user inputs to consistent case
    
    df_attributes$class <- tolower(df_attributes$class)
    
    # Validate attributes -----------------------------------------------------
    
    # Validate attributes: Remaining prompts in units field
    
    df_attributes$unit[is.na(df_attributes$unit)] <- ""
    
    use_i <- str_detect(string = df_attributes$unit,
                        pattern = "^!.*!$")
    
    if (sum(use_i) > 0){
      hold <- df_attributes$attributeName[use_i]
      stop(paste(fname_table_attributes[i], 
                 " contains invalid units. Please edit the units of these attributes: ",
                 paste(hold, collapse = ", ")))
    }
    
    # Validate attributes: Remaining prompts in dateTimeFormatString field
    
    df_attributes$dateTimeFormatString[is.na(df_attributes$dateTimeFormatString)] <- ""
    
    use_i <- str_detect(string = df_attributes$dateTimeFormatString,
                        pattern = "^!.*!$")

    if (sum(use_i) > 0){
      hold <- df_attributes$attributeName[use_i]
      stop(paste(fname_table_attributes[i], 
                 " contains invalid dateTimeFormatString(s). Please edit the dateTimeFormatString(s) of these attributes: ",
                 paste(hold, collapse = ", ")))
    }
    
    # Validate attributes: Each attribute has a definition
    
    use_i <- sum(df_attributes$attributeName != "") ==  sum(df_attributes$attributeDefinition != "")
    
    if (!isTRUE(use_i)){
      hold <- df_attributes$attributeName[df_attributes$attributeDefinition == ""]
      stop(paste(fname_table_attributes[i], 
                 " contains attributes without defintions. Please add definitions to these attributes: ",
                 paste(hold, collapse = ", ")))
    }
    
    # Validate attributes: Each attribute has a class
    
    use_i <- sum(df_attributes$attributeName != "") ==  sum(df_attributes$class != "")
    
    if (!isTRUE(use_i)){
      hold <- df_attributes$attributeName[df_attributes$class == ""]
      stop(paste(fname_table_attributes[i], 
                 " contains attributes without a class. Please add a class to these attributes: ",
                 paste(hold, collapse = ", ")))
    }
    
    # Validate attributes: Each Date class has an entry
    
    use_i <- df_attributes$class == "date"
    use_i2 <- df_attributes$dateTimeFormatString != ""
    use_i3 <- use_i2 == use_i
    
    if (sum(!use_i3) > 0){
      hold <- df_attributes$attributeName[!use_i3]
      stop(paste(fname_table_attributes[i], 
                 " has Date attributes without a dateTimeFormatString. Please add format strings to these attributes: ",
                 paste(hold, collapse = ", ")))
    }
    
    # Validate attributes: class == numeric has non-blank units
    
    use_i <- df_attributes$class == "numeric"
    use_i2 <- df_attributes$unit != ""
    use_i3 <- use_i2 == use_i
    
    if (sum(use_i3) < length(use_i3)){
      hold <- df_attributes$attributeName[!use_i3]
      stop(paste(fname_table_attributes[i], 
                 " has numeric attributes without units. Please add units to these attributes: ",
                 paste(hold, collapse = ", ")))
    }
    
    # Validate attributes: missingValueCodes have missingValueCodeExplanations
    
    use_i <- df_attributes$missingValueCode %in% ""
    use_i2 <- df_attributes$missingValueCodeExplanation == ""
    use_i3 <- use_i2 != use_i
    
    if (sum(is.na(use_i3)) > 0){
      stop(paste(fname_table_attributes[i], 
                 " missingValueCodeExplanation(s) are absent for the missingValueCodes you have entered. Please fix this.",
                 "NA's listed in the missingValueCode column are interpreted as missingValueCodes and are expecting explanation."))
    }
    
    if (sum(use_i3) > 0){
      hold <- df_attributes$attributeName[use_i3]
      stop(paste(fname_table_attributes[i], 
                 " has missingValueCode(s) without missingValueCodeExplanation(s). Please fix this for these attributes: ",
                 paste(hold, collapse = ", ")))
    }
    
    # Validate attributes: missingValueCodeExplanations have non-blank missingValuecodeExplanations
    
    use_i <- df_attributes$missingValueCodeExplanation != ""
    use_i2 <- df_attributes$missingValueCode %in% ""
    use_i3 <- use_i2 == use_i
    
    if (sum(use_i3) > 0){
      hold <- df_attributes$attributeName[use_i3]
      stop(paste(fname_table_attributes[i], 
                 " has blank missingValueCode(s). Blank missing value codes are not allowed. Please fix your data and metadata for these attributes: ",
                 paste(hold, collapse = ", ")))
    }
    
    # Validate attributes: missingValueCodes only have 1 entry per column
    
    vec <- df_attributes$missingValueCode
    vec[is.na(df_attributes$missingValueCode)] <- "NA"
    use_i <- str_count(vec, '[,]|[\\s]') > 0
    if (sum(use_i) > 0){
      hold <- df_attributes$attributeName[use_i]
      stop(paste0(fname_table_attributes[i], 
                 ' has more than one missingValueCode.', 
                 '\nOnly one missingValueCode per attribute is allowed.', 
                 'Please fix your data and metadata for these attributes:\n',
                 paste(hold, collapse = ", ")))
    }
    
    
    # Modify attributes -------------------------------------------------------
    
    # Modify attributes: remove units class != numeric
    
    use_i <- df_attributes$class != "numeric"
    use_i2 <- df_attributes$unit == ""
    use_i3 <- use_i2 != use_i
    
    if (sum(use_i3) > 0){
      df_attributes$unit[use_i3] <- ""
    }
    
    # ------------------------------------------------
    
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
    
    # Remove missing value codes, set attribute number type, then minimumm and 
    # maximum values. Throw an error if non-numeric values are found.

    is_numeric <- which(attributes$columnClasses == "numeric")
    attributes$minimum <- as.numeric(attributes$minimum)
    attributes$maximum <- as.numeric(attributes$maximum)
    
    if (!identical(is_numeric, integer(0))){
      
      for (j in 1:length(is_numeric)){
        
        raw <- df_table[ ,is_numeric[j]]
        
        
        if (attributes$missingValueCode[is_numeric[j]] != ""){
          useI <- raw == attributes$missingValueCode[is_numeric[j]]
          raw <- as.numeric(raw[!useI])
        }
        
        if ((class(raw) == "character") | (class(raw) == "factor")){
          stop(paste0('Characters strings found in the column "',
                      colnames(df_table)[is_numeric[j]],
                      '" of the file "',
                      table_names[i],
                      '". ',
                      'Please remove these non-numeric characters and try again.'))
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
        
        # If all numeric values are NA then set min and max to NA
        
        if (length(raw) == sum(is.na(raw))){
          attributes$minimum[is_numeric[j]] <- NA
          attributes$maximum[is_numeric[j]] <- NA
        } else {
          attributes$minimum[is_numeric[j]] <- round(min(raw,
                                                         na.rm = TRUE),
                                                     digits = 2)
          attributes$maximum[is_numeric[j]] <- round(max(raw,
                                                         na.rm = TRUE),
                                                     digits = 2)
        }
        
      }
      
    }
    
    
    
    is_character <- which(attributes$columnClasses == "character") 
    is_catvar <- which(attributes$columnClasses == "categorical")
    is_date <- which(attributes$columnClasses == "date")
    use_i <- c(is_character, is_catvar)
    
    attributes$numberType[use_i] <- "character"
    
    attributes$columnClasses[is_catvar] <- "factor"
    
    attributes$columnClasses[is_date] <- "Date"

    # Set attribute definition (i.e. "definition")
    
    use_i <- c(is_character, is_catvar)
    
    if (length(use_i) > 0){
      attributes$definition[use_i] <- attributes$attributeDefinition[use_i]
    }
    
    attributes_stored[[i]] <- attributes

  }
  
  list("attributes" = attributes_stored)

}
