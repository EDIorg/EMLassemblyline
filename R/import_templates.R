#' Import metadata templates
#'
#' @description  
#'     Create a working directory for your dataset then run this function to 
#'     import metadata templates. Use these templates to provide information 
#'     about your data.
#'
#' @usage 
#'     import_templates(path, license, data.names)
#'
#' @param path 
#'     A character string specifying a path to the dataset working directory
#'     (e.g. "C:/Users/Colin/Documents/data_sets/gleon_chloride").
#' @param license
#'     A license for your dataset. Select one of 3 options (see details below).
#'
#' @return 
#'     \emph{datasetname_abstract.txt} A text file for the abstract of your 
#'     dataset. This file is UTF-8 formatted.
#'     
#'     \emph{datasetname_additional_info.txt} A text file for additional 
#'     information about your dataset. This file is UTF-8 formatted.
#'     
#'     \emph{datasetname_custom_units.txt} A tab delimited table for custom 
#'     units used in your data that are not defined in the standard unit 
#'     dictionary. This file is UTF-8 formatted.
#'     
#'     \emph{datasetname_cc_by_4.0_intellectual_rights.txt} One of two 
#'     intellectual rights licenses to consider for your dataset. Do not edit 
#'     the text of this file. This file is UTF-8 formatted.
#'     
#'     \emph{datasetname_cc0_1_intellectual_rights.txt} One of two intellectual 
#'     rights licenses to consider for your dataset. Do not edit the text of 
#'     this file. This file is UTF-8 formatted.
#'     
#'     \emph{datasetname_keywords.txt} A tab delimited table for keywords. This 
#'     file is UTF-8 formatted.
#'     
#'     \emph{datasetname_methods.txt} A text file for methods used in creating 
#'     your data. This file is UTF-8 formatted.
#'     
#'     \emph{datasetname_personnel.txt} A tab delimited table for information 
#'     about personnel associated with this dataset. This file is UTF-8 
#'     formatted.
#'     
#'     \emph{datasetname_datatablename_attributes.txt} A tab delimited table 
#'     for information about your data tables. This file is UTF-8 formatted.
#'     
#'     \emph{eml_configuration.R} A file for technical and general information 
#'     about your data.
#'     
#' @details 
#'     If template files already exist in the working directory, new templates 
#'     will not be imported.
#'     
#' @export     
#'     
#' @seealso 
#'     \code{\link{view_instructions}} for guidance on completing the 
#'     templates.


import_templates <- function(path, license, data.names){
  
  # Check for arguments -------------------------------------------------------
  
  if (missing(path)){
    stop("Specify path to your dataset working directory.")
  }
  if (missing(license)){
    stop("Specify a license for your dataset.")
  }
  if (missing(data.names)){
    stop("Specify the names of all the data entities in your dataset.")
  }

  # Check arguments and modify ------------------------------------------------
  
  # Convert arguments to lower case
  
  license.low <- tolower(license)
  
  data.types.low <- tolower(data.types)
  
  # License is a valid option
  
  if (!str_detect(license.low, "cc0|ccby|other")){
    stop('Invalid license. Please choose "CC0", "CCBY", or "other".')
  }
  
  # Data names are valid
  
  files <- list.files(path)
  files <- c(files, str_replace(files, "\\.[:alnum:]*$", replacement = ""))
  use_i <- str_detect(string = files,
                      pattern = str_c("^", data.names, "$", collapse = "|"))
  if (!sum(use_i) == length(data.names)){
    if(sum(use_i) == 0){
      stop(paste("Invalid file name(s) entered: ", paste(data.names, collapse = ", "), sep = ""))
    } else {
      name_issues <- data.names[!files[use_i] == data.names]
      stop(paste("Invalid data.names entered: ", paste(name_issues, collapse = ", "), sep = ""))
    }
  }
  
  # Copy non-attributes templates to working directory with correct names -----
  
  # Abstract
  
  value <- file.copy(from = paste(path.package("EMLassemblyline"),
                                  "/templates/abstract.txt",
                                  sep = ""),
                     to = paste(path,
                                "/",
                                "abstract.txt",
                                sep = ""))
  
  if (isTRUE(value)){
    print("Importing abstract.txt ...")
  } else {
    print("abstract.txt already exists ...")
  }
  
  # Additional info
  
  value <- file.copy(from = paste(path.package("EMLassemblyline"),
                                  "/templates/additional_info.txt",
                                  sep = ""),
                     to = paste(path,
                                "/",
                                "additional_info.txt",
                                sep = ""))
  if (isTRUE(value)){
    print("Importing additional_info.txt", " ...")
  } else {
    print("additional_info.txt already exists ...")
  }
  
  # Configuration
  
  value <- file.copy(from = paste(path.package("EMLassemblyline"),
                                  "/templates/configuration.R",
                                  sep = ""),
                     to = paste(path,
                                "/",
                                "configuration.R",
                                sep = ""))
  
  if (isTRUE(value)){
    print("Importing configuration.R ...")
  } else {
    print("configuration.R already exists ...")
  }
  
  # Custom units
  
  value <- file.copy(from = paste(path.package("EMLassemblyline"),
                                  "/templates/custom_units.txt",
                                  sep = ""),
                     to = paste(path,
                                "/",
                                "custom_units.txt",
                                sep = ""))
  
  if (isTRUE(value)){
    print("Importing custom_units.txt ...")
  } else {
    print("custom_units.txt already exists ...")
  }
  
  # Instructions
  
  value <- file.copy(from = paste(path.package("EMLassemblyline"),
                                  "/templates/instructions.pdf",
                                  sep = ""),
                     to = paste(path,
                                "/",
                                "instructions.pdf",
                                sep = ""))
  
  if (isTRUE(value)){
    print("Importing instructions.pdf ...")
  } else {
    print("Instructions.pdf already exists ...")
  }
  
  # Intellectual rights license
  
  if (license.low == "cc0"){
    value <- file.copy(from = paste(path.package("EMLassemblyline"),
                                    "/templates/intellectual_rights_cc0.txt",
                                    sep = ""),
                       to = paste(path,
                                  "/",
                                  "intellectual_rights.txt",
                                  sep = ""))
    if (isTRUE(value)){
      print("Importing intellectual_rights.txt ...")
    } else {
      print("intellectual_rights.txt already exists ...")
    }
  } else if (license.low == "ccby"){
    value <- file.copy(from = paste(path.package("EMLassemblyline"),
                                    "/templates/intellectual_rights_ccby4.0.txt",
                                    sep = ""),
                       to = paste(path,
                                  "/",
                                  "intellectual_rights.txt",
                                  sep = ""))
    if (isTRUE(value)){
      print("Importing intellectual_rights.txt ...")
    } else {
      print("intellectual_rights.txt already exists ...")
    }
  } else if (license.low == "other"){
    value <- file.copy(from = paste(path.package("EMLassemblyline"),
                                    "/templates/intellectual_rights_other.txt",
                                    sep = ""),
                       to = paste(path,
                                  "/",
                                  "intellectual_rights.txt",
                                  sep = ""))
    if (isTRUE(value)){
      print("Importing intellectual_rights.txt ...")
    } else {
      print("intellectual_rights.txt already exists ...")
    }
  }
  
  # Keywords

  value <- file.copy(from = paste(path.package("EMLassemblyline"),
                                  "/templates/keywords.txt",
                                  sep = ""),
                     to = paste(path,
                                "/",
                                "keywords.txt",
                                sep = ""))
  
  if (isTRUE(value)){
    print("Importing keywords.txt ...")
  } else {
    print("keywords.txt already exists ...")
  }
  
  # Methods
  
  value <- file.copy(from = paste(path.package("EMLassemblyline"),
                                  "/templates/methods.txt",
                                  sep = ""),
                     to = paste(path,
                                "/",
                                "methods.txt",
                                sep = ""))
  
  if (isTRUE(value)){
    print("Importing methods.txt ...")
  } else {
    print("methods.txt already exists ...")
  }
  
  # My workflow script
  
  value <- file.copy(from = paste(path.package("EMLassemblyline"),
                                  "/templates/my_workflow.R",
                                  sep = ""),
                     to = paste(path,
                                "/",
                                "my_workflow.R",
                                sep = ""))
  
  if (isTRUE(value)){
    print("Importing my_workflow.R ...")
  } else {
    print("my_workflow.R already exists ...")
  }
  
  # Personnel
  
  value <- file.copy(from = paste(path.package("EMLassemblyline"),
                                  "/templates/personnel.txt",
                                  sep = ""),
                     to = paste(path,
                                "/",
                                "personnel.txt",
                                sep = ""))
  
  if (isTRUE(value)){
    print("Importing personnel.txt ...")
  } else {
    print("personnel.txt already exists ...")
  }
  
  # Copy attributes files to working directory --------------------------------

  # Check for valid column names
  
  # Extract attributes
  
  
  attributes <- list()
  for (i in 1:length(data.names)){
    
    # Read data table
    
    file_path <- paste(path,
                       "/",
                       data.names[i],
                       sep = "")
    
    delim_guess <- get.delim(file_path,
                             n = 1)

    df_table <- read.table(file_path,
                           header = TRUE,
                           sep = delim_guess,
                           quote = "\"",
                           as.is = TRUE,
                           comment.char = "")
    
    # Initialize attribute table
    
    rows <- ncol(df_table)
    attributes[[i]] <- data.frame(attributeName = character(rows),
                             attributeDefinition = character(rows),
                             class = character(rows),
                             unit = character(rows),
                             dateTimeFormatString = character(rows),
                             missingValueCode = character(rows),
                             missingValueCodeExplanation = character(rows),
                             stringsAsFactors = FALSE)

    # Get names
    
    attributes[[i]]$attributeName <- colnames(df_table)
    
    # Guess character and numeric classes
    
    guess <- unname(unlist(lapply(df_table, class)))
    guess_map <- c(character = "character", 
                   logical = "numeric", 
                   factor = "character",
                   integer = "numeric",
                   numeric = "numeric")
    guess <- unname(guess_map[guess])
    
    # Guess Date class
    
    use_i <- guess == "character"
    if (sum(potential) > 0){
      potential_date_cols <- colnames(df_table)[use_i]
      potential_date_i <- str_detect(tolower(potential_date_cols), "date|time")
      guess_datetime <- potential_date_cols[potential_date_i]
      use_i <- match(guess_datetime, attributes[[i]]$attributeName)
      guess[use_i] <- "Date"
    }
    
    # Guess factor class
    
    use_i <- guess == "character"
    if (sum(potential) > 0){
      potential_fact_cols <- colnames(df_table)[use_i]
      use_i2 <- match(potential_fact_cols, colnames(df_table))
      unique_lengths <- apply(df_table[ ,use_i2], 2, function(x)length(unique(x)))
      potential_facts <- unique_lengths <= 30
      if (sum(potential_facts) > 0){
        potential_facts <- names(potential_facts[potential_facts == TRUE])
        use_i <- match(potential_facts, attributes[[i]]$attributeName)
        guess[use_i] <- "factor"
      }
    }
    
    # Update attributes class
    
    attributes[[i]]$class <- guess
    
    # Add unit for numeric data
    
    use_i <- attributes[[i]]$class == "numeric"
    if (sum(use_i) > 0){
      attributes[[i]]$unit[use_i] <- "Add units"
    }
    
    # Add date time format strings for Date data
    
    use_i <- attributes[[i]]$class == "Date"
    if (sum(use_i) > 0){
      attributes[[i]]$dateTimeFormatString[use_i] <- "Add datetime specifier"
    }
    
    # Write table to file
    
    value <- file.exists(paste(path,
                                 "/",
                                 "attributes_",
                                 substr(data.names[i], 1, nchar(data.names[i]) - 4),
                                 ".txt",
                                 sep = ""))
    if (!isTRUE(value)){
      
      print(paste("Importing attributes_",
                  substr(data.names[i], 1, nchar(data.names[i]) - 4),
                  ".txt ...",
                  sep = ""))
      
      write.table(attributes[[i]],
                  paste(path,
                        "/",
                        "attributes_",
                        substr(data.names[i], 1, nchar(data.names[i]) - 4),
                        ".txt",
                        sep = ""),
                  sep = "\t",
                  row.names = F,
                  quote = F,
                  fileEncoding = "UTF-8")
      
    } else {
      print(paste("attributes_",
            substr(data.names[i], 1, nchar(data.names[i]) - 4),
            ".txt already exists ...",
            sep = ""))
    }

  }

}
