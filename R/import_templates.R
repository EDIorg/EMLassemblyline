#' Import metadata templates
#'
#' @description  
#'     Run this function to import metadata templates for your dataset. 
#'     Information entered in these templates will be rendered into EML by 
#'     \code{make_eml} during the last step of the assembly line process.
#'
#' @usage 
#'     import_templates(path = "", license = "", data.files = c("data.file.1", "data.file.2", "etc."))
#'
#' @param path 
#'     A character string specifying a path to the dataset working directory
#'     (e.g. "C:/Users/Colin/Documents/data_sets/gleon_chloride").
#' @param license
#'     A character string specifying the license for your dataset. Select "CC0" 
#'     or "CCBY". Additional information about these licenses are listed below 
#'     under "details".
#' @param data.files
#'     A list of character strings specifying the names of the data files
#'     of your dataset. It is not necessary to include the file extension.
#'
#' @return 
#'     \strong{abstract.txt} A text file for the abstract of your dataset. Edit 
#'     this file in a text editor. Do not include special characters, symbols, 
#'     or formatting. Keep it simple! Describe your methods in plain text. 
#'     Remove any smart quotes or other symbols specific to Microsoft Office.
#'     
#'     \strong{additional_info.txt} A text file for additional information about 
#'     your dataset.
#'     
#'     \strong{attributes_datafilename.txt} A tab delimited table for information 
#'     about your data tables. Edit this file in a spread sheet editor, DO NOT 
#'     edit in a text editor. NOTE: each of your data files will have a 
#'     corresponding attributes.txt file. \code{import_templates} makes a 
#'     series of informed guesses about the attributes of your data files and 
#'     writes them in these files. You must verify this information is correct.
#'     
#'     \strong{configuration.R} A file for supplying additional parameters to 
#'     functions used in the assembly line. Edit this file in RStudio.
#'     
#'     \strong{custom_units.txt} A tab delimited table for custom units used in 
#'     your data that are not defined in the standard unit dictionary. Edit 
#'     this file in a spread sheet editor, DO NOT edit in a text editor.
#'     
#'     \strong{instructions.pdf} Step-by-step instructions on how to operate the 
#'     assembly line.
#'     
#'     \strong{intellectual_rights.txt} The selected intellectual rights license 
#'     for your dataset. DO NOT edit the text of this file.
#'     
#'     \strong{keywords.txt} A tab delimited table for keywords. Edit this file 
#'     in a spread sheet editor, DO NOT edit in a text editor.
#'     
#'     \strong{methods.txt} A text file for the methods followed in 
#'     collecting/creating your dataset. Edit this file in a text editor. Do 
#'     not include special characters, symbols, or formatting. Keep it simple! 
#'     Describe your methods in plain text. Remove any smart quotes or other 
#'     symbols specific to Microsoft Office.
#'     
#'     \strong{my_workflow.R} A blank R script for you to build an assembly 
#'     line workflow, which can be revisited or modified for future assembly 
#'     line runs.
#'     
#'     \strong{personnel.txt} A tab delimited table for information about 
#'     personnel associated with your dataset. Edit this file in a spread sheet 
#'     editor, DO NOT edit in a text editor.
#'     
#' @details 
#'     New templates will not be imported if template files already exist in 
#'     the directory.
#'     
#'     Here is the text of the recommended intellectual rights licenses.
#'     \itemize{
#'         \item \strong{CCO} This data package is released to the “public 
#'         domain” under Creative Commons CC0 1.0 “No Rights Reserved” (see: 
#'         https://creativecommons.org/publicdomain/zero/1.0/). It is 
#'         considered professional etiquette to provide attribution of the 
#'         original work if this data package is shared in whole or by 
#'         individual components. A generic citation is provided for this data 
#'         package on the website https://portal.edirepository.org (herein 
#'         “website”) in the summary metadata page. Communication (and 
#'         collaboration) with the creators of this data package is recommended 
#'         to prevent duplicate research or publication. This data package (and 
#'         its components) is made available “as is” and with no warranty of 
#'         accuracy or fitness for use. The creators of this data package and 
#'         the website shall not be liable for any damages resulting from 
#'         misinterpretation or misuse of the data package or its components. 
#'         Periodic updates of this data package may be available from the 
#'         website. Thank you.
#'         \item \strong{CCBY} This information is released under the Creative 
#'         Commons license - Attribution - CC BY 
#'         (https://creativecommons.org/licenses/by/4.0/). The consumer of 
#'         these data ("Data User" herein) is required to cite it appropriately 
#'         in any publication that results from its use. The Data User should 
#'         realize that these data may be actively used by others for ongoing 
#'         research and that coordination may be necessary to prevent duplicate 
#'         publication. The Data User is urged to contact the authors of these 
#'         data if any questions about methodology or results occur. Where 
#'         appropriate, the Data User is encouraged to consider collaboration 
#'         or co-authorship with the authors. The Data User should realize that 
#'         misinterpretation of data may occur if used out of context of the 
#'         original study. While substantial efforts are made to ensure the 
#'         accuracy of data and associated documentation, complete accuracy of 
#'         data sets cannot be guaranteed. All data are made available "as is." 
#'         The Data User should be aware, however, that data are updated 
#'         periodically and it is the responsibility of the Data User to check 
#'         for new versions of the data. The data authors and the repository 
#'         where these data were obtained shall not be liable for damages 
#'         resulting from any use or misinterpretation of the data. Thank you.
#'     }
#'     
#' @export     
#'     

import_templates <- function(path, license, data.files){
  
  # Check arguments and parameterize ------------------------------------------
  
  message("Checking input arguments.")
  
  if (missing(path)){
    stop('Input argument "path" is missing! Specify the path to your dataset working directory.')
  }
  if (missing(license)){
    stop('Input argument "license" is missing! Specify a license for your dataset.')
  }
  if (missing(data.files)){
    stop('Input argument "data.files" is missing! Specify the names of all the data files in your dataset.')
  }

  # Valdate path
  
  validate_path(path)
  
  # Validate license
  
  license.low <- tolower(license)
  
  if (!str_detect(license.low, "cc0|ccby")){
    stop('Invalid value entered for the "license" argument. Please choose "CC0" or "CCBY".')
  }
  
  # Validate data.files
  
  data_files <- validate_file_names(path, data.files)
  
  # Detect operating system
  
  os <- detect_os()
  
  # Detect file delimeters
  
  delim_guess <- detect_delimeter(path, data.files, os)

  
  # Copy templates to path if they don't exist --------------------------------
  
  # Abstract
  
  value <- file.copy(from = paste(path.package("EMLassemblyline"),
                                  "/templates/abstract.txt",
                                  sep = ""),
                     to = paste(path,
                                "/",
                                "abstract.txt",
                                sep = ""))
  
  if (isTRUE(value)){
    message("Importing abstract.txt.")
  } else {
    message("abstract.txt already exists!")
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
    message("Importing additional_info.txt.")
  } else {
    message("additional_info.txt already exists!")
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
    message("Importing custom_units.txt.")
  } else {
    message("custom_units.txt already exists!")
  }
  
  # Instructions
  
  value <- file.copy(from = paste(path.package("EMLassemblyline"),
                                  "/templates/instructions.html",
                                  sep = ""),
                     to = paste(path,
                                "/",
                                "instructions.html",
                                sep = ""))
  
  if (isTRUE(value)){
    message("Importing instructions.html.")
  } else {
    message("instructions.html already exists!")
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
      message("Importing intellectual_rights.txt.")
    } else {
      message("intellectual_rights.txt already exists!")
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
      message("Importing intellectual_rights.txt.")
    } else {
      message("intellectual_rights.txt already exists!")
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
    message("Importing keywords.txt.")
  } else {
    message("keywords.txt already exists!")
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
    message("Importing methods.txt.")
  } else {
    message("methods.txt already exists!")
  }
  
  # Blank script (my_workflow.R)
  
  value <- file.copy(from = paste(path.package("EMLassemblyline"),
                                  "/templates/my_workflow.R",
                                  sep = ""),
                     to = paste(path,
                                "/",
                                "my_workflow.R",
                                sep = ""))
  
  if (isTRUE(value)){
    message("Importing my_workflow.R.")
  } else {
    message("my_workflow.R already exists!")
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
    message("Importing personnel.txt.")
  } else {
    message("personnel.txt already exists!")
  }
  
  # Copy attributes files to working directory --------------------------------
  
  # Check for valid column names
  
  message("Checking data.files for valid column names.")
  
  for (i in 1:length(data_files)){
    
    data_path <- paste(path,
                          "/",
                          data_files[i],
                          sep = "")
    
    df_table <- read.table(data_path,
                           header = TRUE,
                           sep = delim_guess[i],
                           quote = "\"",
                           as.is = TRUE,
                           comment.char = "")
    
    column_names <- colnames(df_table)
    use_i <- str_detect(string = column_names,
                        pattern = "\\.")
    
    if (sum(use_i) > 0){
      stop(paste("Invalid column names detected in ", 
                 data_files[i],
                 ":  ",
                 paste(column_names[use_i], collapse = ", "), 
                 '  Replace characters located at periods "." in the above listed column names with underscores "_"',
                 sep = "")
           )
    }
  }
  
  # Extract attributes for each data file
  
  attributes <- list()
  for (i in 1:length(data_files)){
    
    message(paste("Detecting attributes of ",
                data_files[i],
                ".",
                sep = ""))
    
    # Read data table
    
    data_path <- paste(path,
                       "/",
                       data_files[i],
                       sep = "")

    df_table <- read.table(data_path,
                           header = TRUE,
                           sep = delim_guess[i],
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
                   logical = "character", 
                   factor = "character",
                   integer = "numeric",
                   numeric = "numeric")
    guess <- unname(guess_map[guess])
    
    # Guess Date class
    
    use_i <- guess == "character"
    if (sum(use_i) > 0){
      potential_date_cols <- colnames(df_table)[use_i]
      potential_date_i <- str_detect(tolower(potential_date_cols), "date|time")
      guess_datetime <- potential_date_cols[potential_date_i]
      use_i <- match(guess_datetime, attributes[[i]]$attributeName)
      guess[use_i] <- "Date"
    }
    
    # Guess factor class
    
    use_i <- guess == "character"
    if (sum(use_i) > 0){
      potential_fact_cols <- colnames(df_table)[use_i]
      use_i2 <- match(potential_fact_cols, colnames(df_table))
      unique_lengths <- apply(df_table[ ,use_i2], 2, function(x)length(unique(x)))
      potential_facts <- unique_lengths <= 30
      if (sum(potential_facts) > 0){
        potential_facts <- names(potential_facts[potential_facts == TRUE])
        use_i <- match(potential_facts, attributes[[i]]$attributeName)
        guess[use_i] <- "categorical"
      }
    }
    
    # Update attributes class
    
    attributes[[i]]$class <- guess
    
    # Add unit for numeric data
    
    use_i <- attributes[[i]]$class == "numeric"
    if (sum(use_i) > 0){
      attributes[[i]]$unit[use_i] <- "!Add units here!"
    }
    
    # Add date time format strings for Date data
    
    use_i <- attributes[[i]]$class == "Date"
    if (sum(use_i) > 0){
      attributes[[i]]$dateTimeFormatString[use_i] <- "!Add datetime specifier here!"
    }
    
    # Write table to file
    
    value <- file.exists(paste(path,
                                 "/",
                                 "attributes_",
                                 substr(data_files[i], 1, nchar(data_files[i]) - 4),
                                 ".txt",
                                 sep = ""))
    if (!isTRUE(value)){
      
      message(paste("Importing attributes_",
                  substr(data_files[i], 1, nchar(data_files[i]) - 4),
                  ".txt.",
                  sep = ""))
      
      write.table(attributes[[i]],
                  paste(path,
                        "/",
                        "attributes_",
                        substr(data_files[i], 1, nchar(data_files[i]) - 4),
                        ".txt",
                        sep = ""),
                  sep = "\t",
                  row.names = F,
                  quote = F,
                  fileEncoding = "UTF-8")
      
    } else {
      message(paste("attributes_",
            substr(data_files[i], 1, nchar(data_files[i]) - 4),
            ".txt already exists!",
            sep = ""))
    }

  }
  
  message("Done.")

}
