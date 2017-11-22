#' Import metadata templates
#'
#' @description  
#'     Create a working directory for your dataset then run this function to 
#'     import metadata templates. Use these templates to provide information 
#'     about your data. 
#'
#' @usage 
#'     import_templates(path, license, data.files = c("data.file.1", "data.file.2", "etc"))
#'
#' @param path 
#'     A character string specifying a path to the dataset working directory
#'     (e.g. "C:/Users/Colin/Documents/data_sets/gleon_chloride").
#' @param license
#'     A character string specifying the license for your dataset. Select "CC0",
#'     "CCBY", or "other". Additional information about these licenses are 
#'     listed below in "details".
#' @param data.files
#'     A list of character strings specifying the names of the data files
#'     of your dataset.
#'
#' @return 
#'     \emph{abstract.txt} A text file for the abstract of your 
#'     dataset.
#'     
#'     \emph{additional_info.txt} A text file for additional 
#'     information about your dataset.
#'     
#'     \emph{attributes_datafilename.txt} A tab delimited table for information 
#'     about your data tables. Note: each of your data files will have a 
#'     corresponding attributes.txt file. \code{import_templates} makes a series of 
#'     informed guesses about the attributes of your data files and 
#'     writes them in this file. You will have to verify this information is correct.
#'     
#'     \emph{configuration.R} A file for supplying additional parameters to 
#'     functions used in the assemblyline.
#'     
#'     \emph{custom_units.txt} A tab delimited table for custom 
#'     units used in your data that are not defined in the standard unit 
#'     dictionary.
#'     
#'     \emph{instructions.pdf} Step by step instructions on how to operate the 
#'     assembly line.
#'     
#'     \emph{intellectual_rights.txt} The selected intellectual rights license 
#'     for your dataset.
#'     
#'     \emph{keywords.txt} A tab delimited table for keywords.
#'     
#'     \emph{methods.txt} A text file for methods used in creating your data.
#'     
#'     \emph{my_workflow.R} A blank R script file in which to build your 
#'     assembly line workflow.
#'     
#'     \emph{personnel.txt} A tab delimited table for information about 
#'     personnel associated with this dataset.
#'     
#' @details 
#'     If template files already exist in the working directory, new templates 
#'     will not be imported.
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
#'         \item \strong{other} This option is not recommended! Enter the text
#'         of your custom intellectual rights license.
#'     }
#'     
#' @export     
#'     

import_templates <- function(path, license, data.files){
  
  # Check for arguments -------------------------------------------------------
  
  if (missing(path)){
    stop("Specify path to your dataset working directory.")
  }
  if (missing(license)){
    stop("Specify a license for your dataset.")
  }
  if (missing(data.files)){
    stop("Specify the names of all the data files in your dataset.")
  }

  # Check arguments and modify for script -------------------------------------
  
  print("Checking input arguments ...")
  
  # Convert arguments to lower case
  
  license.low <- tolower(license)
  
  # License is a valid option
  
  if (!str_detect(license.low, "cc0|ccby|other")){
    stop('Invalid license. Please choose "CC0", "CCBY", or "other".')
  }
  
  # Data names are valid
  
  files <- list.files(path)
  files <- c(files, str_replace(files, "\\.[:alnum:]*$", replacement = ""))
  use_i <- str_detect(string = files,
                      pattern = str_c("^", data.files, "$", collapse = "|"))
  if (!sum(use_i) == length(data.files)){
    if(sum(use_i) == 0){
      stop(paste("Invalid data.files entered: ", paste(data.files, collapse = ", "), sep = ""))
    } else {
      name_issues <- data.files[!files[use_i] == data.files]
      stop(paste("Invalid data.files entered: ", paste(name_issues, collapse = ", "), sep = ""))
    }
  }
  
  # Get actual file names for script
  
  files <- list.files(path)
  use_i <- str_detect(string = files,
                    pattern = str_c("^", data.files, collapse = "|"))
  data_files <- files[use_i]
  
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
  
  print("Checking data.files for valid column names ...")
  
  for (i in 1:length(data_files)){
    
    file_path <- paste(path,
                       "/",
                       data_files[i],
                       sep = "")
    
    delim_guess <- get.delim(file_path,
                             n = 2)
    
    df_table <- read.table(file_path,
                           header = TRUE,
                           sep = delim_guess,
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
  
  # Extract attributes
  
  attributes <- list()
  for (i in 1:length(data_files)){
    
    print(paste("Detecting attributes of",
                data_files[i],
                "..."))
    
    # Read data table
    
    file_path <- paste(path,
                       "/",
                       data_files[i],
                       sep = "")
    
    delim_guess <- get.delim(file_path,
                             n = 2)

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
                                 substr(data_files[i], 1, nchar(data_files[i]) - 4),
                                 ".txt",
                                 sep = ""))
    if (!isTRUE(value)){
      
      print(paste("Importing attributes_",
                  substr(data_files[i], 1, nchar(data_files[i]) - 4),
                  ".txt ...",
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
      print(paste("attributes_",
            substr(data_files[i], 1, nchar(data_files[i]) - 4),
            ".txt already exists ...",
            sep = ""))
    }

  }

}
