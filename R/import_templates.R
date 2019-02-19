#' Import templates for core metadata
#'
#' @description  
#'     The template files contain the content that will be rendered into EML.
#'     They contain a mix of automatically extracted 
#'     content, boiler plate content, and manually entered human content.
#'
#' @usage 
#'     import_templates(path, data.path = path, license, data.files)
#'
#' @param path 
#'     (character) Path to where the templates will be copied.
#' @param data.path
#'     (character) Path to where the data files reside.
#' @param license
#'     (character) The license under which your dataset is publicly released. Use "CC0" 
#'     (\url{https://creativecommons.org/publicdomain/zero/1.0/}), 
#'     or "CCBY" (\url{https://creativecommons.org/licenses/by/4.0/}).
#' @param data.files
#'     (character) A vector of character strings specifying the names of the data files
#'     of your dataset. It is not necessary to include the file extension. E.g 
#'     data.files = c("data.file.1", "data.file.2", "etc.").
#'
#' @return 
#'     \itemize{
#'         \item{abstract.txt} A file for the abstract of your dataset.
#'         \item{additional_info.txt} A file for information about 
#'         your dataset, that doesn't fit in anywhere else.
#'         \item{attributes_*.txt} A tab delimited table for information 
#'         about your data tables. NOTE: each of your data files will have a 
#'         corresponding attributes.txt file. `import_templates` makes a 
#'         series of informed guesses about the attributes of your data files and 
#'         writes them in these files. A human needs to verify the content.
#'         \item{bounding_boxes.txt} A tab delimited table for geographic bounding boxes of the
#'         dataset.
#'         \item{custom_units.txt} A tab delimited table for non-standard units used in 
#'         your dataset.
#'         \item{intellectual_rights.txt} Contains the intellectual rights license.
#'         \item{keywords.txt} A tab delimited table for keywords.
#'         \item{methods.txt} A text file for the dataset methods
#'         \item{personnel.txt} A tab delimited table for information about 
#'         personnel associated with the dataset.
#'     }
#'     
#' @details 
#'     New templates will not be imported if template files already exist in 
#'     the directory.
#'
#' @examples
#'     # Create a directory structure for an example dataset
#'     
#'     # Copy example data tables to /dataset/data
#'     
#'     # Import templates to /dataset/metadata_templates
#'     
#' @export     
#'     

import_templates <- function(path, data.path = path, license, data.files){
  
  message('Importing metadata templates')
  
  # Validate arguments and parameterize ---------------------------------------
  
  if (missing(path)){
    stop('Input argument "path" is missing! Specify the path to your dataset working directory.')
  } else if (missing(license)){
    stop('Input argument "license" is missing! Specify a license for your dataset.')
  } else if (missing(data.files)){
    stop('Input argument "data.files" is missing! Specify the names of all the data files in your dataset.')
  }
  
  license.low <- tolower(license)
  if (!stringr::str_detect(license.low, "^cc0$|^ccby$")){
    stop('Invalid value entered for the "license" argument. Please choose "CC0" or "CCBY".')
  }

  data_files <- EDIutils::validate_file_names(
    data.path, 
    data.files
  )
  
  EDIutils::validate_fields(
    data.path, 
    data.files = data_files
  )
  
  delim_guess <- EDIutils::detect_delimeter(
    data.path, 
    data.files = data_files, 
    EDIutils::detect_os()
  )

  # Import abstract.txt -------------------------------------------------------
  
  value <- file.copy(
    from = paste0(
      path.package("EMLassemblyline"),
      "/templates/abstract.txt"
    ),
    to = paste0(
      path,
      "/abstract.txt"
    )
  )
  
  if (isTRUE(value)){
    message("Importing abstract.txt.")
  } else {
    message("abstract.txt already exists!")
  }
  
  # Import additional_info.txt ------------------------------------------------
  
  value <- file.copy(
    from = paste0(
      path.package("EMLassemblyline"),
      "/templates/additional_info.txt"
    ),
    to = paste0(
      path,
      "/additional_info.txt"
    )
  )
  
  if (isTRUE(value)){
    message("Importing additional_info.txt.")
  } else {
    message("additional_info.txt already exists!")
  }
  
  # Import bounding_boxes.txt -------------------------------------------------
  
  value <- file.copy(
    from = paste0(
      path.package("EMLassemblyline"),
      "/templates/bounding_boxes.txt"
    ),
    to = paste0(
      path,
      "/bounding_boxes.txt"
    )
  )
  
  if (isTRUE(value)){
    message("Importing bounding_boxes.txt.")
  } else {
    message("bounding_boxes.txt already exists!")
  }
  
  # Import custom_units.txt ---------------------------------------------------
  
  value <- file.copy(
    from = paste0(
      path.package("EMLassemblyline"),
      "/templates/custom_units.txt"
    ),
    to = paste0(
      path,
      "/custom_units.txt"
    )
  )
  
  if (isTRUE(value)){
    message("Importing custom_units.txt.")
  } else {
    message("custom_units.txt already exists!")
  }

  # Import intellectual_rights.txt --------------------------------------------
  
  if (license.low == "cc0"){
    
    value <- file.copy(
      from = paste0(
        path.package("EMLassemblyline"),
        "/templates/intellectual_rights_cc0.txt"
      ),
      to = paste0(
        path,
        "/intellectual_rights.txt"
      )
    )
    
    if (isTRUE(value)){
      message("Importing intellectual_rights.txt.")
    } else {
      message("intellectual_rights.txt already exists!")
    }
    
  } else if (license.low == "ccby"){
    
    value <- file.copy(
      from = paste0(
        path.package("EMLassemblyline"),
        "/templates/intellectual_rights_ccby4.0.txt"
      ),
      to = paste0(
        path,
        "/intellectual_rights.txt"
      )
    )
    
    if (isTRUE(value)){
      message("Importing intellectual_rights.txt.")
    } else {
      message("intellectual_rights.txt already exists!")
    }
    
  }
  
  # Import keywords.txt -------------------------------------------------------

  value <- file.copy(
    from = paste0(
      path.package("EMLassemblyline"),
      "/templates/keywords.txt"
    ),
    to = paste0(
      path,
      "/keywords.txt"
    )
  )
  
  if (isTRUE(value)){
    message("Importing keywords.txt.")
  } else {
    message("keywords.txt already exists!")
  }
  
  # Import methods.txt --------------------------------------------------------
  
  value <- file.copy(
    from = paste0(
      path.package("EMLassemblyline"),
      "/templates/methods.txt"
    ),
    to = paste0(
      path,
      "/methods.txt"
    )
  )
  
  if (isTRUE(value)){
    message("Importing methods.txt.")
  } else {
    message("methods.txt already exists!")
  }
  
  # Import personnel.txt ------------------------------------------------------
  
  value <- file.copy(
    from = paste0(
      path.package("EMLassemblyline"),
      "/templates/personnel.txt"
    ),
    to = paste0(
      path,
      "/personnel.txt"
    )
  )
  
  if (isTRUE(value)){
    message("Importing personnel.txt.")
  } else {
    message("personnel.txt already exists!")
  }
  
  # Import attributes templates -----------------------------------------------
  
  # Check column names
  
  message("Checking data.files for valid column names.")
  
  for (i in 1:length(data_files)){
    
    data_path <- paste0(
      data.path,
      "/",
      data_files[i]
    )
    
    df_table <- utils::read.table(
      data_path,
      header = TRUE,
      sep = delim_guess[i],
      quote = "\"",
      as.is = TRUE,
      comment.char = ""
    )
    
    column_names <- colnames(df_table)
    
    use_i <- stringr::str_detect(
      string = column_names,
      pattern = "\\."
    )
    
    if (sum(use_i) > 0){
      stop(
        paste(
          "Invalid column names detected in ", 
          data_files[i],
          ":  ",
          paste(
            column_names[use_i], 
            collapse = ", "
          ), 
          '  Replace characters located at periods "." in the above listed column names with underscores "_"',
          sep = ""
        )
      )
    }
    
  }
  
  # Extract attributes of each data file
  
  attributes <- list()
  
  for (i in 1:length(data_files)){
    
    message(
      paste(
        "Detecting attributes of ",
        data_files[i],
        ".",
        sep = ""
      )
    )
    
    # Read data table
    
    data_path <- paste0(
      data.path,
      "/",
      data_files[i]
    )

    df_table <- utils::read.table(
      data_path,
      header = TRUE,
      sep = delim_guess[i],
      quote = "\"",
      as.is = TRUE,
      comment.char = "",
      na.strings = c('NA','NULL')
    )
    
    # Initialize attribute table
    
    rows <- ncol(df_table)
    
    attributes[[i]] <- data.frame(
      attributeName = character(rows),
      attributeDefinition = character(rows),
      class = character(rows),
      unit = character(rows),
      dateTimeFormatString = character(rows),
      missingValueCode = character(rows),
      missingValueCodeExplanation = character(rows),
      stringsAsFactors = FALSE
    )

    # Get names
    
    attributes[[i]]$attributeName <- colnames(df_table)
    
    # Guess character and numeric classes
    
    guess <- unname(unlist(lapply(df_table, class)))
    
    guess_map <- c(
      character = "character", 
      logical = "character", 
      factor = "character",
      integer = "numeric",
      numeric = "numeric"
    )
    
    guess <- unname(guess_map[guess])
    
    # Guess Date class
    
    use_i <- guess == "character"
    
    if (sum(use_i) > 0){
      potential_date_cols <- colnames(df_table)[use_i]
      potential_date_i <- stringr::str_detect(tolower(potential_date_cols), "date|time|day")
      guess_datetime <- potential_date_cols[potential_date_i]
      use_i <- match(guess_datetime, attributes[[i]]$attributeName)
      guess[use_i] <- "Date"
    }
    
    # Guess factor class
    
    use_i <- guess == "character"
    if (sum(use_i) > 0){
      potential_fact_cols <- colnames(df_table)[use_i]
      use_i2 <- match(potential_fact_cols, colnames(df_table))
      if (length(use_i2) == 1){
        unique_lengths <- length(unique(df_table[ ,use_i2]))
      } else {
        unique_lengths <- apply(df_table[ ,use_i2], 2, function(x)length(unique(x)))
      }
      potential_facts <- unique_lengths <= dim(df_table)[1]*0.3
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
    
    value <- file.exists(
      paste0(
        path,
        "/",
        "attributes_",
        substr(data_files[i], 1, nchar(data_files[i]) - 4),
        ".txt"
      )
    )
    
    if (!isTRUE(value)){
      
      message(
        paste0(
          "Importing attributes_",
          substr(data_files[i], 1, nchar(data_files[i]) - 4),
          ".txt."
        )
      )
      
      utils::write.table(
        attributes[[i]],
        paste0(
          path,
          "/",
          "attributes_",
          substr(data_files[i], 1, nchar(data_files[i]) - 4),
          ".txt"
        ),
        sep = "\t",
        row.names = F,
        quote = F,
        fileEncoding = "UTF-8"
      )
      
    } else {
      
      message(
        paste0(
          "attributes_",
          substr(data_files[i], 1, nchar(data_files[i]) - 4),
          ".txt already exists!"
        )
      )
      
    }

  }
  
  message("Done.")

}
