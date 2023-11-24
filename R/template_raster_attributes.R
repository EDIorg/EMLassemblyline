#' Describe raster files
#'
#' @description Describes variables and dimensions of a raster file
#'  (classes, units, datetime formats, missing value codes).
#'  Only GeoTiff files are currently supported.
#'
#' @param path
#'     (character) Path to the metadata template directory.
#' @param data.path
#'     (character) Path to the data directory.
#' @param raster.file
#'     (character) File name. If more than one, then supply as a
#'     vector of character strings 
#'     (e.g. \code{raster.file = c('change.tif', 'mean_votes.tif')}).
#' @param write.file
#'     (logical; optional) Whether to write the template file. Default is \code{TRUE}.
#'
#' @return
#' \item{attributes_*}{Columns:
#'     \itemize{
#'     \item{attributeName: Column name}
#'     \item{attributeDefinition: Column definition}
#'     \item{class: Column class. Valid options are: "numeric" (Numeric variable), "categorical" (Categorical variable, i.e. nominal), "character" (Free text character strings, e.g. notes), "Date" (Date and time variable)}
#'     \item{unit: Column unit. Required for numeric classes. Select from EML's standard unit dictionary, accessible with \code{view_unit_dictionary()}. Use values in the "id" column. If not found, then define as a custom unit (see custom_units.txt).}
#'     \item{dateTimeFormatString: Format string. Required for Date classes. Valid format string components are: "Y" (Year), "M" (Month), "D" (Day), "h" (Hour), "m" (Minute), "s" (Second), Common separators of format string components (e.g. "-" "/" "\" ":"") are supported.}
#'     \item{missingValueCode: Missing value code. Required for columns containing a missing value code).}
#'     \item{missingValueCodeExplanation: Definition of missing value code.}
#'     }
#' }
#' \item{custom_units}{Describes non-standard units used in a vector attribute template. Columns:
#'     \itemize{
#'     \item{id: Unit name listed in the unit column of the vector attributes template (e.g. feetPerSecond)}
#'     \item{unitType: Unit type (e.g. velocity)}
#'     \item{parentSI: SI equivalent (e.g. metersPerSecond)}
#'     \item{multiplierToSI: Multiplier to SI equivalent (e.g. 0.3048)}
#'     \item{Abbreviation: Abbreviation (e.g. ft/s)}
#'     }
#' }
#'
#' @details
#'     Character encoding of metadata extracted directly from the tables are
#'     converted to UTF-8 via \code{enc2utf8()}.
#'
#' @examples
#' \dontrun{
#' # Set working directory
#' setwd("/Users/me/Documents/data_packages/pkg_260")
#' 
#' template_raster_attributes(
#'   path = './metadata_templates',
#'   data.path = './data_objects',
#'   raster.file = 'geotiff_test_file.tif')
#' }
#'     
#' @export
#'
template_raster_attributes <- function(
    path = NULL,
    data.path = path,
    raster.file = NULL,
    write.file = TRUE) {

  message("Templating raster attributes ...")

  # Validate arguments --------------------------------------------------------

  # Validate path usage before passing arguments to validate_arguments()
  if (missing(path)){
    stop('Input argument "path" is missing.')
  }

  # Pass remaining arguments to validate_arguments().

  validate_arguments(
    fun.name = 'template_raster_attributes',
    fun.args = as.list(environment()))

  # Read metadata templates and data ------------------------------------------

  # If files are absents

  if (is.null(raster.file)){

    stop('Input argument "raster.file" is missing.')

  } else if (!is.null(raster.file)){

    # Validate raster.file

    raster.file <- suppressWarnings(
      validate_file_names(
        path = data.path,
        data.files = raster.file
      )
    )

  }

  # Import attributes_*.txt ---------------------------------------------------

  # Extract attributes of each data file

  attributes <- list()

  for (i in 1:length(raster.file)){

    if (mime::guess_type(raster.file[i]) == "image/tiff") {
      # Do not template geotiff to prevent for R crash due to too many cells to 
      # read when converting raster in dataframe 
      
      attributes[[i]] <- data.frame(
        attributeName = character(0),
        attributeDefinition = character(0),
        class = character(0),
        unit = character(0),
        dateTimeFormatString = character(0),
        missingValueCode = character(0),
        missingValueCodeExplanation = character(0),
        stringsAsFactors = FALSE
      )
      
    } else {
      
      # Read file
  
      rasterFile <- terra::rast(paste(data.path, raster.file[i], sep = "/"))
  
      # Convert in dataframe to read data
      
      rasterData <- data.frame(rasterFile)
  
      # Initialize attribute table
  
      rows <- ncol(rasterData)
  
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
  
      attributes[[i]]$attributeName <- colnames(rasterData)
  
      # Guess character and numeric classes
  
      guess <- unname(
        unlist(
          lapply(
            rasterData,
            function(k) {
              # An exception for the two component "IDate Date" class created
              # by data.table::fread(). Returning both components results in
              # a class vector that is longer than the column vector.
              if (any(c("POSIXct", "POSIXt", "Date") %in% class(k))) {
                return("Date")
              } else {
                # 'character' has no consequence on other steps.
                # Then, makes empty cols be recognized as it.
                if (length(unique(k)) == 1 && (all(is.na(k)) || all(k == ""))) {
                  return("empty")
                }
                return(class(k))
              }
            })))
  
      guess_map <- c(
        character = "character",
        logical = "character",
        factor = "character",
        integer = "numeric",
        integer64 = "numeric",
        numeric = "numeric",
        Date = "Date",
        empty = "empty")
  
      guess <- unname(guess_map[guess])
  
      # Guess Date class
  
      use_i <- guess == "character"
  
      if (sum(use_i) > 0) {
        potential_date_cols <- colnames(rasterData)[use_i]
        potential_date_i <- stringr::str_detect(tolower(potential_date_cols), "date|time|day")
        guess_datetime <- potential_date_cols[potential_date_i]
        use_i <- match(guess_datetime, attributes[[i]]$attributeName)
        guess[use_i] <- "Date"
      }
  
      use_i <- guess == "numeric"
  
      if (sum(use_i) > 0){
        potential_date_cols <- colnames(rasterData)[use_i]
        potential_date_i <- stringr::str_detect(tolower(potential_date_cols), "date|time|day|year")
        guess_datetime <- potential_date_cols[potential_date_i]
        use_i <- match(guess_datetime, attributes[[i]]$attributeName)
        guess[use_i] <- "Date"
      }
  
      # Guess factor class
  
      use_i <- guess == "character"
  
      if (sum(use_i) > 0){
        potential_fact_cols <- colnames(rasterData)[use_i]
        use_i2 <- match(potential_fact_cols, colnames(rasterData))
        if (length(use_i2) == 1) {
          unique_lengths <- length(unique(rasterData[ ,use_i2]))
        } else {
          unique_lengths <- apply(rasterData[ ,use_i2], 2, function(x)length(unique(x)))
        }
  
        ### OLD ###
        # potential_facts <- unique_lengths <= dim(x$data.table[[i]]$content)[1]*0.3
        ### NEW ###
        # Avoid magical '0.3' by selecting an empirical but logical value
        # this value is set as the most repeated character string
        potential_facts <- unique_lengths < sapply(
          rasterData[,names(unique_lengths)],
          function(col)
            max(table(col))
        )
        ###
        if (sum(potential_facts) > 0){
          potential_facts <- names(potential_facts[potential_facts == TRUE])
          use_i <- match(potential_facts, attributes[[i]]$attributeName)
          guess[use_i] <- "categorical"
        }
      }
  
      # Replace empty col class by "character"
  
      guess <- replace(guess, list = which(guess == "empty"), "character")
  
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
  
      # Encode extracted metadata in UTF-8
  
      attributes[[i]]$attributeName <- enc2utf8(attributes[[i]]$attributeName)
        
    }
    # Write template to file

    # If writing to file ...

    if (isTRUE(write.file)){

      # TODO: check for other raster file type (not only .tif)
      if (mime::guess_type(raster.file[i]) == "image/tiff"){
        filename <- gsub(".tif$|.tiff$", "", raster.file[i])
      }
      
      value <- file.exists(
        paste0(
          path,
          "/",
          "attributes_",
          filename,
          ".txt"
        )
      )

      if (!isTRUE(value)){

        message(
          paste0(
            "attributes_",
            filename,
            ".txt."
          )
        )

        utils::write.table(
          attributes[[i]],
          paste0(
            path,
            "/",
            "attributes_",
            enc2utf8(
              filename),
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
            filename,
            ".txt already exists!"
          )
        )

      }

    }

  }

  # Import custom_units.txt ---------------------------------------------------

  # If writing to file ...

  if (isTRUE(write.file)){

    # Write to path

    value <- file.copy(
      from = system.file(
        '/templates/custom_units.txt',
        package = 'EMLassemblyline'
      ),
      to = paste0(
        path,
        "/custom_units.txt"
      )
    )

    # Send message

    if (isTRUE(value)){
      message("custom_units.txt")
    } else {
      message("custom_units.txt already exists!")
    }

  }

  # Return --------------------------------------------------------------------

  if (!isTRUE(write.file)){
    message('No templates were written to file (write.file = FALSE).')
  }

  message("Done.")

}
