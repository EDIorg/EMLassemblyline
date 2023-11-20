#' Describe vector files
#'
#' @description Describes variables and dimensions of a vector file
#'  (classes, units, datetime formats, missing value codes).
#'  Currently supported files are GeoJSON files and shapefile (not in a .zip archive)
#'
#' @param path
#'     (character) Path to the metadata template directory.
#' @param data.path
#'     (character) Path to the data directory.
#' @param vector.file
#'     (character) File name (or directory name for shapefiles). If more than one, 
#'     then supply as a vector of character strings (e.g.
#'     \code{vector.file = c('shapefile', 'data.GeoJSON')}).
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
#'   raster.file = c('shapefile_test', 'geojson_test_file.GeoJSON'))
#' }
#'     
#' @export
#'
template_vector_attributes <- function(
    path = NULL,
    data.path = path,
    vector.file = NULL,
    write.file = TRUE) {

  message("Templating vector attributes ...")

  # Validate arguments --------------------------------------------------------

  # Validate path usage before passing arguments to validate_arguments()
  if (missing(path)){
    stop('Input argument "path" is missing.')
  }

  # Pass remaining arguments to validate_arguments().

  validate_arguments(
    fun.name = 'template_vector_attributes',
    fun.args = as.list(environment()))

  # Read metadata templates and data ------------------------------------------

  # If files are absents

  if (is.null(vector.file)){

    stop('Input argument "vector.file" is missing.')

  } else if (!is.null(vector.file)){

    # Validate vector.file

    vector.file <- suppressWarnings(
      validate_file_names(
        path = data.path,
        data.files = vector.file
      )
    )

  }

  # Import attributes_*.txt ---------------------------------------------------

  # Extract attributes of each data file

  attributes <- list()

  for (i in 1:length(vector.file)){

    # Read file

    # TODO: Does not read shapefile in .zip ! -> Do not take .zip files ?
    vectorFile <- terra::vect(paste(data.path, vector.file[i], sep = "/"))
    
    vectorData <- data.frame(vectorFile)

    # Initialize attribute table

    rows <- ncol(vectorData)

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

    attributes[[i]]$attributeName <- colnames(vectorData)

    # Guess character and numeric classes

    guess <- unname(
      unlist(
        lapply(
          vectorData,
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
      potential_date_cols <- colnames(vectorData)[use_i]
      potential_date_i <- stringr::str_detect(tolower(potential_date_cols), "date|time|day")
      guess_datetime <- potential_date_cols[potential_date_i]
      use_i <- match(guess_datetime, attributes[[i]]$attributeName)
      guess[use_i] <- "Date"
    }

    use_i <- guess == "numeric"

    if (sum(use_i) > 0){
      potential_date_cols <- colnames(vectorData)[use_i]
      potential_date_i <- stringr::str_detect(tolower(potential_date_cols), "date|time|day|year")
      guess_datetime <- potential_date_cols[potential_date_i]
      use_i <- match(guess_datetime, attributes[[i]]$attributeName)
      guess[use_i] <- "Date"
    }

    # Guess factor class

    use_i <- guess == "character"

    if (sum(use_i) > 0){
      potential_fact_cols <- colnames(vectorData)[use_i]
      use_i2 <- match(potential_fact_cols, colnames(vectorData))
      if (length(use_i2) == 1) {
        unique_lengths <- length(unique(vectorData[ ,use_i2]))
      } else {
        unique_lengths <- apply(vectorData[ ,use_i2], 2, function(x)length(unique(x)))
      }

      ### OLD ###
      # potential_facts <- unique_lengths <= dim(x$data.table[[i]]$content)[1]*0.3
      ### NEW ###
      # Avoid magical '0.3' by selecting an empirical but logical value
      # this value is set as the most repeated character string
      potential_facts <- unique_lengths < sapply(
        vectorData[,names(unique_lengths)],
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

    # Write template to file

    # If writing to file ...

    if (isTRUE(write.file)){
      
      # TODO: check for other vector file type (not only shp and GeoJSON)
      if (is_shp_dir(paste(data.path, vector.file[i], sep = "/"))){
        filename <- tools::file_path_sans_ext(vector.file[i])
      } else if (tolower(tools::file_ext(vector.file[i])) == "geojson") { # GeoJSON not in mime::guess_type function
        filename <- substr(vector.file[i], 1, nchar(vector.file[i]) - 8)
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




#' Is directory a shapefile directory ?
#' 
#' @description Check if the directory contains mandatory shapefiles (.shp, .shx, .dbf)
#' 
#' @param dir_path 
#'    (character) Directory to check.
#'
#' @return (booleen) True if the directory seems to be a shapefile directory, false if not.
#' 
#' @noRd
is_shp_dir <- function(dir_path) {
  if (!file.exists(dir_path)){
    message(paste0(dir_path, ' not found.'))
    return(FALSE)
  }
  if (dir.exists(dir_path)){
    dir <- dir(dir_path)
  } else if (grepl("zip$", dir_path)) {
    dir <- unzip(dir_path, list = TRUE)[, 1]
    # message("Shape file in zip archive is not supported for the moment.
    #         Unzip your file and try again.")
    # return(FALSE)
  } else {
    return(FALSE)
  }
  return(any(grepl("shp/?$", dir)) && any(grepl("shx/?$", dir)) && any(grepl("dbf/?$", dir)))
}
