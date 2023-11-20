#' Describe NetCDf files
#'
#' @description Describes variables and dimensions of a NetCDF file
#'  (classes, units, datetime formats, missing value codes).
#'
#' @param path 
#'     (character) Path to the metadata template directory.
#' @param data.path
#'     (character) Path to the data directory.
#' @param netcdf
#'     (character) file name. If more than one, then supply as a 
#'     vector of character strings (e.g. 
#'     \code{netcdf = c('netcdf_1.nc', 'netcdf_2.nc')}).
#' @param write.file
#'     (logical; optional) Whether to write the template file.
#'
#' @return 
#' \item{attributes_*}{Columns:
#'     \itemize{
#'     \item{attributeName: variable/dimension name}
#'     \item{attributeDefinition: variable/dimension definition}
#'     \item{class: variable/dimension class. Valid options are: "numeric" (Numeric variable), "categorical" (Categorical variable, i.e. nominal), "character" (Free text character strings, e.g. notes), "Date" (Date and time variable)}
#'     \item{unit: variable/dimension unit. Required for numeric classes. Select from EML's standard unit dictionary, accessible with \code{view_unit_dictionary()}. Use values in the "id" column. If not found, then define as a custom unit (see custom_units.txt).}
#'     \item{dateTimeFormatString: Format string. Required for Date classes. Valid format string components are: "Y" (Year), "M" (Month), "D" (Day), "h" (Hour), "m" (Minute), "s" (Second), Common separators of format string components (e.g. "-" "/" "\" ":"") are supported.}
#'     \item{missingValueCode: Missing value code. Required for variables containing a missing value code).}
#'     \item{missingValueCodeExplanation: Definition of missing value code.}
#'     }
#' }
#' \item{custom_units}{Describes non-standard units used in a NetCDF attribute template. Columns:
#'     \itemize{
#'     \item{id: Unit name listed in the unit column of the table attributes template (e.g. feetPerSecond)}
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
#' template_netcdf_attributes(
#'   path = './metadata_templates',
#'   data.path = './data_objects',
#'   netcdf = 'netcdf_test_file.nc')
#' }
#'     
#' @export     
#'     
template_netcdf_attributes <- function(
    path, 
    data.path = path, 
    netcdf = NULL, 
    write.file = TRUE){
  
  message('Templating NetCDF attributes ...')
  
  # Validate arguments --------------------------------------------------------
  
  # Validate path usage before passing arguments to validate_arguments()
  if (missing(path)){
    stop('Input argument "path" is missing.')
  }
  
  # Pass remaining arguments to validate_arguments().
  
  validate_arguments(
    fun.name = 'template_netcdf_attributes',
    fun.args = as.list(environment()))
  
  # Read metadata templates and data ------------------------------------------
  
  # If files are absent ...
  
  if (is.null(netcdf)){
    
    stop('Input argument "netcdf" is missing.')
    
  } else if (!is.null(netcdf)){
    
    # Validate netcdf
    
    netcdf <- suppressWarnings(
      validate_file_names(
        path = data.path, 
        data.files = netcdf
      )
    )

  }
  
  # Import attributes_*.txt ---------------------------------------------------
  
  # Extract attributes of each data file
  
  attributes <- list()
  
  for (i in 1:length(netcdf)){
    
    # Read NetCDF file
    
    nc <- ncdf4::nc_open(paste(data.path, netcdf[i], sep = "/"))
    
    # Initialize attribute table
    
    rows <- nc$nvars + nc$ndims
    
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
    
    attributes[[i]]$attributeName <- c(attributes(nc$var)$names, attributes(nc$dim)$names)
    
    # Guess classes
    # Because of the class 'array' or 'double' of variables and dimensions, 
    # classes are guessed with the 'is.*()' functions
    
    guess <- unname(
      unlist(
        lapply(
          seq(rows),
          function(k){
            
            # For variables
            
            if (k <= nc$nvars){
              
              # Get values of the current variable
              
              vals_k <- ncdf4::ncvar_get(nc, attributes(nc$var)$names[k])
              
            # For dimensions
              
            } else if (k > nc$nvars){
              
              # Get values of the current dimension
              
              vals_k <- nc$dim[[attributes(nc$dim)$names[k-nc$nvars]]]$vals
              
            }
            
            # Guess class
            
            if (length(unique(vals_k)) == 0 && (all(is.na(vals_k)) || all(vals_k == ""))){
              return("empty")
            } else if (is.numeric(vals_k)){
              return("numeric")
            } else if (is.factor(vals_k)){
              return("categorical")
            } else {
              return("character")
            }
            
          }
        )
      )
    )
    
    # Replace empty col class by "character"
    
    guess <- replace(guess, list = which(guess == "empty"), "character")
    
    # Update attributes class
    
    attributes[[i]]$class <- guess
    
    # Add unit for numeric data
    
    use_i <- attributes[[i]]$class == "numeric"
    
    if (sum(use_i) > 0){
      attributes[[i]]$unit[use_i] <- "!Add units here!"
    }
    
    # Encode extracted metadata in UTF-8
    
    attributes[[i]]$attributeName <- enc2utf8(attributes[[i]]$attributeName)
    
    # Write template to file
    
    # If writing to file ...
    
    if (isTRUE(write.file)){
      
      value <- file.exists(
        paste0(
          path,
          "/",
          "attributes_",
          substr(netcdf[i], 1, nchar(netcdf[i]) - 3),
          ".txt"
        )
      )
      
      if (!isTRUE(value)){
        
        message(
          paste0(
            "attributes_",
            substr(netcdf[i], 1, nchar(netcdf[i]) - 3),
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
              substr(netcdf[i], 1, nchar(netcdf[i]) - 3)),
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
            substr(netcdf[i], 1, nchar(netcdf[i]) - 3),
            ".txt already exists!"
          )
        )
        
      }
      
    }
    
    ncdf4::nc_close(nc)
    
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
