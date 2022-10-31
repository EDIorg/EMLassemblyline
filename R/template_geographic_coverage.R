#' Describe where the data were collected
#'
#' @description Use this function to extract the unique location coordinates and names from a table or to create a blank template to manually fill.
#'
#' @param path 
#'     (character) Path to the metadata template directory.
#' @param data.path
#'     (character) Path to the data directory.
#' @param data.table 
#'     (character) Table name containing geographic coordinates.
#' @param lat.col 
#'     (character) Column name containing latitude coordinates in decimal 
#'     degrees, where latitudes south of the equator are negative.
#' @param lon.col 
#'     (character) Column name containing longitude coordinates in decimal 
#'     degrees, where longitudes west of the prime meridian are negative.
#' @param site.col
#'     (character) Column name containing site names.
#' @param empty
#'     (logical) Whether to write an empty template file.
#' @param write.file
#'     (logical; optional) Whether to write the template file.
#' @param x
#'     (named list; optional) Alternative input to 
#'     \code{template_geographical_coverage()}. Use \code{template_arguments()} 
#'     to create \code{x}.
#'
#' @return
#' \item{geographical_coverage}{Columns:
#'     \itemize{
#'     \item{geographicDescription: Brief description of location.}
#'     \item{northBoundingCoordinate: North coordinate}
#'     \item{southBoundingCoordinate: South coordinate}
#'     \item{eastBoundingCoordinate: East coordinate}
#'     \item{westBoundingCoordinate: West coordinate}
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
#' # For a table containing site coordinates and names
#' template_geographic_coverage(
#'   path = "./metadata_templates",
#'   data.path = "./data_objects",
#'   data.table = "nitrogen.csv",
#'   site.col = "site_name",
#'   lat.col = "site_lat",
#'   lon.col = "site_lon")
#'   
#' # For returning an empty template to be filled manually
#' template_geographic_coverage(
#'   path = "./metadata_templates",
#'   empty = TRUE)
#' }
#'
#' @export
#'
template_geographic_coverage <- function(
  path, 
  data.path = path, 
  data.table, 
  lat.col, 
  lon.col, 
  site.col, 
  empty = FALSE, 
  write.file = TRUE, 
  x = NULL){
  
  message('Templating geographic coverage ...')
  
  # Validate arguments --------------------------------------------------------
  
  # Validate path usage before passing arguments to validate_arguments()
  # When not using x, inputs are expected from path and data.path. 
  # When using x, only data.path is used. Ignored are path and write.file.
  
  if (is.null(x) & missing(path)){
    stop('Input argument "path" is missing.')
  } else if (!is.null(x) & missing(path)){
    path <- NULL
    data.path <- NULL
  }
  
  # Pass remaining arguments to validate_arguments().
  
  validate_arguments(
    fun.name = 'template_geographic_coverage',
    fun.args = as.list(environment())
  )
  
  # Read data -----------------------------------------------------------------
  
  # If not using x ...
  
  if (is.null(x)){
    
    if (!isTRUE(empty)){
      
      # Validate file name
      
      data_file <- suppressWarnings(
        validate_file_names(
          path = data.path, 
          data.files = data.table
        )
      )
      
      # Read data table
      
      x <- template_arguments(
        data.path = data.path,
        data.table = data_file
      )
      
      x <- x$x
      
      data_read_2_x <- NA_character_
      
    }

    # Does file exist?
    
    f_exists <- file.exists(
      paste0(
        path,
        '/geographic_coverage.txt'
      )
    )
    
    # If using x ...  
    
  } else if (!is.null(x)){
    
    if (!isTRUE(empty)){
      
      # data.table
      
      data_file <- data.table

    }
    
    # write.file
    
    if (isTRUE(write.file)){
      
      stop('Input argument "write.file" is not supported when using "x".')
      
    }
    
    # Does file exist?
    
    f_exists <- is.data.frame(x$template$geographic_coverage.txt$content)
    
  }
  
  # Extract geographic coverage -----------------------------------------------
  
  if (isTRUE(f_exists)){
    
    message("geographic_coverage.txt already exists!")
    
  } else {
    
    message('geographic_coverage.txt')
    
    if (!isTRUE(empty)){
      
      df_table <- x$data.table[[data_file]]$content
      
      # Validate column names
      
      columns <- colnames(df_table)
      columns_in <- c(lat.col, lon.col, site.col)
      use_i <- stringr::str_detect(string = columns,
                                   pattern = stringr::str_c("^", columns_in, "$", collapse = "|"))
      if (sum(use_i) > 0){
        use_i2 <- columns[use_i]
        use_i3 <- columns_in %in% use_i2
        if (sum(use_i) != 3){
          stop(paste("Invalid column names entered: ", paste(columns_in[!use_i3], collapse = ", "), sep = ""))
        }
      }
      
      # Subset table names
      
      df_table <- df_table[ ,c(lat.col, lon.col, site.col)]
      
      # Remove incomplete lines
      
      use_i <- df_table[site.col] == ""
      df_table[use_i, site.col] <- NA
      df_table <- df_table[stats::complete.cases(df_table), ]
      
      # Get vectors of latitude, longitude, and site
      
      latitude <- df_table[lat.col]
      
      longitude <- df_table[lon.col]
      
      site_name <- unique(unlist(df_table[site.col]))
      
      # Output lat and long corresponding to sites
      
      latitude_out = c()
      longitude_out = c() 
      site_out = c()
      
      for (i in 1:length(site_name)){
        
        useI <- site_name[i] == df_table[site.col]
        
        latitude_out[i] <- suppressWarnings(
          as.numeric(latitude[useI][1]))
        
        longitude_out[i] <- suppressWarnings(
          as.numeric(longitude[useI][1]))
        
        site_out[i] <- site_name[i]
        
      }
      
      geocoverage_out <- data.frame(
        geographicDescription = enc2utf8(as.character(site_out)), # Encode extracted metadata in UTF-8
        northBoundingCoordinate = latitude_out,
        southBoundingCoordinate = latitude_out,
        eastBoundingCoordinate = longitude_out,
        westBoundingCoordinate = longitude_out,
        stringsAsFactors = F)
      
      geocoverage_out <- geocoverage_out[complete.cases(geocoverage_out), ]
      
    } else {
      
      geocoverage_out <- data.frame(
        geographicDescription = character(0),
        northBoundingCoordinate = character(0),
        southBoundingCoordinate = character(0),
        eastBoundingCoordinate = character(0),
        westBoundingCoordinate = character(0),
        stringsAsFactors = F)
      
    }

    # FIXME: Automatically create bounding coordinates
    
    # geocoverage_out$geographicDescription <- 'Bounding area of sites'
    # geocoverage_out$northBoundingCoordinate <- latitude_out
    # geocoverage_out$southBoundingCoordinate <- latitude_out
    # geocoverage_out$eastBoundingCoordinate <- longitude_out
    # geocoverage_out$westBoundingCoordinate <- longitude_out
    
    # Write geographic_coverage.txt -------------------------------------------
    
    if (isTRUE(write.file)){
      
      suppressWarnings(
        utils::write.table(
          geocoverage_out,
          paste0(
            path,
            "/",
            "geographic_coverage.txt"
          ),
          sep = "\t",
          row.names = F,
          quote = F,
          fileEncoding = "UTF-8"
        )
      )
      
    } else if (!exists('data_read_2_x')){
      
      value <- stringr::str_detect(
        names(x$template),
        'geographic_coverage.txt'
      )
      
      if (!any(value)){
        
        message('Adding geographic_coverage.txt to x.')
        
        missing_template <- list(
          content = geocoverage_out
        )
        
        missing_template <- list(
          missing_template
        )
        
        names(missing_template) <- 'geographic_coverage.txt'
        
        x$template <- c(
          x$template, 
          missing_template
        )
        
      }
      
    }
    
  }
  
  # Return values -------------------------------------------------------------
  
  message("Done.")
  
  if (!exists('data_read_2_x') & !isTRUE(empty)){
    
    return(x)
    
  }
  
}
