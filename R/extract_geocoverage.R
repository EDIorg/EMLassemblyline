#' Import template for geographic coverage
#'
#' @description  
#'     Import template for defining geographic points and areas of a dataset. 
#'     The content of this template is automatically extracted from a data 
#'     table containing geographic coordinates.
#'
#' @usage 
#'     extract_geocoverage(
#'       path, 
#'       data.path = path, 
#'       data.table, 
#'       lat.col, 
#'       lon.col, 
#'       site.col, 
#'       write.file = TRUE,
#'       x = NULL
#'     )
#'
#' @param path 
#'     (character) Path to the directory where the geographic coverage 
#'     template will be written.
#' @param data.path
#'     (character) Path to the directory containing \code{data.table}.
#' @param data.table 
#'     (character) Name of table containing geographic coordinates 
#'     represented in decimal degrees, where latitudes south of the equator 
#'     and longitudes west of the prime meridian are negative.
#' @param lat.col 
#'     (character) Name of latitude column.
#' @param lon.col 
#'     (character) Name of longitude column.
#' @param site.col
#'     (character) Name of site column, where site is the name of the location
#'     specified by \code{lat.col} and \code{lon.col}.
#' @param write.file
#'     (logical; optional) Whether to write the geographic coverage template 
#'     to \code{path}.
#' @param x
#'     (named list; optional) Alternative input to 
#'     \code{EMLassemblyline} functions. Use \code{template_arguments()} to 
#'     create \code{x}.
#'
#' @return 
#'     \itemize{
#'         \item{\strong{geographic_coverage.txt} The geographic 
#'         coverage template. A tab delimited table.}
#'         \item{If using \code{x}, then the geographic coverage template is 
#'         added to \strong{/x/templates}.}
#'     }
#'     
#' @details 
#'     An existing geographic coverage template will not be overwritten by 
#'     subsequent calls to \code{extract_geocoverage()}.
#'     
#' @examples 
#' # Set working directory
#' setwd(tempdir())
#' 
#' # Create data package directory "pkg_255"
#' file.copy(
#'   from = system.file('/examples/pkg_255', package = 'EMLassemblyline'),
#'   to = '.',
#'   recursive = TRUE
#' )
#' 
#' # View directory contents (NOTE: geographic_coverage.txt doesn't exist)
#' dir('./pkg_255/metadata_templates')
#' 
#' # Extract geographic coverage
#' extract_geocoverage(
#'   path = './pkg_255/metadata_templates',
#'   data.path = './pkg_255/data_objects',
#'   data.table = 'nitrogen.csv',
#'   site.col = 'site_name',
#'   lat.col = 'site_lat',
#'   lon.col = 'site_lon'
#' )
#' 
#' # View directory contents (NOTE: geographic_coverage.txt exists)
#' dir('./pkg_255/metadata_templates')
#' 
#' # Rerunning extract_geocoverage() does not overwrite files
#' extract_geocoverage(
#'   path = './pkg_255/metadata_templates',
#'   data.path = './pkg_255/data_objects',
#'   data.table = 'nitrogen.csv',
#'   site.col = 'site_name',
#'   lat.col = 'site_lat',
#'   lon.col = 'site_lon'
#' )
#' 
#' # Clean up
#' unlink(
#'   './pkg_255',
#'   recursive = TRUE
#' )
#'     
#' @export
#'

extract_geocoverage <- function(path, data.path = path, data.table, lat.col, 
                                lon.col, site.col, x = NULL, write.file = TRUE, 
                                data.file){
  
  message('Creating geographic coverage template.')
  
  # Send deprecation notice ---------------------------------------------------
  
  .Deprecated(
    new = 'template_geographic_coverage',
    package = 'EMLassemblyline',
    old = 'extract_geocoverage'
  )
  
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
    fun.name = 'extract_geocoverage',
    fun.args = as.list(environment())
  )
  
  # Handle deprecated arguments
  
  if (!missing(data.file)){
    
    warning(
      'Argument "data.file" is deprecated; please use "data.table" instead.',
      call. = FALSE)

    data.table <- data.file
    
  }

  # Read data -----------------------------------------------------------------
  
  # If not using x ...
  
  if (is.null(x)){
    
    # Validate file name
    
    data_file <- suppressWarnings(
      EDIutils::validate_file_names(
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

  # If using x ...  
        
  } else if (!is.null(x)){

    # data.table
    
    data_file <- data.table
    
    # write.file
    
    if (isTRUE(write.file)){
      
      stop('Input argument "write.file" is not supported when using "x".')
      
    }
    
  }
  
  # Extract geographic coverage -----------------------------------------------
  
  if (is.data.frame(x$template[['geographic_coverage.txt']]$content)){
    
    message("geographic_coverage.txt already exists!")
    
  } else {

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
      
      latitude_out[i] <- latitude[useI][1]
      
      longitude_out[i] <- longitude[useI][1]
      
      site_out[i] <- site_name[i]
      
    }
    
    if (class(latitude_out) != "numeric"){
      stop("Latitude contains non-numeric values. Remove these from your data table, then rerun this function.")
    }
    if (class(longitude_out) != "numeric"){
      stop("Longitude contains non-numeric values. Remove these from your data table, then rerun this function.")
    }
    
    geocoverage_out <- data.frame(latitude = latitude_out,
                                  longitude = longitude_out,
                                  site = site_out,
                                  stringsAsFactors = F)
    
    # Write geographic_coverage.txt -------------------------------------------
    
    if (isTRUE(write.file)){
      
      message('Writing geographic_coverage.txt to path')
      
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
  
  if (!exists('data_read_2_x')){
    
    return(x)
    
  }

}
