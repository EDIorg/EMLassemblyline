#' Create geographic coverage template
#'
#' @description  
#'     Import template for describing the geographic coverage of a dataset.
#'     The content of this template is automatically extracted from a data 
#'     table containing geographical coordinates.
#'
#' @usage 
#'     template_geographic_coverage(
#'       path, 
#'       data.path = path, 
#'       data.table, 
#'       lat.col, 
#'       lon.col, 
#'       site.col, 
#'       x = NULL, 
#'       write.file = TRUE
#'     )
#'
#' @param path 
#'     (character) Path to where the template(s) will be imported.
#' @param data.path
#'     (character) Path to where the data files are stored.
#' @param data.table 
#'     (character) Data table containing geographic coordinates represented in 
#'     decimal degrees, where latitudes south of the equator and longitudes 
#'     west of the prime meridian are negative.
#' @param lat.col 
#'     (character) Name of latitude column.
#' @param lon.col 
#'     (character) Name of longitude column.
#' @param site.col
#'     (character) Name of site column, where site is the name of the location
#'     specified by `lat.col` and `lon.col`.
#' @param x
#'     (named list) Alternative input/output to `EMLassemblyline` functions. 
#'     Use `make_arguments()` to create `x`.
#' @param write.file
#'     (logical) Write `geographic_coverage.txt` to `path`.
#'
#' @return 
#'     \itemize{
#'         \item{`geographic_coverage.txt` A tab delimited file written to 
#'         `path` containing geographic coordinates and corresponding site 
#'         names.}
#'         \item{If using `x`, then content of `geographic_coverage.txt` is 
#'         added to `x` under `/x/templates`}
#'     }
#'     
#' @details 
#'     Existing templates will not be overwritten by subsequent calls to 
#'     `template_geographic_coverage`.
#'
#' @export
#'

template_geographic_coverage <- function(path, data.path = path, data.table, 
                                         lat.col, lon.col, site.col, x = NULL, 
                                         write.file = TRUE){
  
  message('Creating geographic coverage template.')
  
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
    
    # Validate file name
    
    data_file <- EDIutils::validate_file_names(
      path = data.path, 
      data.files = data.table
    )
    
    # Validate fields of data.tables
    
    EDIutils::validate_fields(
      path = data.path, 
      data.files = data_file
    )
    
    # Read data table
    
    x <- make_arguments(
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
    
    geocoverage_out <- data.frame(
      geographicDescription = site_out,
      northBoundingCoordinate = latitude_out,
      southBoundingCoordinate = latitude_out,
      eastBoundingCoordinate = longitude_out,
      westBoundingCoordinate = longitude_out,
      stringsAsFactors = F)
    
    # # Add bounding coordinates
    # 
    # geocoverage_out$geographicDescription <- 'Bounding area of sites'
    # geocoverage_out$northBoundingCoordinate <- latitude_out
    # geocoverage_out$southBoundingCoordinate <- latitude_out
    # geocoverage_out$eastBoundingCoordinate <- longitude_out
    # geocoverage_out$westBoundingCoordinate <- longitude_out
    
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
