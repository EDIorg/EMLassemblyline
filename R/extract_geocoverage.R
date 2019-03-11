#' Import template for geographic coverage
#'
#' @description  
#'     Import template for describing the geographic coverage of a dataset.
#'     The content of this template is automatically extracted from a data 
#'     table containing geographical coordinates.
#'
#' @usage 
#'     extract_geocoverage(path, data.path = path, data.table, lat.col, 
#'     lon.col, site.col, return.obj = FALSE, write.file = TRUE, 
#'     data.file = NULL)
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
#' @param return.obj
#'     (logical) Return the `geocoverage` data.frame.
#' @param write.file
#'     (logical) Write `geocoverage` file to `path`.
#' @param data.file
#'     NOTE: `data.file` has been deprecated. Use `data.table` instead.
#'
#' @return 
#'     \itemize{
#'         \item{`Status messages` describing the geocoverage creation status}
#'         \item{`geographic_coverage.txt` A tab delimited file written to 
#'         `path` containing geographic coordinates and corresponding site 
#'         names.}
#'         \item{`geocoverage data.frame` when `return.obj = TRUE`}
#'     }
#'     
#' @details 
#'     Existing template will not be overwritten by subsequent calls to 
#'     `extract_geocoverage`.     
#'
#' @export
#'


extract_geocoverage <- function(path, data.path = path, data.table, lat.col, 
                                lon.col, site.col, return.obj = FALSE,
                                write.file = TRUE, data.file = NULL){
  
  # Check arguments and parameterize ------------------------------------------
  
  message("Checking input arguments.")
  
  if (missing(path)){
    stop('Input argument "path" is missing! Specify the path to dataset working directory.')
  }
  if (!is.null(data.file)){
    stop('Input argument "data.file" has been deprecated. Use "data.table" instead.')
  }
  if (missing(data.table)){
    stop('Input argument "data.table" is missing! Specify the data file containing the geographic coordinates.')
  }
  if (missing(lat.col)){
    stop('Input argument "lat.col" is missing! Specify latitude column name.')
  }
  if (missing(lon.col)){
    stop('Input argument "lon.col" is missing! Specify longitude column name.')
  }
  if (missing(site.col)){
    stop('Input argument "site.col" is missing! Specify site column name.')
  }
  
  # Validate file names

  data_file <- EDIutils::validate_file_names(path = data.path, data.files = data.table)
  
  # Validate fields of data.tables
  
  EDIutils::validate_fields(path = data.path, data.files = data_file)
  
  # Get file names ------------------------------------------------------------
  
  files <- list.files(data.path)
  use_i <- stringr::str_detect(string = files,
                      pattern = stringr::str_c("^", data_file, collapse = "|"))
  data_files <- files[use_i]
  
  # Detect operating system
  
  os <- EDIutils::detect_os()
  
  # Detect data file delimeter
  
  delim_guess <- EDIutils::detect_delimeter(data.path, data.files = data_file, os)
  
  # Read data table -----------------------------------------------------------
  
  file_path <- paste(data.path,
                     "/",
                     data_file,
                     sep = "")
  
  if (file.exists(paste(data.path, "/geographic_coverage.txt", sep = ""))){
    
    message("geographic_coverage.txt already exists!")
    
    geocoverage_out <- NULL
    
  } else {
    
    message(paste("Reading ", data_file, ".", sep = ""))
    
    if (delim_guess == ','){
      df_table <- utils::read.csv(file = file_path, header = T, quote = '\"', as.is = T, comment.char = '')
    } else if (delim_guess == '\t'){
      df_table <- utils::read.table(file_path, header = T, sep = '\t', quote = "\"", as.is = T, comment.char = '')
    }
    
    df_table <- as.data.frame(df_table)
    
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
    
    message("Extracting coordinates and site names.")
    
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
    
    # Write data to file ------------------------------------------------------
    
    if (isTRUE(write.file)){
    
      lib_path <- system.file(
        '/example_dataset/metadata_templates/abstract.txt',
        package = 'EMLassemblyline')
      lib_path <- substr(lib_path, 1, nchar(lib_path) - 48)
      
      if (!stringr::str_detect(path, lib_path)){
        message("Writing geographic_coverage.txt.")
        suppressWarnings(utils::write.table(geocoverage_out,
                                            paste(path,
                                                  "/",
                                                  "geographic_coverage.txt", sep = ""),
                                            sep = "\t",
                                            row.names = F,
                                            quote = F,
                                            fileEncoding = "UTF-8"))
      }
    }

  }

  message("Done.")
  
  # Return values -------------------------------------------------------------
  
  if (isTRUE(return.obj)){
    geocoverage_out
  }

}
