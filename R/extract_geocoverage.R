#' Extract geographic coverage
#'
#' @description  
#'     Extract detailed geographic coverage (latitude, longitude, and site 
#'     name) to be included in the EML.
#'
#' @usage 
#'     extract_geocoverage(path, data.path = path, data.file, lat.col, 
#'     lon.col, site.col)
#'
#' @param path 
#'     A path to the metadata directory.
#' @param data.path 
#'     A path to the directory containing the data table with geographic 
#'     information. Don't use this argument if the data table is located at 
#'     the path argument listed above.
#' @param data.file 
#'     Name of the input data table containing geographic coverage data.
#' @param lat.col 
#'     Name of latitude column. Values of this column must be in decimal 
#'     degrees. Latitudes south of the equator must be prefixed with a minus 
#'     sign (i.e. dash, "-").
#' @param lon.col 
#'     Name of longitude column. Values of this column must be in decimal 
#'     degrees. Longitudes west of the prime meridian must be prefixed with a 
#'     minus sign (i.e. dash, "-"). 
#' @param site.col
#'     Name of site column. This column lists site specific names to be 
#'     associated with the geographic coordinates.
#'
#' @return 
#'     A tab delimited file in the dataset working directory 
#'     titled \emph{geographic_coverage.txt} and containing decimal degree 
#'     latitude, decimal degree longitude, and site name.
#'
#' @export
#'


extract_geocoverage <- function(path, data.path = path, data.file, lat.col, lon.col, site.col){
  
  # Check arguments and parameterize ------------------------------------------
  
  message("Checking input arguments.")
  
  if (missing(path)){
    stop('Input argument "path" is missing! Specify the path to dataset working directory.')
  }
  if (missing(data.file)){
    stop('Input argument "data.file" is missing! Specify the data file containing the geographic coordinates.')
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
  
  # Validate path
  
  EDIutils::validate_path(path)
  if (!missing(data.path)){
    EDIutils::validate_path(data.path)  
  }
  
  # Validate file names

  data_file <- EDIutils::validate_file_names(path = data.path, data.files = data.file)
  
  # Validate fields of data.files
  
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
    
  } else {
    
    message(paste("Reading ", data_file, ".", sep = ""))
    
    if (delim_guess == ','){
      df_table <- utils::read.csv(file = data_path, header = T, quote = '\"', as.is = T, comment.char = '')
    } else if (delim_guess == '\t'){
      df_table <- utils::read.table(data_path, header = T, sep = '\t', quote = "\"", as.is = T, comment.char = '')
    }
    
    df_table <- as.data.frame(df_table)
    
    # df_table <- utils::read.table(file_path,
    #                        header = TRUE,
    #                        sep = delim_guess,
    #                        quote = "\"",
    #                        as.is = TRUE,
    #                        comment.char = "")
    
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
    
    # Write data to file
    
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

  message("Done.")

}
