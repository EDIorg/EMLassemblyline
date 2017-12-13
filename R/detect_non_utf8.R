#' Detect non-UTF-8 encoded character strings
#'
#' @description  
#'     Identify non-UTF-8 encoded character strings and report location 
#'     relative to user supplied inputs.
#'
#' @usage 
#'     detect_non_utf8(str = "", method = "")
#'
#' @param x
#'     A character vector.
#'     \itemize{
#'         \item{table} Check character strings of table and report column, 
#'         row, and value of offending character string.
#'         row of offending entity.
#'         \item{text} Check paragraphs of input text and report line number 
#'         and value of character string.
#'     }
#'
#' @export
#'


detect_non_utf8 <- function(str, method){
  
  # Check arguments and parameterize ------------------------------------------
  
  message("Checking input arguments.")
  
  if (missing(str)){
    stop('Input argument "str" is missing! Specify the string to be tested.')
  }
  if (missing(method)){
    stop('Input argument "method" is missing! Valid options are "table" and "text".')
  }

  # Validate path
  
  validate_path(path)
  
  # Validate file names
  
  data_file <- validate_file_names(path, data.files = data.file)
  
  # Validate fields of data.files
  
  validate_fields(path, data.files = data_file)
  
  # Get file names ------------------------------------------------------------
  
  files <- list.files(path)
  use_i <- str_detect(string = files,
                      pattern = str_c("^", data_file, collapse = "|"))
  data_files <- files[use_i]
  
  # Detect operating system
  
  os <- detect_os()
  
  # Detect data file delimeter
  
  delim_guess <- detect_delimeter(path, data.files = data_file, os)
  
  # Read data table -----------------------------------------------------------
  
  file_path <- paste(path,
                     "/",
                     data_file,
                     sep = "")
  
  if (file.exists(paste(path, "/geographic_coverage.txt", sep = ""))){
    
    message("geographic_coverage.txt already exists!")
    
  } else {
    
    message(paste("Reading ", data_file, ".", sep = ""))
    
    df_table <- read.table(file_path,
                           header = TRUE,
                           sep = delim_guess,
                           quote = "\"",
                           as.is = TRUE,
                           comment.char = "")
    
    # Validate column names
    
    columns <- colnames(df_table)
    columns_in <- c(lat.col, lon.col, site.col)
    use_i <- str_detect(string = columns,
                        pattern = str_c("^", columns_in, "$", collapse = "|"))
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
    df_table <- df_table[complete.cases(df_table), ]
    
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
    
    suppressWarnings(write.table(geocoverage_out,
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