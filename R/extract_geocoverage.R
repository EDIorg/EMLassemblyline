#' Extract geographic coverage
#'
#' @description  
#'     Extract detailed geographic coverage (latitude, longitude, and site 
#'     name) to be included in the EML.
#'
#' @usage 
#'     extract_geocoverage(path, data.file, lat.col, lon.col, site.col)
#'
#' @param path 
#'     A path to the dataset working directory containing the data table with 
#'     geographic information.
#' @param data.file 
#'     Name of the input data table containing geographic coverage data..
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


extract_geocoverage <- function(path, data.file, lat.col, lon.col, site.col){
  
  # Check for arguments -------------------------------------------------------
  
  if (missing(path)){
    stop("Specify path to dataset working directory.")
  }
  
  if (missing(data.file)){
    stop("Specify the data file containing the geographic coordinates.")
  }
  
  if (missing(lat.col)){
    stop("Specify latitude column name.")
  }
  
  if (missing(lat.col)){
    stop("Specify latitude column name.")
  }
  
  if (missing(lat.col)){
    stop("Specify site column name.")
  }
  
  # Check arguments and modify for script -------------------------------------
  
  message("Checking input arguments ...")
  
  # Data names are valid
  
  files <- list.files(path)
  files <- c(files, str_replace(files, "\\.[:alnum:]*$", replacement = ""))
  use_i <- str_detect(string = files,
                      pattern = str_c("^", data.file, "$", collapse = "|"))
  if (!sum(use_i) == length(data.file)){
    if(sum(use_i) == 0){
      stop(paste("Invalid data.file entered: ", paste(data.file, collapse = ", "), sep = ""))
    } else {
      name_issues <- data.files[!files[use_i] == data.file]
      stop(paste("Invalid data.file entered: ", paste(name_issues, collapse = ", "), sep = ""))
    }
  }
  
  # Get actual file name for script
  
  files <- list.files(path)
  use_i <- str_detect(string = files,
                      pattern = str_c("^", data.file, collapse = "|"))
  data_file <- files[use_i]
  
  # Path is valid
  
  value <- file.exists(paste(path, "/", data_file, sep = ""))
  if (!isTRUE(value)){
    stop(paste('Invalid file path entered: ', path))
  }
  
  # Detect users operating system
  
  sysinfo <- Sys.info()["sysname"]
  if (sysinfo == "Darwin"){
    os <- "mac"
  } else {
    os <- "win"
  }
  
  # Read data table
  
  file_path <- paste(path,
                     "/",
                     data_file,
                     sep = "")
  
  if (os == "mac"){
    delim_guess <- get.delim(file_path,
                             n = 2)
  } else if (os == "win"){
    delim_guess <- get.delim(file_path,
                             n = 1)
  }
  
  message(paste("Reading", data_file, "...", sep = " "))
  
  df_table <- read.table(file_path,
                         header = TRUE,
                         sep = delim_guess,
                         quote = "\"",
                         as.is = TRUE,
                         comment.char = "")
  
  # Get vectors of latitude, longitude, and site
  
  message("Selecting variables ...")
  
  latitude <- df_table[lat.col]
  
  longitude <- df_table[lon.col]
  
  site_name <- unique(unlist(df_table[site.col]))
  
  # Output lat and long corresponding to sites
  
  message("Extracting coordinates and site names ...")
  
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
                                site = site_out)
  
  # Write data to file
  
  message("Writing geographic_coverage.txt ...")
  
  write.table(geocoverage_out,
              paste(path,
                    "/",
                    "geographic_coverage.txt", sep = ""),
              sep = "\t",
              row.names = F,
              quote = F,
              fileEncoding = "UTF-8")
  
  message("Done.")

}