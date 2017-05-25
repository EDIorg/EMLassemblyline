#' Extract geographic coverage and write to file
#'
#' @description  A function to extract detailed geographic coverage (latitude, 
#'     longitude, site name) from the referenced data table and to be included 
#'     under methods/geographicCoverage in the EML file.
#'
#' @usage extract_geocoverage(path, table.name, lat.col, lon.col, site.col)
#'
#' @param path A path to the dataset working directory containing the data 
#'     table containing decimal degree latitude, longitude, and site name.
#' @param table.name Name of the input data table containing geographic 
#'     coverage data.
#' @param lat.col Name of latitude column. Values of this column must be in 
#'     decimal degrees. Latitudes south of the equator must be prefixed with 
#'     a minus sign (i.e. dash, -).
#' @param lon.col Name of longitude column. Values of this column must be in 
#'     decimal degrees. Longitudes west of the prime meridian must be prefixed 
#'     with a minus sign (i.e. dash, -). 
#' @param site.col Name of site column.
#'
#' @return 
#'     A file in the dataset working directory titled 
#'     \emph{geographic coverage.xlsx} containing decimal degree latitude, 
#'     longitude, and site name.
#'
#' @export
#'
#' @seealso \code{\link{create_eml}} to write the EML file.


extract_geocoverage <- function(path, table.name, lat.col, lon.col, site.col){
  
  # Load dependencies
  
  library("xlsx")
  
  # Get system information
  
  sysinfo <- Sys.info()["sysname"]
  if (sysinfo == "Darwin"){
    os <- "mac"
  } else {
    os <- "win"
  }
  
  # Load data table
  
  df_table <- read.csv(
    paste(path, "/", table.name, sep = ""),
    header=TRUE,
    sep=",",
    quote="\"",
    as.is=TRUE)
  
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
    print("Latitude contain non-numeric values. Remove these from your data table, then rerun this function.")
  }
  if (class(longitude_out) != "numeric"){
    print("Longitude contain non-numeric values. Remove these from your data table, then rerun this function.")
  }
  
  geocoverage_out <- data.frame(latitude = latitude_out,
                                longitude = longitude_out,
                                site = site_out)
  
  # Write file for inspection
  
  write.table(geocoverage_out,
              paste(path,
                    "/",
                    "geographic_coverage.csv", sep = ""),
              sep = ",",
              col.names = T,
              row.names = F)
  
  if (os == "mac"){
    
    system(paste("open",
                 paste(
                   path,
                   "/",
                   "geographic_coverage.csv",
                   sep = "")))
    
  } else if (os == "win"){
    
    shell.exec(paste(path,
                     "/",
                     "geographic_coverage.csv",
                     sep = ""))
    
  }
  
  print("Latitude and longitude must be in decimal degree format.")
  
  print("Longitudes west of the prime meridian and latitudes south of the equator are prefixed with a minus sign (-).")
  
  readline(
    prompt = "Edit the geographic coverage file, save, close and press <enter> when done."
  )
  

}