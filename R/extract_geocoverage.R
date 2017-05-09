#' Write geographic coverage of unique sampling sites to file
#'
#' @description  A function to write latitude and longitude of unique sampling
#' sites to file.
#'
#' @usage extract_geocoverage(path, table.name, lat.col, lon.col, site.col)
#'
#' @param path Path to the directory containing the data table containing latitude, 
#' longitude, and site name.
#' @param table.name Name of the input data table containing geographic 
#' coverage data.
#' @param lat.col Name of latitude column.
#' @param lon.col Name of longitude column.
#' @param site.col Name of site column.
#'
#' @return A file named datasetname_attributes_draft.xlsx
#' @return A file named datasetname_attributes.xlsx
#' @return A file named datasetname_attributes.txt
#'


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
    
    latitude_out[i] <- as.character(latitude[useI][1])
    
    longitude_out[i] <- as.character(longitude[useI][1])
    
    site_out[i] <- site_name[i]

  }
  
  geocoverage_out <- data.frame(latitude = latitude_out,
                                longitude = longitude_out,
                                site = site_out)
  
  # Write file for inspection
  
  xlsx::write.xlsx(geocoverage_out,
                   paste(path,
                         "/",
                         "geographic_coverage.xlsx", sep = ""),
                   col.names = T,
                   row.names = F,
                   showNA = F)
  
  if (os == "mac"){
    
    system(paste("open",
                 paste(
                   path,
                   "/",
                   "geographic_coverage.xlsx",
                   sep = "")))
    
  } else if (os == "win"){
    
    shell.exec(paste(path,
                     "/",
                     "geographic_coverage.xlsx",
                     sep = ""))
    
  }
  
  print("Latitute and longitude must be in decimal degree format.")
  
  print("Longitudes west of the prime meridian and latitudes south of the equator are prefixed with a minus sign (-).")
  
  readline(
    prompt = "Edit the geographic coverage file, save, close and press <enter> when done."
  )
  

}