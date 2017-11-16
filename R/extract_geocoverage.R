#' Extract geographic coverage
#'
#' @description  
#'     Extract detailed geographic coverage (latitude, longitude, and site 
#'     name) to be included in the EML.
#'
#' @usage 
#'     extract_geocoverage(path, table.name, lat.col, lon.col, site.col)
#'
#' @param path 
#'     A path to the dataset working directory containing the data table with 
#'     geographic information.
#' @param table.name 
#'     Name of the input data table containing geographic coverage data. The 
#'     table name must include the file extension (e.g. .csv).
#' @param lat.col 
#'     Name of latitude column. Values of this column must be in decimal 
#'     degrees. Latitudes south of the equator must be prefixed with a minus 
#'     sign (i.e. dash, -).
#' @param lon.col 
#'     Name of longitude column. Values of this column must be in decimal 
#'     degrees. Longitudes west of the prime meridian must be prefixed with a 
#'     minus sign (i.e. dash, -). 
#' @param site.col
#'     Name of site column. This column lists site specific names to be 
#'     associated with the geographic coordinates.
#'
#' @return 
#'     A tab delimited UTF-8 formatted file in the dataset working directory 
#'     titled \emph{geographic_coverage.txt} and containing decimal degree 
#'     latitude, decimal degree longitude, and site name.
#'
#' @export
#'
#' @seealso \code{\link{make_eml}} to make the EML file.


extract_geocoverage <- function(path, table.name, lat.col, lon.col, site.col){
  
  # Check arguments
  
  if (missing(path)){
    stop("Specify path to dataset working directory.")
  }
  
  if (missing(table.name)){
    stop("Specify table name.")
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
  
  # Load the datasets configuration file
  
  print("Loading configuration file ...")
  
  source(paste(path, "/eml_configuration.R", sep = ""))
  
  # Read data table
  
  print(paste("Reading", table.name, "...", sep = " "))
  
  use_i <- match(table.name, table_names)
  
  df_table <- read.table(
    paste(path, "/", table.name, sep = ""),
    header=TRUE,
    sep=",",
    quote="\"",
    as.is=TRUE,
    comment.char = "")
  
  # if (field_delimeter[use_i] == "comma"){
  #   
  #   df_table <- read.table(
  #     paste(path, "/", table.name, sep = ""),
  #     header=TRUE,
  #     sep=",",
  #     quote="\"",
  #     as.is=TRUE,
  #     comment.char = "")
  #   
  # } else if (field_delimeter[use_i] == "tab"){
  #   
  #   df_table <- read.table(
  #     paste(path, "/", table.name, sep = ""),
  #     header=TRUE,
  #     sep="\t",
  #     quote="\"",
  #     as.is=TRUE,
  #     comment.char = "")
    
  }
  
  # Get vectors of latitude, longitude, and site
  
  print("Selecting variables ...")
  
  latitude <- df_table[lat.col]
  
  longitude <- df_table[lon.col]
  
  site_name <- unique(unlist(df_table[site.col]))
  
  # Output lat and long corresponding to sites
  
  print("Extracting coordinates and site names ...")
  
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
    print("Latitude contains non-numeric values. Remove these from your data table, then rerun this function.")
  }
  if (class(longitude_out) != "numeric"){
    print("Longitude contains non-numeric values. Remove these from your data table, then rerun this function.")
  }
  
  geocoverage_out <- data.frame(latitude = latitude_out,
                                longitude = longitude_out,
                                site = site_out)
  
  # Write data to file
  
  print("Writing geographic_coverage.txt ...")
  
  write.table(geocoverage_out,
              paste(path,
                    "/",
                    "geographic_coverage.txt", sep = ""),
              sep = "\t",
              row.names = F,
              quote = F,
              fileEncoding = "UTF-8")
  
  print("Done.")

}