#' Describe where the data were collected -- for shp files.
#'
#' @description Use this function to extract the unique location coordinates and
#'  names from a shapefile or to create a blank template to manually fill. For 
#'  each feature in the shapefile, the coverage will create a covering polygon
#'  with \{sf\}'s convex hull algorithm (draws a covering area with little precision).
#' 
#' @param path 
#'     (character) Path to the metadata template directory.
#' @param data.path 
#'     (character) Path to the data directory. Defaults to "\code{<path>}/data_objects". 
#' @param site.col 
#'     (character) Column name containing site names. If multiple shp files are 
#'     present in data.path, this column shall be common to all of them.
#' @param write.file 
#'     (logical; optional) Whether to write the template file.
#' @param overwrite
#'     (various; optional) Can be either FALSE (stop the function if template is
#'     already written), TRUE (replace the template if it is already writtent) 
#'     or "append" (add new coverage to existing one, and do remove duplicated
#'     rows).
#' 
#' @return
#' A list with two items:
#' \item{spatial_coverage}{Columns:
#'     \itemize{
#'     \item{file: file name for reference.}
#'     \item{site_name: sites names from the column <site.col>.}
#'     \item{wkt: a well-known text polygon covering the given site.}
#'    }
#' }
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
#' @importFrom sf read_sf st_convex_hull st_bbox st_as_text 
#'
#' @examples 
#' \dontrun{
#' # Set working directory
#' setwd("/Users/me/Documents/data_packages/pkg_260")
#' 
#' # For a shapefile containing site names in column "site_name"
#' template_spatial_coverage(
#'   path = "./metadata_templates",
#'   data.path = "./data_objects",
#'   site.col = "site_name")
#'   
#' # Or with defaults
#' template_spatial_coverage(site.col = "site_name")
#' }
#' 
#' @export
#'
template_spatial_coverage <- function(
  path, 
  data.path = path, 
  site.col, # FIXME in doc when example data is set
  write.file = TRUE,
  overwrite = FALSE
) {
  # Checks ----
  
  ### Surely add more controls here ###
  
  # assumes: if .spatial_coverage.txt is written, geographic_coverage.txt is too.
  written <- isTRUE(file.exists(paste0(path, "/.spatial_coverage.txt")))
  if(written & isFALSE(overwrite))
    message(".spatial_coverage.txt already exists!")
  if(written & !isFALSE(overwrite)) # overwrite can == "append"
    message(".spatial_coverage.txt will be overwritten.")
  # if all is written and no overwrite allowed, stop.
  if(written & isFALSE(overwrite))
    return(NULL)

  data.files <- dir(data.path, full.names = TRUE)
  is.shp <- sapply(data.files, is.shp.dir)
  if(!any(is.shp)){
    message("No shapefile found.")
    return(NULL)
  }
  
  # Check column names to read
  if(is.null(site.col)){
    message("No column name provided.")
    return(NULL)
  }
  
  # Coverages ----
  
  if(written & 
     overwrite == "append") {
    shp_coverage <- utils::read.csv(
      paste0(path, "/.spatial_coverage.txt"),
      TRUE,
      sep = "\t"
    )
  } else {
    shp_coverage <- data.frame(
      file = c(),
      site_name = c(),
      wkt = c()
    )
  }
  
  
  if(file.exists(paste0(path, "/geographic_coverage.txt")) & 
     overwrite == "append") {
    geo_coverage <- utils::read.csv(
      paste0(path, "/geographic_coverage.txt"), 
      TRUE,
      sep = "\t"
    )
  } else {
    geo_coverage <- data.frame(
      geographicDescription = c(),
      northBoundingCoordinate = c(),
      southBoundingCoordinate = c(),
      eastBoundingCoordinate  = c(),
      westBoundingCoordinate  = c(),
      stringsAsFactors = FALSE
    )
  }
  
  sapply(data.files[is.shp], function(shp.file){
    # If zipped
    if(grepl("zip$", shp.file))
      shp.file <- dirname(unzip(shp.file, exdir = tempdir())[1])
    # Get shp data
    shp <- sf::read_sf(shp.file) # shp itself
    # Ensure the column is available in the shp
    if(isFALSE(site.col %in% names(shp))) {
      message(sprintf("Column '%s' not found in '%s'.", site.col, basename(shp.file)))
      return(NULL)
    }
    # Get coverage polygons
    st_conv_cov <- sf::st_convex_hull(shp$geometry)
    # Get bounding boxes -- based on convex hull shapes
    bboxes <- sapply(st_conv_cov, sf::st_bbox)
    
    # Write coverage
    shp_coverage <<- rbind(
      shp_coverage,
      data.frame(
        file = basename(shp.file),
        site_name = shp[[site.col]],
        wkt = sf::st_as_text(st_conv_cov)
      )
    ) |>
      unique()
    geo_coverage <<- rbind(
      geo_coverage,
      data.frame(
        geographicDescription = shp[[site.col]],
        northBoundingCoordinate = bboxes["ymax",],
        southBoundingCoordinate = bboxes["ymin",],
        eastBoundingCoordinate  = bboxes["xmax",],
        westBoundingCoordinate  = bboxes["xmin",],
        stringsAsFactors = FALSE
      )
    ) |>
      unique()
  })
  
  message('.spatial_coverage.txt')
  
  # Output ----
  
  if(isTRUE(write.file)){
    suppressWarnings(
      utils::write.table(
        shp_coverage,
        gsub(
          "//", "/", # remove "//" from path
          paste0(path, "/.spatial_coverage.txt")
        ),
        sep = "\t",
        row.names = F,
        quote = F,
        fileEncoding = "UTF-8"
      )
    )
    
    
    suppressWarnings(
      utils::write.table(
        geo_coverage,
        gsub(
          "//", "/", # remove "//" from path
          paste0(path, "/geographic_coverage.txt")
        ),
        sep = "\t",
        row.names = F,
        quote = F,
        fileEncoding = "UTF-8"
      )
    )
    return(NULL)
  } else {
    return(
      list(shp_coverage = shp_coverage, geographic_coverage = geo_coverage)
    )
  }
  
}

is.shp.dir <- function(dir.path) {
  return(  
    # check for folder path & presence of main file of the shp ESRI format
    if(dir.exists(dir.path) & length(dir(dir.path, pattern = "shp/?$")) > 0){
      # OR check for zip path & presence of main file of the shp ESRI format
      grepl("zip$", dir.path) & grepl("shp/?$", unzip(dir.path, list=TRUE)[1,1])
    } else
      FALSE
  )
}

#' Write shapefile in a folder
#' 
#' @param shp 
#'    (sf) object to write.
#' @param path
#'    (character) folder in which to create a shp file. Will contain a 
#'    folder named \code{name}. 
#' @param name
#'    (character) name of the file that will be created.
write_shp_dir <- function(shp, path, name) {
  # Missing arguments
  if(any(missing(shp) | isFALSE(any(class(shp) == "sf"))))
    stop("No shp object to write.")
  if(missing(path))
    stop("No destination given.")
  if(missing(name))
    stop("No name given to file.")
  
  # Curate arguments
  if(isFALSE(dir.exists(path)))
    dir.create(path, recursive = TRUE)
  
  # (Over)Write shp folder
  file_name <- fs::path(path, name)
  if(dir.exists(file_name))
    unlink(file_name, recursive = TRUE)
  message(sprintf("Writing %s .", file_name))
  sf::st_write(shp, file_name, driver = "ESRI Shapefile")
  
  return(file_name)
}
