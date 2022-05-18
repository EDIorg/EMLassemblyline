#' Describe where the data were collected -- for shp files.
#'
#' @description Use this function to extract the unique location coordinates and
#'  names from a shapefile or to create a blank template to manually fill. For
#'  each feature in the shapefile, the coverage will create a covering polygon
#'  with \{sf\}'s convex hull algorithm (draws a covering area with little
#'   precision).
#'
#' @param path
#'     (character) Path to the metadata template directory.
#' @param data_path
#'     (character) Path to the data directory. Defaults to
#'     "\code{<path>}/data_objects".
#' @param site_col
#'     (character) Column name containing site names. If multiple shp files are
#'     present in data_path, this column shall be common to all of them.
#' @param write_file
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
#'     \item{site_name: sites names from the column <site_col>.}
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
#' 
#' # Data used in this example represents the french department
#' # of Finistere.
#' 
#' # Set working directory
#' setwd("/Users/me/Documents/data_packages/pkg_260")
#'
#' # For a shapefile containing site names in column "site_name"
#' geo_cov <- template_spatial_coverage(
#'   path = "./metadata_templates",
#'   data_path = "./data_objects",
#'   site_col = "nom")
#' 
#' # You will get both geographic_coverage.txt and 
#' # spatial_coverage.txt written based on this shapefile.
#' # You can view the example result by using the {leaflet}
#' # package:
#' 
#' library(leaflet)
#' leaflet() |>
#'   addTiles() |>
#'   addPolygons(data = polygon[[1]])
#' }
#'
template_spatial_coverage <- function(
    path,
    data_path = path,
    site_col, # FIXME in doc when example data is set
    write_file = TRUE,
    overwrite = FALSE
) {
  
  # Checks ----
  
  ### TODO Surely add more controls here ###
  
  # assumes: if spatial_coverage.txt is written, geographic_coverage.txt is too
  written <- isTRUE(file.exists(paste0(path, "/spatial_coverage.txt")))
  if (written & isFALSE(overwrite))
    message("spatial_coverage.txt already exists!")
  if (written & !isFALSE(overwrite)) # overwrite can == "append"
    message("spatial_coverage.txt will be overwritten.")
  # if all is written and no overwrite allowed, stop.
  if (written & isFALSE(overwrite))
    return(NULL)
  
  # better listing to be done
  data_files <- dir(data_path, full.names = TRUE)
  is_shp <- sapply(data_files, is_shp_dir)
  if (!any(is_shp)) {
    message("No shapefile found.")
    return(NULL)
  }
  
  # Check column names to read
  if (is.null(site_col)) {
    message("No column name provided.")
    return(NULL)
  }
  
  # Coverages ----
  
  # If asked, append new coverage to existing one
  if (written & overwrite == "append") {
    shp_coverage <- utils::read.csv(
      paste0(path, "/spatial_coverage.txt"),
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
  
  if (file.exists(paste0(path, "/geographic_coverage.txt")) &
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
  
  sapply(data_files[is_shp], function(shp_file) {
    # If zipped
    if (grepl("zip$", shp_file))
      shp_file <- dirname(unzip(shp_file, exdir = tempdir())[1])
    # Get shp data
    shp <- sf::read_sf(shp_file) # shp itself
    # Ensure the column is available in the shp
    if (isFALSE(site_col %in% names(shp))) {
      message(sprintf("Column '%s' not found in '%s'.", 
                      site_col, basename(shp_file)))
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
        file = basename(shp_file),
        site_name = shp[[site_col]],
        wkt = sf::st_as_text(st_conv_cov)
      )
    ) %>%
      unique()
    geo_coverage <<- rbind(
      geo_coverage,
      data.frame(
        geographicDescription = shp[[site_col]],
        northBoundingCoordinate = bboxes["ymax", ],
        southBoundingCoordinate = bboxes["ymin", ],
        eastBoundingCoordinate  = bboxes["xmax", ],
        westBoundingCoordinate  = bboxes["xmin", ],
        stringsAsFactors = FALSE
      )
    ) %>%
      unique()
  })
  
  # Output ----
  
  if (isTRUE(write_file)) {
    suppressWarnings(
      utils::write.table(
        shp_coverage,
        gsub(
          "//", "/", # remove "//" from path
          paste0(path, "/spatial_coverage.txt")
        ),
        sep = "\t",
        row.names = F,
        quote = F,
        fileEncoding = "UTF-8"
      )
    )
    message("spatial_coverage.txt")
    
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
    message("geographic_coverage.txt")
  }
  
  return(
    list(
      shp_coverage = shp_coverage,
      geographic_coverage = geo_coverage
    )
  )
  
}

#' @noRd
is_shp_dir <- function(dir_path) {
  # check for folder path & presence of main file of the shp ESRI format
  (dir.exists(dir_path) && length(dir(dir_path, pattern = "shp/?$")) > 0) ||
    # OR check for zip path & presence of main file of the shp ESRI format
    (grepl("zip$", dir_path) &&
       any(grepl("shp/?$", unzip(dir_path, list = TRUE)[, 1])))
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
#'
write_shp_dir <- function(shp, path, name) {
  # Missing arguments
  if (any(missing(shp) | isFALSE(any(class(shp) == "sf"))))
    stop("No shp object to write.")
  if (missing(path))
    stop("No destination given.")
  if (missing(name))
    stop("No name given to file.")
  
  # Curate arguments
  if (isFALSE(dir.exists(path)))
    dir.create(path, recursive = TRUE)
  
  # (Over)Write shp folder
  file_name <- fs::path(path, name)
  if (dir.exists(file_name))
    unlink(file_name, recursive = TRUE)
  message(sprintf("Writing %s .", file_name))
  sf::st_write(shp, file_name, driver = "ESRI Shapefile")
  
  return(file_name)
}
