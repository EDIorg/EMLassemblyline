#' Create template for information need to create EML element of vector file
#'
#' @description Create a template containing all mandatory information need to create spatialVector EML element.
#'
#' @param path
#'     (character) Path to the metadata template directory.
#' @param data.path
#'     (character) Path to the data directory.
#' @param vector.file
#'     (character) File names of vector files. If more than one, then supply as
#'     a vector.
#' @param write.file
#'     (logical; optional) Whether to write the template file.
#'
#' @return
#' \item{information_*}{Columns:
#'     \itemize{
#'     \item{geometry: Geometry of features in the vector file.  Accepted values are : 
#'           "Point", "LineString", "LinearRing", "Polygon", "MultiPoint", "MultiLineString", "MultiPolygon", "MultiGeometry"}
#'     \item{geometricObjectCount: Number of geometric objects}
#'     \item{spatialReference: Coordinate system used.}
#'     }
#' }
#'
#' @examples
#' \dontrun{
#' # Set working directory
#' setwd("/Users/me/Documents/data_packages/pkg_260")
#' 
#' template_vector_information(
#'   path = "./metadata_templates",
#'   data.path = "./data_objects",
#'   vector.file = c("shapefile_test", "geojson_test_file.GeoJSON"))
#' }
#' 
#' @export
#'
template_vector_information <- function(
  path = NULL,
  data.path = path,
  vector.file = NULL,
  write.file = TRUE) {

  message('Templating vector information ...')

  # Validate arguments --------------------------------------------------------

  # Validate path usage before passing arguments to validate_arguments()
  if (missing(path)){
    stop('Input argument "path" is missing.')
  }

  validate_arguments(
    fun.name = 'template_vector_information',
    fun.args = as.list(environment()))

  # If files are absent ...

  if (is.null(vector.file)){

    stop('Input argument "vector.file" is missing.')

  }

  # Import information_*.txt ---------------------------------------------------

  # Extract information of each vector file

  information <- list()

  for (i in 1:length(vector.file)){

    # Read file

    vectorFile <- terra::vect(paste(data.path, vector.file[i], sep = "/"))

    # Initialize information table

    information[[i]] <- data.frame(
      geometry = "",
      # Not mandatory
      geometricObjectCount = nrow(vectorFile),
      spatialReference = "",
      stringsAsFactors = FALSE
    )
    
    # Get geomtype
    
    vector_geomtype <- terra::geomtype(vectorFile)
    
    if (vector_geomtype == "points"){
      information[[i]]$geometry <- "Point"
    } else if (vector_geomtype == "polygons"){
      information[[i]]$geometry <- "Polygon"
    } else if (vector_geomtype == "lines"){
      information[[i]]$geometry <- "LineString"
    }
    
    # Get spatialReference
    
    projection <- terra::crs(vectorFile, proj = TRUE)
    
    if (grepl("+datum=WGS84", projection)){
      information[[i]]$spatialReference <- "GCS_WGS_1984"
    } else if (projection == "+proj=lcc +lat_0=46.5 +lon_0=3 +lat_1=49 +lat_2=44 +x_0=700000 +y_0=6600000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"){
      information[[i]]$spatialReference <- "RGF_1993_Lambert_93"
    }
    

    if (isTRUE(write.file)){

      # TODO: check for other vector file type (not only shp and GeoJSON)
      if (is_shp_dir(paste(data.path, vector.file[i], sep = "/"))){
        filename <- tools::file_path_sans_ext(vector.file[i])
      } else if (tolower(tools::file_ext(vector.file[i])) == "geojson") { # GeoJSON not in mime::guess_type function
        filename <- substr(vector.file[i], 1, nchar(vector.file[i]) - 8)
      }
      
      value <- file.exists(
        paste0(
          path,
          "/",
          "information_",
          filename,
          ".txt"
        )
      )

      if (!isTRUE(value)){

        message(
          paste0(
            "information_",
            filename,
            ".txt."
          )
        )

        utils::write.table(
          information[[i]],
          paste0(
            path,
            "/",
            "information_",
            enc2utf8(
              filename),
            ".txt"
          ),
          sep = "\t",
          row.names = F,
          quote = F,
          fileEncoding = "UTF-8"
        )

      } else {

        message(
          paste0(
            "information_",
            filename,
            ".txt already exists!"
          )
        )

      }

    }

  }

  # Return --------------------------------------------------------------------

  if (!isTRUE(write.file)){
    message('No templates were written to file (write.file = FALSE).')
  }

  message("Done.")

}