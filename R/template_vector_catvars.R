#' Describe categorical variables associated with a vector file
#'
#' @description Describes the categorical variable represented in a vector file.
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
#' \item{catvars_*.txt}{Columns:
#'     \itemize{
#'     \item{attributeNames: Variable name}
#'     \item{code: Categorical variable}
#'     \item{definition: Definition of categorical variable}
#'     }
#' }
#'
#' @examples 
#' \dontrun{
#' # Set working directory
#' setwd("/Users/me/Documents/data_packages/pkg_260")
#' 
#' # For files containing categorical variables as classified in the vector attributes template
#' template_vector_catvars(
#'   path = "./metadata_templates",
#'   data.path = "./data_objects",
#'   vector.file = setNames(
#'     c("shapefile_test", "geojson_test_file.GeoJSON"), 
#'     c("attributes_shapefile_test.txt", "attributes_geojson_test_file.txt")
#'   ))
#' }
#' 
#' @export
#'
template_vector_catvars <- function(
  path = NULL,
  data.path = path,
  vector.file = NULL,
  write.file = TRUE) {

  message('Templating vector categorical variables ...')

  # Validate arguments --------------------------------------------------------

  validate_arguments(
    fun.name = 'template_vector_catvars',
    fun.args = as.list(environment()))

  # If files are absent ...

  if (is.null(vector.file)){

    stop('Input argument "vector.file" is missing.')

  }

  # Extract categorical variables ---------------------------------------------

  # Categorical variables are classified in each vector attribute template.
  # For each categorical variable found, extract unique codes, except for
  # declared missing value codes, and return in a long data frame.

  r <- lapply(
    seq_along(vector.file),
    function(i, vector.file) {

      # Get components
      vectorFile <- data.frame(terra::vect(paste0(data.path, "/", vector.file[i])))

      attributes <- as.data.frame(
        data.table::fread(
          file = paste0(path, "/", names(vector.file)[i]),
          fill = TRUE,
          blank.lines.skip = TRUE,
          sep = "\t",
          colClasses = list(
            character = 1:utils::count.fields(paste0(path, "/", names(vector.file)[i]), sep = "\t")[1])))

      # Do not continue unless data and attributes have made it this far
      if (is.null(vectorFile) | is.null(attributes)) {
        return(NULL)
      }

      categorical_variables <- attributes$attributeName[
        attributes$class == "categorical"]

      missing_value_codes <- dplyr::select(
        attributes, attributeName, missingValueCode)

      categorical_variables_file_name <- stringr::str_replace(
        names(vector.file)[i],
        "attributes_",
        "catvars_")

      # Continue if categorical variables exist for this data table and if
      # a categorical variables template doesn't already exist

      if (length(categorical_variables) == 0) {
        message("No categorical variables found.")
      } else {

        # if (categorical_variables_file_name %in% names(x$template)) {
        if (file.exists(paste0(path, "/", categorical_variables_file_name))) {
          message(categorical_variables_file_name, " already exists!")
        } else {
          message(categorical_variables_file_name)

          # Compile components for the categorical variables template
          catvars <- dplyr::select(vectorFile, categorical_variables)
          catvars <- tidyr::gather(catvars, "attributeName", "code")
          catvars <- dplyr::distinct(catvars)
          catvars <- dplyr::right_join(missing_value_codes, catvars, by = "attributeName")

          # Remove missing value codes listed in the table attributes template
          # since these will be listed in the EML metadata separately. NOTE:
          # Because EAL templates use "" instead of NA, all "" from the template
          # are converted to NA to facilitate matching.

          use_i <- apply(
            catvars,
            1,
            function(x) {
              if (x[["missingValueCode"]] == "NA") {
                x[["missingValueCode"]] <- NA_character_
              }
              missing_value_code <- x[["missingValueCode"]] %in% x[["code"]]
              return(missing_value_code)
            })

          catvars <- catvars[!use_i, ]

          # Tranform contents into the categorical variables template format

          catvars$definition <- ""
          catvars <- dplyr::select(catvars, -missingValueCode)

          # Order results

          catvars <- dplyr::arrange(catvars, attributeName, code)

          # Encode extracted metadata in UTF-8

          catvars$attributeName <- enc2utf8(as.character(catvars$attributeName))
          catvars$code <- enc2utf8(as.character(catvars$code))

          # List under "content" to accord with structure returned by
          # template_arguments()

          catvars <- list(content = catvars)

          return(catvars)

        }
      }
    },
    vector.file = vector.file)

  names(r) <- stringr::str_replace(
    names(vector.file),
    "attributes_",
    "catvars_")

  # Write to file -------------------------------------------------------------

  if (write.file) {
    for (i in names(r)) {
      if (!is.null(r[[i]])) {
        data.table::fwrite(
          x = r[[i]]$content,
          file = paste0(path, "/", enc2utf8(i)),
          sep = "\t",
          quote = FALSE,
          na = "NA")
      }
    }
  }

  message("Done.")

  # Return --------------------------------------------------------------------

  return(r)
}
