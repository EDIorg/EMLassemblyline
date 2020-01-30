#' Create annotations template
#'
#' @description  
#'     Use this function to extract annotatable elements from completed 
#'     metadata templates.
#'     \href{https://ediorg.github.io/EMLassemblyline/articles/edit_metadata_templates.html}{Instructions for editing this template.}
#'
#' @usage 
#'     template_annotations(
#'       path,
#'       write.file = TRUE,
#'       x = NULL
#'     )
#'
#' @param path 
#'     (character) Path to the metadata template directory.
#' @param write.file
#'     (logical; optional) Whether to write the template file.
#' @param x
#'     (named list; optional) Alternative input to 
#'     \code{template_annotations()}. Use \code{template_arguments()} 
#'     to create \code{x}.
#'
#' @return 
#'     \strong{annotations.txt} The tab delimited annotations template. This 
#'     file is written to \code{path} unless using \code{x}, in which case the 
#'     template is added to \strong{/x/templates/annotations.txt}.
#'     
#' @details 
#'     An existing annotations template will not be overwritten by subsequent 
#'     calls to \code{template_annotations()}.
#'     
#' @examples 
#' # Initialize data package directory for template_annotations()
#' file.copy(
#'  from = system.file('/examples/pkg_260', package = 'EMLassemblyline'),
#'  to = tempdir(),
#'  recursive = TRUE
#' )
#' 
#' # Set working directory
#' setwd(paste0(tempdir(), '/pkg_260'))
#' 
#' # View directory contents (NOTE: annotations.txt dosn't exist)
#' dir('./metadata_templates')
#' 
#' # Template annotations
#' template_annotations(
#'   path = './metadata_templates'
#' )
#' 
#' # View directory contents (NOTE: annotations.txt exists)
#' dir('./metadata_templates')
#' 
#' # View template contents
#' df <- data.table::fread(./metadata_templates/annotations.txt)
#' df
#' 
#' # Rerunning template_annotations() does not overwrite files
#' template_annotations(
#'   path = './metadata_templates'
#' )
#' 
#' # Clean up
#' unlink('.', recursive = TRUE)
#'     
#' @export     
#'     

template_annotations <- function(
  path,
  write.file = TRUE,
  x = NULL) {
  
  message('Templating annotations ...')
  
  # Validate arguments --------------------------------------------------------
  
  # Validate path usage before passing arguments to validate_arguments()
  # When not using x, inputs are expected from path and data.path. When using x, 
  # only data.path is required.
  if (is.null(x) & missing(path)){
    stop('Input argument "path" is missing.')
  } else if (!is.null(x) & missing(path)){
    path <- NULL
    if (missing(data.path)){
      data.path <- NULL
    }
  }
  
  # Pass remaining arguments to validate_arguments()
  validate_arguments(
    fun.name = 'template_annotations',
    fun.args = as.list(environment())
  )
  
  # Read metadata templates and data ------------------------------------------
  # Create x if it doesn't exist and indicate it has been created (i.e. 
  # "data_read_2_x <- TRUE").
  
  if (is.null(x)) {
    x <- template_arguments(path = path)
    x <- x$x
    data_read_2_x <- TRUE
  }
  
  # Parameterize --------------------------------------------------------------
  
  # Load annotation search parameters
  
  attr_anno <- as.data.frame(
    data.table::fread(
      file = system.file(
        '/templates/annotation_characteristics.txt',
        package = 'EMLassemblyline'
      ),
      colClasses = rep(
        "character",
        max(
          utils::count.fields(
            system.file(
              '/templates/annotation_characteristics.txt',
              package = 'EMLassemblyline'
            ),
            sep = "\t"
          )
        )
      ),
      fill = TRUE,
      blank.lines.skip = TRUE
    )
  )
  
  # Initialize annotations.txt
  
  anno <- as.data.frame(
    data.table::fread(
      file = system.file(
        '/templates/annotations.txt',
        package = 'EMLassemblyline'
      ),
      colClasses = rep(
        "character",
        max(
          utils::count.fields(
            system.file(
              '/templates/annotations.txt',
              package = 'EMLassemblyline'
            ),
            sep = "\t"
          )
        )
      ),
      fill = TRUE,
      blank.lines.skip = TRUE
    )
  )
  
  # Set default annotations ---------------------------------------------------

  # dataset
  
  e <- "dataset"
  use_i <- attr_anno$element == e
  anno <- rbind(
    anno,
    c(
      id = uuid::UUIDgenerate(use.time = TRUE),
      dplyr::select(attr_anno, -"r_list_path")[use_i, ]
    )
  )

  # entities
  
  collect_entity <- function(element) {
    use_i <- x$template$attributes_dataset.txt$content$element == element
    if (any(use_i)) {
      rbind(
        anno,
        suppressWarnings(
          data.frame(
            id = x$template$attributes_dataset.txt$content$id[use_i],
            dplyr::select(attr_anno, "element", "context")[attr_anno$element == element, ],
            value = x$template$attributes_dataset.txt$content$value[use_i],
            dplyr::select(attr_anno, -"r_list_path", -"element", -"context", -"value")[attr_anno$element == element, ],
            stringsAsFactors = FALSE
          )
        )
      )
    } else {
      anno
    }
  }
  
  anno <- collect_entity("dataTable")
  anno <- collect_entity("otherEntity")
  anno <- collect_entity("spatialRaster")
  anno <- collect_entity("spatialVector")
  
  # attributes
  
  collect_attributes <- function(element) {
    use_i <- x$template$attributes_dataset.txt$content$element == element
    if (any(use_i)) {
      f <- names(x$template)[
        stringr::str_detect(names(x$template), "attributes_[^dataset].*.txt")
      ]
      for (i in f) {
        anno <- rbind(
          anno,
          suppressWarnings(
            data.frame(
              id = x$template[[i]]$content$id,
              element = dplyr::select(attr_anno, "element")[attr_anno$element == "attribute", ],
              context = i,
              value = x$template[[i]]$content$attributeName,
              dplyr::select(attr_anno, -"r_list_path", -"element", -"context", -"value")[attr_anno$element == "attribute", ],
              stringsAsFactors = FALSE
            )
          )
        )
      }
      anno
    } else {
      anno
    }
  }
  
  anno <- collect_attributes("dataTable")
  
  # units
  
  collect_units <- function(element) {
    use_i <- x$template$attributes_dataset.txt$content$element == element
    if (any(use_i)) {
      f <- names(x$template)[
        stringr::str_detect(names(x$template), "attributes_[^dataset].*.txt")
      ]
      for (i in f) {
        anno <- rbind(
          anno,
          suppressWarnings(
            data.frame(
              id = x$template[[i]]$content$id,
              element = dplyr::select(attr_anno, "element")[attr_anno$element == "unit", ],
              context = i,
              value = x$template[[i]]$content$attributeName,
              dplyr::select(attr_anno, -"r_list_path", -"element", -"context", -"value")[attr_anno$element == "unit", ],
              stringsAsFactors = FALSE
            )
          )
        )
      }
      anno
    } else {
      anno
    }
  }
  
  anno <- collect_units("dataTable")
  
  # individualName
  # Assign a UUID to each unique individual within annotations.txt and 
  # personnel.txt.
  
  collect_persons <- function(element) {
    if (nrow(x$template$personnel.txt$content) != 0) {
      p <- unique(
        paste(
          x$template$personnel.txt$content$givenName,
          x$template$personnel.txt$content$middleInitial,
          x$template$personnel.txt$content$surName
        )
      )
      rbind(
        anno,
        suppressWarnings(
          data.frame(
            id = replicate(
              n = length(p),
              expr = uuid::UUIDgenerate(use.time = TRUE)
            ),
            dplyr::select(attr_anno, "element", "context")[attr_anno$element == element, ],
            value = p,
            dplyr::select(attr_anno, -"r_list_path", -"element", -"context", -"value")[attr_anno$element == element, ],
            stringsAsFactors = FALSE
          )
        )
      )
    } else {
      anno
    }
  }
  
  anno <- collect_persons("individualName")
  
  # Add IDs to personnel.txt
  
  # FIXME: Sort anno so attribute description and unit are listed together
  
  # Write annotations.txt -----------------------------------------------------

  # Return --------------------------------------------------------------------
  
  if ((!isTRUE(write.file)) & is.null(x)){
    message('No templates were written to file (write.file = FALSE).')
  }
  
  message("Done.")
  
  if (!exists('data_read_2_x')){
    return(x)
  }
  
}


