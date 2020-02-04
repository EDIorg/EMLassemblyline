#' Create the annotations template
#'
#' @description  
#'     Annotate your metadata with terms from an ontology. Run this function 
#'     after all your metadata templates are complete
#'
#' @usage 
#'     template_annotations(
#'       path,
#'       data.path = path,
#'       data.table = NULL,
#'       other.entity = NULL
#'     )
#'
#' @param path 
#'     (character) Path to the metadata template directory and where 
#'     annotations.txt will be written.
#' @param data.path
#'     (character; optional) Path to the data directory. Defaults to 
#'     \code{path}.
#' @param data.table
#'     (character; optional) Table name. If more than one, then supply as a 
#'     vector of character strings (e.g. 
#'     \code{data.table = c('nitrogen.csv', 'decomp.csv')}).
#' @param other.entity
#'     (character; optional) Other entity name. If more than one, then supply 
#'     as a vector of character strings (e.g. 
#'     \code{other.entity = c('maps.zip', 'analysis_script.R')}).
#'
#' @return 
#'     \strong{annotations.txt} The tab delimited annotations template.
#'     
#' @details 
#'     This function extracts annotatable metadata from the template files
#'     and assigns default annotations for the predicate labels and URIs. The 
#'     user manually assigns the object labels and URIs.
#'     
#'     \code{data.table} and \code{other.entity} are added to annotations.txt 
#'     and assigned UUIDs.
#'     
#'     Unique persons within personnel.txt are added to annotations.txt and 
#'     assigned UUIDs, which are also added to the \code{id} field of 
#'     personnel.txt.
#'     
#' @examples 
#' # Add completed metadata templates to a temporary directory
#' file.copy(
#'  from = system.file("/examples/pkg_260/metadata_templates", package = "EMLassemblyline"),
#'  to = tempdir(),
#'  recursive = TRUE
#' )
#' unlink("./metadata_templates/annotations.txt", force = TRUE)
#' setwd(paste0(tempdir(), "/metadata_templates"))
#' 
#' # Create the annotations template
#' template_annotations(
#'   path = "."
#' )
#' 
#' # View directory contents
#' dir(".")
#' 
#' # View template contents
#' df <- data.table::fread("./annotations.txt")
#' df
#' 
#' # Remove the temporary directory
#' setwd(tempdir())
#' unlink("./metadata_templates", recursive = TRUE, force = TRUE)
#'     
#' @export     
#'     

template_annotations <- function(
  path,
  data.path = path,
  data.table = NULL,
  other.entity = NULL) {
  
  # Validate arguments --------------------------------------------------------
  
  validate_arguments(
    fun.name = 'template_annotations',
    fun.args = as.list(environment())
  )
  
  message('Templating annotations ...')
  
  # Read metadata templates ---------------------------------------------------
  
  x <- template_arguments(
    path = path, 
    data.path = data.path,
    data.table = data.table,
    other.entity = other.entity
  )$x
  
  # Stop if annotations.txt already exists
  
  if (any(names(x$template) == "annotations.txt")) {
    stop(
      paste0(
        "annotations.txt already exists. To create a new annotations ",
        "template, remove this one from 'path'."
      ), 
      call. = FALSE
    )
  }
  
  # Set parameters ------------------------------------------------------------
  
  # Load default annotation definitions
  
  defs <- as.data.frame(
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
  
  # Initialize the annotations.txt data frame
  
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
  
  # Gather subjects and annotate ----------------------------------------------
  
  annotate_element <- function(element) {
    
    # Gather subjects
    
    if (element == "dataset") {
      s <- "dataset"
    } else if (element == "dataTable") {
      s <- data.table
    } else if (element == "otherEntity") {
      s <- other.entity
    } else if (element == "attribute") {
      s <- unlist(
        lapply(
          names(x$template)[
            stringr::str_detect(
              names(x$template), 
              "attributes_(?!dataset).*.txt")
            ],
          function(k) {
            x$template[[k]]$content$attributeName
          }
        )
      )
    } else if (element == "unit") {
      s <- anno$subject[anno$element == "attribute"]
    } else if (element == "individualName") {
      s <- unique(
        paste(
          x$template$personnel.txt$content$givenName,
          x$template$personnel.txt$content$middleInitial,
          x$template$personnel.txt$content$surName
        )
      )
    } else if (element == "organizationName") {
      s <- anno$subject[anno$element == "individualName"]
    }
    
    # Add context and default annotations from annotation_characteristics.txt
    
    df <- suppressWarnings(
      data.frame(
        id = replicate(
          n = length(s),
          expr = uuid::UUIDgenerate(use.time = TRUE)
        ),
        dplyr::select(
          defs[match(element, defs$element), ],
          "element",
          "context"
        ),
        subject = s,
        dplyr::select(
          defs[match(element, defs$element), ],
          -"element",
          -"context",
          -"subject",
          -"r_list_path"
        ),
        stringsAsFactors = FALSE
      )
    )
    
    # Update IDs and context (some context is defined by metadata values)
    
    if (element == "attribute") {
      
      df$id <- unlist(
        lapply(
          names(x$template)[
            stringr::str_detect(
              names(x$template), 
              "attributes_(?!dataset).*.txt"
            )
          ],
          function(k) {
            x$template[[k]]$content$id
          }
        )
      )
      
      df$context <- unlist(
        mapply(
          function(i, j) {
            rep(j, length(x$template[[i]]$content$attributeName))
          },
          i = names(x$template)[
            stringr::str_detect(
              names(x$template), 
              "attributes_(?!dataset).*.txt"
            )
          ],
          j = names(x$data.table),
          USE.NAMES = FALSE
        )
      )
      
    } else if (element == "unit") {
      
      df$id <- anno$id[anno$element == "attribute"]
      df$context <- anno$context[anno$element == "attribute"]
      
    } else if (element == "individualName") {
      
      use_i <- match(
        paste(
          x$template$personnel.txt$content$givenName,
          x$template$personnel.txt$content$middleInitial,
          x$template$personnel.txt$content$surName
        ),
        df$subject
      )
      x$template$personnel.txt$content$id <- df$id[use_i]
      
      data.table::fwrite(
        x = x$template$personnel.txt$content,
        file = paste0(path, '/personnel.txt'),
        sep = "\t",
        quote = FALSE
      )
      
    } else if (element == "organizationName") {
      
      df$id <- anno$id[anno$element == "individualName"]
      
    }
    
    # Return object
    
    rbind(anno, df)
    
  }
  
  anno <- annotate_element("dataset")
  if (!is.null(data.table)) {
    anno <- annotate_element("dataTable")
    anno <- annotate_element("attribute")
    anno <- annotate_element("unit")
  }
  if (!is.null(other.entity)) {
    anno <- annotate_element("otherEntity")
  }
  if (nrow(x$template$personnel.txt$content) != 0) {
    anno <- annotate_element("individualName")
    anno <- annotate_element("organizationName")
  }
  
  # Remove double white spaces introduced when collapsing individualName so 
  # downstream matching isn't affected
  
  anno$subject <- trimws(anno$subject)
  anno$subject <- stringr::str_replace(anno$subject, "[:blank:]{2,}", " ")
  
  # Write annotations.txt -----------------------------------------------------
  
  data.table::fwrite(
    x = anno,
    file = paste0(path, '/annotations.txt'),
    sep = "\t",
    quote = FALSE
  )
  
  message("Done.")
  
}
