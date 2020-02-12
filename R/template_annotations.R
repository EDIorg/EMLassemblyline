#' Create the annotations template
#'
#' @description  
#'     Annotate your metadata with terms from an ontology. Run this function 
#'     after all your metadata templates are complete.
#'
#' @usage 
#'     template_annotations(
#'       path,
#'       data.path = path,
#'       data.table = NULL,
#'       other.entity = NULL,
#'       default.annotations = NULL
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
#' @param default.annotations
#'     (data frame; optional) Default annotations to be used within the 
#'     annotations.txt template. EMLassemblyline defaults are used unless 
#'     supplying your own (see details).
#'
#' @return 
#'     \strong{annotations.txt} The tab delimited annotations template with
#'     the fields:
#'     \describe{
#'       \item{id}{A unique identifier assigned to the elements within the EML,
#'       except in the case of ResponsibleParty sub-trees where a UUID is 
#'       assigned to each corresponding occurence within the EML (e.g. under
#'       creator, contact)}.
#'       \item{element}{The EML element being annotated as a relative path to 
#'       the element.}
#'       \item{context}{The context of the subject to be annotated (e.g. If the
#'       same field name is used in more than one data table, you will need 
#'       to know which table it came from. In this example the file name is 
#'       the context.)}
#'       \item{subject}{The subject being annotated.}
#'       \item{predicate_label}{The label of the predicate (a.k.a. property) 
#'       obtained from an ontology.}
#'       \item{predicate_uri}{The URI of the predicate (a.k.a. property) 
#'       obtained from an ontology.}
#'       \item{object_label}{The label of the object (a.k.a. value) obtained 
#'       from an ontology.}
#'       \item{predicate_uri}{The URI of the object (a.k.a. value) obtained 
#'       from an ontology.}
#'     
#'     }
#'     
#' @details 
#'     This function extracts annotatable metadata from the template files
#'     and assigns default annotations for the predicate labels and URIs. You 
#'     manually select object labels and URIs from the ontology of your choice
#'     and add them to annotations.txt.
#'     
#'     \code{data.table} and \code{other.entity} are added to annotations.txt.
#'     
#'     To set your own annotation defaults, copy the EMLassemblyline defaults file 
#'     (\code{file.copy(from = system.file("/templates/annotation_defaults.txt", package = "EMLassemblyline"), to = path)}, where path is where you want the file written to)
#'     then change the values in the predicate_label, predicate_uri, 
#'     object_label, and object_uri fields, save the file, and then read this 
#'     file into a data frame and supply to the \code{default.annotations} 
#'     argument when calling \code{template_annotations()}.
#'     
#'     
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
  other.entity = NULL,
  default.annotations = NULL) {
  
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
  )
  
  # Stop if annotations.txt already exists
  
  if (any(names(x$x$template) == "annotations.txt")) {
    stop(
      paste0(
        "annotations.txt already exists. To create a new annotations ",
        "template, remove this one from 'path'."
      ), 
      call. = FALSE
    )
  }
  
  # Create EML ----------------------------------------------------------------
  # Run make_eml() to get the EML list object from which annotatable metadata 
  # will be extracted.
  
  # Remove non-required templates and reduce template content to expedite 
  # make_eml().
  
  for (i in c("additional_info", "taxonomic_coverage.txt", 
              "bounding_boxes.txt", "geographic_coverage.txt", 
              "taxonomicCoverage.xml")) {
    x$x$template[
      stringr::str_detect(names(x$x$template), i)
      ] <- NULL
  }
  
  x$x$template$keywords.txt$content <- x$x$template$keywords.txt$content[1, ]
  
  # Assign argument values and call make_eml()
  
  x$path <- path
  x$data.path <- data.path
  x$dataset.title <- "Placeholder"
  x$data.table <- data.table
  x$other.entity <- other.entity
  if (!is.null(data.table)) {
    x$data.table.name <- rep("Placeholder", length(data.table))
    x$data.table.description <- rep("Placeholder", length(data.table))
  }
  if (!is.null(other.entity)) {
    x$other.entity.name <- rep("Placeholder", length(other.entity))
    x$other.entity.description <- rep("Placeholder", length(other.entity))
  }
  x$temporal.coverage <- c('2014-05-01', '2015-10-31')
  x$geographic.coordinates <- c("45", "-123", "44", "-124")
  x$geographic.description <- "Placeholder"
  x$maintenance.description <- "Placeholder"
  x$user.id <- "someuserid"
  x$user.domain <- "LTER"
  x$package.id <- 'placeholder'
  x$return.obj <- TRUE
  x$write.file <- FALSE
  
  eml <- suppressMessages(
    suppressWarnings(
      do.call(
        make_eml, 
        x[names(x) %in% names(formals(make_eml))]
      )
    )
  )

  # Set parameters ------------------------------------------------------------
  
  # Load default annotations
  
  if (!is.null(default.annotations)) {
    
    defs <- default.annotations
    
  } else {
    
    defs <- as.data.frame(
      data.table::fread(
        file = system.file(
          '/templates/annotation_defaults.txt',
          package = 'EMLassemblyline'
        ),
        colClasses = rep(
          "character",
          max(
            utils::count.fields(
              system.file(
                '/templates/annotation_defaults.txt',
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
    
  }

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
  
  # Gather annotatable elements and set defaults ------------------------------
  
  annotate_element <- function(element) {
    
    # Gather subjects
    
    if (element == "dataset") {
      
      s <- "dataset"
      ctxt <- "eml"
      e <- "/dataset"
      id <- "/dataset"
      
    } else if (element == "dataTable") {
      
      s <- unlist(
        lapply(
          eml$dataset$dataTable,
          function(k) {
            k$physical$objectName
          }
        )
      )
      ctxt <- "dataset"
      e <- "/dataTable"
      id <- paste0("/", s)
      
    } else if (element == "otherEntity") {
      
      s <- unlist(
        lapply(
          eml$dataset$otherEntity,
          function(k) {
            k$physical$objectName
          }
        )
      )
      ctxt <- "dataset"
      e <- "/otherEntity"
      id <- paste0("/", s)
      
    } else if (element == "attribute") {
      
      output <- data.table::rbindlist(
        lapply(
          eml$dataset$dataTable,
          function(k) {
            suppressWarnings(
              data.frame(
                context = k$physical$objectName,
                subject = unlist(
                  lapply(
                    k$attributeList$attribute,
                    function(m) {
                      m$attributeName
                    }
                  )
                ), 
                stringsAsFactors = FALSE
              )
            )
          }
        )
      )
      s <- output$subject
      ctxt <- output$context
      e <- rep("/dataTable/attribute", length(s))
      id <- paste0("/", ctxt, "/", s)
      
    } else if (element == "unit") {
      
      s <- anno$subject[anno$element == "/dataTable/attribute"]
      ctxt <- anno$context[anno$element == "/dataTable/attribute"]
      e <- rep("/dataTable/attribute/unit", length(s))
      id <- anno$id[anno$element == "/dataTable/attribute"]
      
    } else if (element == "individualName") {
      
      get_individualName <- function(x) {
        unlist(
          lapply(
            eval(parse(text = x)),
            function(k) {
              sub_i <- trimws(paste(unlist(k$individualName), collapse = " "))
              stringr::str_replace(sub_i, "[:blank:]{2,}", " ")
            }
          )
        )
      }
      
      s <- unique(
        unlist(
          lapply(
            c("eml$dataset$creator", "eml$dataset$contact", 
              "eml$dataset$associatedParty", "eml$dataset$project$personnel",
              "eml$dataset$project$relatedProject"),
            get_individualName
          )
        )
      )
      s <- s[s != ""]
      
      ctxt <- rep("dataset", length(s))
      e <- rep("/ResponsibleParty/individualName", length(s))
      id <- paste0("/", s)
      
    } else if (element == "organizationName") {
      
      s <- anno$subject[anno$element == "/ResponsibleParty/individualName"]
      ctxt <- anno$context[anno$element == "/ResponsibleParty/individualName"]
      e <- rep("/ResponsibleParty/organizationName", length(s))
      id <- anno$id[anno$element == "/ResponsibleParty/individualName"]
      
    }
    
    # Add context and default annotations from annotation_defaults.txt
    
    df <- suppressWarnings(
      data.frame(
        id = id,
        element = e,
        context = ctxt,
        subject = s,
        dplyr::select(defs[match(e, defs$element), ], -"element"),
        stringsAsFactors = FALSE
      )
    )
    
    rbind(anno, df)
    
  }
  
  anno <- annotate_element("dataset")
  
  # FIXME: Group these 3 function calls into 1 where the annotation_defaults.txt
  # parameterizes what elements are called for.
  if (!is.null(data.table)) {
    anno <- annotate_element("dataTable")
    anno <- annotate_element("attribute")
    anno <- annotate_element("unit")
  }
  if (!is.null(other.entity)) {
    anno <- annotate_element("otherEntity")
  }
  anno <- annotate_element("individualName")
  anno <- annotate_element("organizationName")
  browser()
  
  # Write annotations.txt -----------------------------------------------------
  
  data.table::fwrite(
    x = anno,
    file = paste0(path, '/annotations.txt'),
    sep = "\t",
    quote = FALSE
  )
  
  message("Done.")
  
}
