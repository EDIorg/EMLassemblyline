#' Create the annotations template
#'
#' @description  
#'     Annotate your metadata with terms from an ontology. Run this function 
#'     after all your metadata templates are complete, or if you're annotating
#'     a legacy EML record.
#'
#' @usage 
#'     template_annotations(
#'       path,
#'       data.path = path,
#'       data.table = NULL,
#'       other.entity = NULL,
#'       default.annotations = NULL,
#'       eml = NULL
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
#'     \code{other.entity = c('maps.zip', 'analysis.R')}).
#' @param default.annotations
#'     (data frame; optional) Default annotations to be used within the 
#'     annotations.txt template. EMLassemblyline presets are used unless 
#'     supplying your own (see details below).
#' @param eml
#'     (eml .xml file; optional) An EML .xml file located at \code{path}. 
#'     Use this argument if annotating legacy EML. Once you've completed 
#'     annotations.txt then run \code{EMLassemblyline::annotate_legacy_eml()} 
#'     to create an annotated version of your legacy EML.
#'
#' @return 
#'     \strong{annotations.txt} The tab delimited annotations template with
#'     the fields:
#'     \describe{
#'       \item{id}{A unique identifier to be assigned to the subject element
#'       when the EML is created, except in the case of ResponsibleParty 
#'       where a UUID is assigned to each recurring instance (e.g. creator, 
#'       contact) in order to maintain the "uniquenes" required by EML IDs.
#'       See details below for rules on creating IDs for annotations.txt.}
#'       \item{element}{The EML element being annotated as a relative path.
#'       See details below for rules on creating elements for annotations.txt.}
#'       \item{context}{The context of the subject being annotated (e.g. If the
#'       same column name is used in more than one of your data tables, you 
#'       will need to know which table it came from.).}
#'       \item{subject}{The subject being annotated.}
#'       \item{predicate_label}{The label of the predicate (a.k.a. property) 
#'       obtained from an ontology.}
#'       \item{predicate_uri}{The URI of the predicate (a.k.a. property) 
#'       obtained from an ontology.}
#'       \item{object_label}{The label of the object (a.k.a. value) obtained 
#'       from an ontology.}
#'       \item{object_uri}{The URI of the object (a.k.a. value) obtained 
#'       from an ontology.}
#'     }
#'     Only the predicate_label, predicate_uri, object_label, and 
#'     object_uri fields should be edited. If you want to add an annotation
#'     to any of the subjects, simply replicate the subjects row and modify
#'     the predicate and object fields.
#'     
#' @details 
#'     This function runs \code{EMLassemblyline::make_eml()} to get the EML
#'     R list object from which annotatable metadata is extracted, assigned 
#'     default predicate labels and URIs (in some cases object labels and URIs 
#'     as well), then written to annotations.txt. You will have to add object 
#'     labels and URIs from an ontology of your choice.
#'     
#'     If creating annotations.txt for legacy EML, then 
#'     \code{EMLassemblyline::make_eml()} isn't run, and the arguments 
#'     \code{data.path}, \code{data.table}, and \code{other.entity} are not 
#'     used.
#'     
#'     To set your own annotation defaults, copy the EMLassemblyline defaults 
#'     file 
#'     (\code{file.copy(from = system.file("/templates/annotation_defaults.txt", package = "EMLassemblyline"), to = path)}, 
#'     where path is where you want the file written to) then change the values 
#'     in the predicate_label, predicate_uri, object_label, and object_uri 
#'     fields, save the file, and then read this file into a data frame and 
#'     supply to the \code{default.annotations} argument of 
#'     \code{template_annotations()}.
#'     
#'     The id and element fields of annotations.txt are keys to locate the 
#'     subjects within the EML and to annotate them with the corresponding 
#'     predicate and object information. To create the id and element fields 
#'     use these rules:
#'     \describe{
#'       \item{dataset}{Use "/dataset" for both the id and element.}
#'       \item{dataTable}{Use \code{paste0("/", file.name)} for the id (where 
#'       file.name is the name of your dataTable file) and use "/dataTable" for
#'       the element.}
#'       \item{otherEntity}{Use \code{paste0("/", file.name)} for the id (where 
#'       file.name is the name of your otherEntity file) and use "/otherEntity" 
#'       for the element.}
#'       \item{attribute of a dataTable}{Use 
#'       \code{paste0("/", file.name, "/", column.name)} for the id (where 
#'       file.name is the name of your dataTable file and column.name is the 
#'       name of the column in the file.name) and use "/dataTable/attribute" 
#'       for the element.}
#'       \item{ResponsibleParty}{Use 
#'       \code{paste0("/", paste(first.name, middle.name, last.name, collapse = " "))}
#'       for the id (where first.name and middle.name are the persons first and 
#'       middle given names, respectively (more than one middle name works too
#'       ), and last.name is the persons surname).}
#'     }
#'     
#' @examples 
#' # Create annotations.txt from a set of completed metadata templates, 
#' # data tables and other entities
#' 
#' file.copy(
#'  from = system.file("/examples/pkg_260", package = "EMLassemblyline"),
#'  to = tempdir(),
#'  recursive = TRUE
#' )
#' 
#' unlink(
#'   paste0(tempdir(), "/pkg_260/metadata_templates/annotations.txt"),
#'   force = TRUE
#' )
#' 
#' template_annotations(
#'   path = paste0(tempdir(), "/pkg_260/metadata_templates"),
#'   data.path = paste0(tempdir(), "/pkg_260/data_objects"),
#'   data.table = c("nitrogen.csv", "decomp.csv"),
#'   other.entity = c("ancillary_data.zip", "processing_and_analysis.R")
#' )
#' 
#' df <- data.table::fread(
#'   paste0(tempdir(), "/pkg_260/metadata_templates/annotations.txt")
#' )
#' df
#' 
#' unlink(
#'   paste0(tempdir(), "/pkg_260"), 
#'   recursive = TRUE, 
#'   force = TRUE
#' )
#' 
#' # Create annotations.txt for an existing EML file
#' 
#' file.copy(
#'   from = system.file(
#'     "/examples/eml/edi.260.3.xml", 
#'     package = "EMLassemblyline"
#'   ),
#'   to = tempdir(),
#'   recursive = TRUE
#' )
#' 
#' template_annotations(
#'   path = tempdir(),
#'   eml = "edi.260.3.xml"
#' )
#' 
#' df <- data.table::fread(
#'   paste0(tempdir(), "/annotations.txt")
#' )
#' df
#' 
#' unlink(
#'   paste0(tempdir(), "/edi.260.3.xml"),
#'   force = TRUE
#' )
#' unlink(
#'   paste0(tempdir(), "/annotations.txt"),
#'   force = TRUE
#' )   
#' 
#' @export     
#'     

template_annotations <- function(
  path,
  data.path = path,
  data.table = NULL,
  other.entity = NULL,
  default.annotations = NULL,
  eml = NULL) {
  
  # Validate arguments --------------------------------------------------------
  
  validate_arguments(
    fun.name = 'template_annotations',
    fun.args = as.list(environment())
  )
  
  message('Templating annotations ...')
  
  # Set parameters ------------------------------------------------------------
  
  # Load default annotations to be added to the subjects found in eml
  
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
  
  # Initialize the annotations data frame that will be written to the 
  # annotations.txt template file. Each annotatable element will be
  # added to this data frame as they are gathered.
  
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
  
  # Create EML ----------------------------------------------------------------
  
  # Create the emld list object from which annotatable elements will be 
  # extracted. Create this object from either the metadata templates 
  # (i.e. EMLassemblyline::make_eml()) or from an EML file (i.e. 
  # EMLassemblyline::read_eml()).
  
  if (is.null(eml)) {
    
    # Read metadata templates
    
    x <- template_arguments(
      path = path,
      data.path = data.path,
      data.table = data.table,
      other.entity = other.entity
    )
    
    # Call make_eml() with minimal inputs to expedite processing
    
    for (i in c("additional_info", "taxonomic_coverage.txt", 
                "bounding_boxes.txt", "geographic_coverage.txt", 
                "taxonomicCoverage.xml")) {
      x$x$template[
        stringr::str_detect(names(x$x$template), i)
        ] <- NULL
    }
    
    x$x$template$keywords.txt$content <- x$x$template$keywords.txt$content[1, ]
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
    
  } else {
    
    # Read an EML file
    
    eml <- EMLassemblyline::read_eml(path, eml)
    
  }
  
  # Create annotations template -----------------------------------------------
  
  # A function for adding annotatable elements to the annotations data frame 
  # (anno)
  
  create_anno <- function(element) {
    
    # Helper functions --------------------------------------------------------
    
    # Add rows to anno
    
    append_anno <- function(id, element, context, subject) {
      for (i in 1:length(subject)) {
        anno <- rbind(
          anno,
          suppressWarnings(
            data.frame(
              id = id[i],
              element = element,
              context = context[i],
              subject = subject[i],
              dplyr::select(defs[element == defs$element, ], -"element"),
              stringsAsFactors = FALSE
            )
          )
        )
      }
      anno
    }
    
    # Create ids for anno
    
    create_id <- function(subject, context = NULL) {
      if (is.null(context)) {
        paste0("/", subject)
      } else if (!is.null(context)) {
        paste0("/", context, "/", subject)
      }
    }
    
    # Add ResponsibleParty to anno
    
    get_individualName <- function(x) {
      
      if (x == "eml$dataset$project$relatedProject"){
        
        lapply(
          eval(parse(text = x)),
          function(k) {
            lapply(
              k$personnel,
              function(m) {
                sub_i <- trimws(
                  paste(unlist(m$individualName), collapse = " ")
                )
                sub_i <- stringr::str_replace(sub_i, "[:blank:]{2,}", " ")
                anno <<- append_anno(
                  id = create_id(sub_i),
                  element = "/ResponsibleParty",
                  context = "dataset",
                  subject = sub_i
                )
              }
            )
          }
        )
        
      } else {
        
        lapply(
          eval(parse(text = x)),
          function(k) {
            sub_i <- trimws(
              paste(unlist(k$individualName), collapse = " ")
            )
            sub_i <- stringr::str_replace(sub_i, "[:blank:]{2,}", " ")
            anno <<- append_anno(
              id = create_id(sub_i),
              element = "/ResponsibleParty",
              context = "dataset",
              subject = sub_i
            )
          }
        )
        
      }
      
    }
    
    # Gather annotatable elements and set default annotations -----------------
    
    # Gather annotatable elements from the emld list object (eml). NOTE: 
    # Annotatable children elements of a parent are gathered if supported (e.g. 
    # When a dataTable is present so are its attributes.). Currently supported
    # elements are: dataset, dataTable, dataTable/attribute, otherEntity and
    # ResponsibleParty. Unique responsible parties are added to anno so the 
    # user doesn't have to expend superfluous effort annotating these multiple
    # times when recurring throughout the EML.
    
    if (element == "dataset") {
      
      anno <- append_anno(
        id = create_id(element),
        element = create_id(element),
        context = "eml",
        subject = element
      )

    } else if (element == "dataTable") {
      
      if (!is.null(eml$dataset$dataTable)) {
        lapply(
          eml$dataset$dataTable,
          function(k) {
            anno <<- append_anno(
              id = create_id(k$physical$objectName),
              element = "/dataTable",
              context = "dataset",
              subject = k$physical$objectName
            )
            lapply(
              k$attributeList$attribute,
              function(m) {
                anno <<- append_anno(
                  id = create_id(m$attributeName, k$physical$objectName),
                  element = "/dataTable/attribute",
                  context = k$physical$objectName,
                  subject = m$attributeName
                )
              }
            )
          }
        )
      }
      
    } else if (element == "otherEntity") {

      if (!is.null(eml$dataset$otherEntity)) {
        lapply(
          eml$dataset$otherEntity,
          function(k) {
            anno <<- append_anno(
              id = create_id(k$physical$objectName),
              element = "/otherEntity",
              context = "dataset",
              subject = k$physical$objectName
            )
          }
        )

      }
      
    } else if (element == "ResponsibleParty") {
      
      lapply(
        c(
          "eml$dataset$creator",
          "eml$dataset$contact",
          "eml$dataset$associatedParty",
          "eml$dataset$project$personnel",
          "eml$dataset$project$relatedProject"
        ),
        get_individualName
      )
      rp <- unique(anno[anno$element == "/ResponsibleParty", ])
      anno <- rbind(
        anno[anno$element != "/ResponsibleParty", ],
        rp[rp$id != "/", ]
      )

    }
    
    anno
    
  }
  
  # Run create_anno() for each supported element ------------------------------
  
  anno <- create_anno("dataset")
  anno <- create_anno("dataTable")
  anno <- create_anno("otherEntity")
  anno <- create_anno("ResponsibleParty")
  
  # Write to file -------------------------------------------------------------
  
  data.table::fwrite(
    x = anno,
    file = paste0(path, '/annotations.txt'),
    sep = "\t",
    quote = FALSE
  )
  
  message("Done.")
  
}





#' Read EML into an emld list object
#'
#' @description  
#'     This function wraps \code{EML::read_eml()} with a layer of quality 
#'     control ensuring the returned emld list object has the same structure 
#'     as output by \code{EMLassemblyline::make_eml()} and can be used in 
#'     EMLassemblyline workflows.
#'
#' @usage 
#'     read_eml(
#'       path,
#'       eml
#'     )
#'
#' @param path 
#'     (character) Path to the metadata template directory and where 
#'     annotations.txt will be written.
#' @param eml
#'     (file) An EML .xml file located at \code{path}.
#'
#' @return 
#'     An emld list object as similarly created by \code{EML::read_eml()}.
#'     
#' @details
#'     When representing EML in the emld list structure, nodes that have 
#'     1 or more children are structured as a list of unnamed lists 
#'     (e.g. \code{list(dataTable = list("dataTable1", "dataTable2"))}) and
#'     this is the structure output by EMLassemblyline::make_eml(). However,
#'     EML::read_eml() removes the unnamed list when the node has only 1 child
#'     (e.g. \code{list(dataTable = "dataTable1")}), thereby breaking 
#'     EMLassemblyline code using \code{lapply()} to parse such nodes. 
#'     Currently this QC is selectively applied to annotatable nodes targeted
#'     by \code{template_annotations()}:
#'     
#'     \describe{
#'       \item{eml/dataset/dataTable}{}
#'       \item{eml/dataset/dataTable/attributeList/attribute}{}
#'       \item{eml/dataset/otherEntity}{}
#'       \item{eml/dataset/creator}{}
#'       \item{eml/dataset/contact}{}
#'       \item{eml/dataset/associatedParty}{}
#'       \item{eml/dataset/project/personnel}{}
#'       \item{eml/dataset/project/relatedProject}{}
#'       \item{eml/dataset/project/relatedProject/personnel}{}
#'     }
#'     
#'     Extending support to all nodes capable of having 1 or more children is
#'     welcomed.
#'     
#' @examples 
#' eml <- EMLassemblyline::read_eml(
#'   path = system.file(
#'     "/examples/eml", 
#'     package = "EMLassemblyline"
#'   ),
#'   eml = "edi.260.3.xml"
#' )
#' 
#' 
read_eml <- function(path, eml) {
  
  # Create the emld list object
  
  eml <- EML::read_eml(
    paste0(path, "/", eml)
  )
  
  # A helper function to wrap target nodes in list(). There are two 
  # implementations, one for target nodes and a second for nested target nodes.
  #
  # Arguments:
  # eml = emld list object
  # path.parent = path of the parent node using the "$" subsetting character
  # path.child = path of child node
  
  list_it <- function(eml, path.parent, path.child = NULL) {

    if (is.null(path.child)) {

      e <- eval(parse(text = path.parent))
      if ((length(e) > 1) & (!is.null(names(e)))) {
        eval(parse(text = paste0(path.parent, " <- list(e)")))
      }

    } else if (!is.null(path.child)) {

      lapply(
        seq_along(eval(parse(text = path.parent))),
        function(k) {
          if ((length(eval(parse(text = paste0(path.parent, "[[", k, "]]", path.child)))) > 1) & 
              (!is.null(names(eval(parse(text = paste0(path.parent, "[[", k, "]]", path.child))))))) {
            
            eval(
              parse(
                text = paste0(
                  path.parent, "[[", k, "]]", path.child, 
                  " <<- list(", 
                  paste0(path.parent, "[[", k, "]]", path.child), ")"
                )
              )
            )
            
          }
        }
      )

    }

    eml

  }
  
  # Fix the emld list object
  
  eml <- list_it(eml, "eml$dataset$dataTable")
  eml <- list_it(eml, "eml$dataset$otherEntity")
  eml <- list_it(eml, "eml$dataset$creator")
  eml <- list_it(eml, "eml$dataset$contact")
  eml <- list_it(eml, "eml$dataset$associatedParty")
  eml <- list_it(eml, "eml$dataset$project$personnel")
  eml <- list_it(eml, "eml$dataset$project$relatedProject")
  eml <- list_it(eml, "eml$dataset$project$relatedProject", "$personnel")
  eml <- list_it(eml, "eml$dataset$dataTable", "$attributeList$attribute")
  
  eml
  
}
