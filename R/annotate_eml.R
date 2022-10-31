#' Annotate an EML file (.xml) or an emld list object
#'
#' @param annotations
#'     (character; data frame) Path or data frame of the annotations.txt 
#'     template for annotating \code{eml.in}. Create annotations.txt with 
#'     \code{template_annotations()}.
#' @param eml.in
#'     (EML; emld list) Path to, or emld list of, the EML file to be annotated. 
#'     Accepted versions of the emld list object can be created by
#'     \code{EMLassemblyline::read_eml()} or \code{make_eml()}.
#' @param eml.out
#'     (character) Path of the annotated EML file to be written.
#'
#' @return
#'     (EML; emld list) If the input to \code{eml.in} is an EML file then the
#'     output will be an EML file. The emld list object is always returned.
#'     
#' @note 
#'     All annotated elements are assigned ids and their annotations are 
#'     placed both immediately under the parent element (subject) when 
#'     supported and within the /eml/annotations node through id+reference 
#'     pairs. This redundant approach supports variation in where EML metadata 
#'     consumers harvest this information and supports annotation of EML 
#'     elements requiring id+references.
#'
#' @examples
#' 
#' # Annotate an EML file --------------------------------------------------------
#' #
#' # Annotate an existing EML file (edi.260.3.xml) and write a new version 
#' # (edi.260.4.xml)
#' 
#' eml <- annotate_eml(
#'  annotations = system.file("/examples/pkg_260/metadata_templates_overflow/annotations.txt", package = "EMLassemblyline"),
#'  eml.in = system.file("/examples/eml/edi.260.3.xml", package = "EMLassemblyline"),
#'  eml.out = paste0(tempdir(), "/edi.260.4.xml")
#' )
#' eml
#' 
#' # Remove the EML files from the temporary directory
#' 
#' unlink(paste0(tempdir(), "/edi.260.3.xml"))
#' unlink(paste0(tempdir(), "/edi.260.4.xml"))
#'
#' @export
#'
annotate_eml <- function(
  annotations = NULL, 
  eml.in = NULL, 
  eml.out = NULL) {
  
  # Validate arguments --------------------------------------------------------
  
  validate_arguments(
    fun.name = 'annotate_eml',
    fun.args = as.list(environment()))
  
  if (is.character(eml.in) | is.list(eml.in)) {
    message("Annotating EML ...")
  }
  
  # Read metadata templates ---------------------------------------------------
  
  if (is.character(annotations)) {
    x <- template_arguments(
      path = dirname(annotations)
    )$x
  } else if (is.data.frame(annotations)) {
    x <- template_arguments()$x
    x$template$annotations.txt$content <- annotations
  }
  
  # Validate templates --------------------------------------------------------
  
  # Don't rerun validate_templates() if it has already been run by make_eml().
  # Validation can take minutes.
  
  if (!any(stringr::str_detect(deparse(sys.calls()), "make_eml"))) {
    x <- validate_templates("annotate_eml", x)
  }
  
  # Continue if annotations.txt hasn't been dropped after validation
  
  if (!is.null(x$template$annotations.txt)) {
    
    # Set parameters ----------------------------------------------------------
    
    # Read EML
    
    if (is.character(eml.in)) {
      eml <- EMLassemblyline::read_eml(
        path = dirname(eml.in),
        eml = basename(eml.in))
    } else {
      eml <- eml.in
    }
    
    # Replace blank cells with NAs and remove incomplete cases to make 
    # annotations data easier to use and to preempt assigning a subject an 
    # incomplete annotation.
    
    anno <- x$template$annotations.txt$content
    anno[anno == ""] <- NA_character_
    anno <- anno[complete.cases(anno), ]
    
    # ResponsibleParty elements often recur throughout an EML record, each of 
    # which requires a unique ID. To facilitate expansion of ResponsibleParty
    # from the annotations.txt template, create an empty copy of the 
    # annotations data frame (anno) and fill it with ResponsibleParty elements 
    # gathered from the EML (rp). Then replace the ResponsibleParty of anno 
    # with rp and add to the /eml/datset/annotations element within the EML.
    
    rp <- anno[0, ]
    
    # Convert annotations into the list structure expected by EML::
    
    anno_ls <- lapply(
      seq(length(anno$id)),
      function(k) {
        list(
          propertyURI = list(
            anno$predicate_uri[k],
            label = anno$predicate_label[k]),
          valueURI = list(
            anno$object_uri[k],
            label = anno$object_label[k]))
      })
    
    # Annotate ----------------------------------------------------------------
    
    # Match annotatable elements (subjects) to their annotations listed in
    # the annotations.txt template. Target EML sub-trees where the subjects 
    # occur, extract their values and, when not unique, their context, then 
    # use these as keys to annotations.txt from which IDs and predicate
    # + object annotations are retrieved and assigned to the EML. Annotatable
    # elements only supported through ID references are added to  
    # /eml/dataset/annotations (e.g. ResponsibleParty).
    
    annotate_element <- function(element) {
      
      if (element == "dataset") {
        
        if (!is.null(eml$dataset)) {
          sub_i <- element
          eml$dataset$id <- unique(anno$id[anno$subject == sub_i])
          eml$dataset$annotation <- anno_ls[anno$subject == sub_i]
        }
        
      } else if (element == "dataTable") {
        
        if (!is.null(eml$dataset$dataTable)) {
          lapply(
            seq_along(eml$dataset$dataTable),
            function(k) {
              sub_i <- eml$dataset$dataTable[[k]]$physical$objectName
              if (sub_i != "") {
                eml$dataset$dataTable[[k]]$id <<- unique(anno$id[anno$subject == sub_i])
                eml$dataset$dataTable[[k]]$annotation <<- anno_ls[anno$subject == sub_i]
              }
              lapply(
                seq_along(eml$dataset$dataTable[[k]]$attributeList$attribute), 
                function(m) {
                  sub_i <- eml$dataset$dataTable[[k]]$attributeList$attribute[[m]]$attributeName
                  con_i <- eml$dataset$dataTable[[k]]$physical$objectName
                  use_i <- (anno$subject == sub_i) & (anno$context == con_i)
                  eml$dataset$dataTable[[k]]$attributeList$attribute[[m]]$id <<- unique(anno$id[use_i])
                  eml$dataset$dataTable[[k]]$attributeList$attribute[[m]]$annotation <<- anno_ls[use_i]
                })
            })
        }
        
      } else if (element == "otherEntity") {
        
        if (!is.null(eml$dataset$otherEntity)) {
          lapply(
            seq_along(eml$dataset$otherEntity),
            function(k) {
              sub_i <- eml$dataset$otherEntity[[k]]$physical$objectName
              if (sub_i != "") {
                eml$dataset$otherEntity[[k]]$id <<- unique(anno$id[anno$subject == sub_i])
                eml$dataset$otherEntity[[k]]$annotation <<- anno_ls[anno$subject == sub_i]
              }
              k
            })
        }
        
      } else if (element == "ResponsibleParty") {
        
        # A helper function for assigning ResponsibleParty IDs and appending
        # recurrences in the rp data frame.
        
        append_rp <- function(n) {
          sub_i <- paste(
            c(unlist(n$individualName$givenName), n$individualName$surName),
            collapse = " ")
          if ((sub_i != "") & any(anno$subject == sub_i)) {
            n$id <- paste0(
              unique(anno$id[anno$subject == sub_i]),
              " ",
              uuid::UUIDgenerate(use.time = TRUE))
            rp <<- rbind(
              rp,
              suppressWarnings(
                data.frame(
                  id = n$id,
                  anno[
                    anno$subject == sub_i,
                    c("element", "context", "subject", "predicate_label",
                      "predicate_uri", "object_label", "object_uri")
                    ])))
          }
          n
        }
        
        if (!is.null(eml$dataset$creator)) {
          eml$dataset$creator <- lapply(
            eml$dataset$creator,
            function(k) {
              append_rp(k)
            })
        }
        
        if (!is.null(eml$dataset$contact)) {
          eml$dataset$contact <- lapply(
            eml$dataset$contact,
            function(k) {
              append_rp(k)
            })
        }
        
        if (!is.null(eml$dataset$associatedParty)) {
          eml$dataset$associatedParty <- lapply(
            eml$dataset$associatedParty,
            function(k) {
              append_rp(k)
            })
        }
        
        if (!is.null(eml$dataset$project$personnel)) {
          eml$dataset$project$personnel <- lapply(
            list(eml$dataset$project$personnel),
            function(k) {
              append_rp(k)
            })
        }
        
        if (!is.null(eml$dataset$project$relatedProject)) {
          eml$dataset$project$relatedProject <- lapply(
            eml$dataset$project$relatedProject,
            function(k) {
              k$personnel <- append_rp(k$personnel)
              k
            })
        }
        
      }
      
      eml
      
    }
    
    # Assign IDs and annotations. Note: ResponsibleParty annotations are 
    # added to /eml/dataset/annotations (below).
    
    eml <- annotate_element("dataset")
    eml <- annotate_element("dataTable")
    eml <- annotate_element("otherEntity")
    eml <- annotate_element("ResponsibleParty")
    
    # Replace ResponsibleParty in anno with expanded ResponsibleParty in rp and 
    # update anno_ls so all annotations listed in /eml/dataset/annotations will
    # have the correct ID pairing.
    #
    # (A patch for EML::) IDs within the annotations template uses a 
    # preceding "/" (e.g. /dataTable/attribute) that EML:: fails to include in 
    # the id attribute of annotatable elements but does include in the
    # references attribute of /eml/dataset/annotations/annotation. Remove 
    # preceding "/" from anno$id until this is fixed in EML::.
    
    anno <- anno[anno$element != "/ResponsibleParty", ]
    anno <- rbind(anno, rp)
    anno$id <- stringr::str_remove(anno$id, "^/")
    
    anno_ls <- lapply(
      seq(length(anno$id)),
      function(k) {
        list(
          propertyURI = list(
            anno$predicate_uri[k],
            label = anno$predicate_label[k]),
          valueURI = list(
            anno$object_uri[k],
            label = anno$object_label[k]))
      })
    
    # Create the /dataset/annotations node and append below.
    
    annotations <- list(
      annotation = lapply(
        seq_along(anno_ls),
        function(k) {
          list(
            references = anno$id[k],
            propertyURI = list(
              label = anno_ls[[k]]$propertyURI$label,
              propertyURI = anno_ls[[k]]$propertyURI[[1]]),
            valueURI = list(
              label = anno_ls[[k]]$valueURI$label,
              valueURI = anno_ls[[k]]$valueURI[[1]]))
        }))
    
    # Remove unused references from the /dataset/annotations node or the EML will
    # be invalid
    
    rmatch <- function(x, name) {
      pos <- match(name, names(x))
      if (!is.na(pos)) return(x[[pos]])
      for (el in x) {
        if (class(el) == "list") {
          out <- Recall(el, name)
          if (!is.null(out)) return(out)
        }
      }
    }
    
    references <- unlist(
      lapply(
        annotations$annotation, 
        rmatch, 
        "references"))
    
    eml_flattened <- unlist(eml)
    eml_ids <- eml_flattened[
      stringr::str_detect(names(eml_flattened), "id")]
    
    annotation_ids <- stringr::str_remove(
      eml_ids[
        stringr::str_detect(eml_ids, "/")], 
      "^/")
    
    annotations_clean <- annotations$annotation[
      references %in% annotation_ids]
    
    # Add the /eml/datset/annotations element created and cleaned above
    
    eml$annotations$annotation <- annotations_clean
    
    # Return ------------------------------------------------------------------
    
    # If writing to file, then update packageId, schemaLocation, and EML version.
    
    if (is.character(eml.in)) {
      eml$packageId <- basename(tools::file_path_sans_ext(eml.out))
      eml$schemaLocation <- "eml://ecoinformatics.org/eml-2.2.0  http://nis.lternet.edu/schemas/EML/eml-2.2.0/xsd/eml.xsd"
      emld::eml_version("eml-2.2.0")
      EML::write_eml(eml, eml.out)
    }
    
    if (is.character(eml.in) | is.list(eml.in)) {
      message("Done.")
    }
    
    eml
    
  }

}
















#' Annotate an EML element
#' 
#' This is a helper function for \code{annotate_eml()}.
#'
#' @param element 
#'     (character) Element to be annotated
#' @param eml 
#'     (emld list) EML to be annotated
#' @param anno
#'     (data.frame) The annotations.txt template
#' @param rp
#'     (data.frame) The responsibleParty table
#' 
#'
#' @return
#' \item{eml}{emld list object being annotated}
#' 
#' @details 
#' Match annotatable elements (subjects) to their annotations listed in
#' the annotations.txt template. Target EML sub-trees where the subjects 
#' occur, extract their values and, when not unique, their context, then 
#' use these as keys to annotations.txt from which IDs and predicate + 
#' object annotations are retrieved and assigned to the EML. Annotatable
#' elements only supported through ID references are added to 
#' /eml/dataset/annotations (e.g. ResponsibleParty).
#' 
#' @keyword internal
#' 
annotate_element <- function(element, eml, anno, rp) {
  
  if (element == "dataset") {
    
    if (!is.null(eml$dataset)) {
      sub_i <- element
      eml$dataset$id <- unique(anno$id[anno$subject == sub_i])
      eml$dataset$annotation <- anno_ls[anno$subject == sub_i]
    }
    
  } else if (element == "dataTable") {
    
    if (!is.null(eml$dataset$dataTable)) {
      lapply(
        seq_along(eml$dataset$dataTable),
        function(k) {
          sub_i <- eml$dataset$dataTable[[k]]$physical$objectName
          if (sub_i != "") {
            eml$dataset$dataTable[[k]]$id <<- unique(anno$id[anno$subject == sub_i])
            eml$dataset$dataTable[[k]]$annotation <<- anno_ls[anno$subject == sub_i]
          }
          lapply(
            seq_along(eml$dataset$dataTable[[k]]$attributeList$attribute), 
            function(m) {
              sub_i <- eml$dataset$dataTable[[k]]$attributeList$attribute[[m]]$attributeName
              con_i <- eml$dataset$dataTable[[k]]$physical$objectName
              use_i <- (anno$subject == sub_i) & (anno$context == con_i)
              eml$dataset$dataTable[[k]]$attributeList$attribute[[m]]$id <<- unique(anno$id[use_i])
              eml$dataset$dataTable[[k]]$attributeList$attribute[[m]]$annotation <<- anno_ls[use_i]
            })
        })
    }
    
  } else if (element == "otherEntity") {
    
    if (!is.null(eml$dataset$otherEntity)) {
      lapply(
        seq_along(eml$dataset$otherEntity),
        function(k) {
          sub_i <- eml$dataset$otherEntity[[k]]$physical$objectName
          if (sub_i != "") {
            eml$dataset$otherEntity[[k]]$id <<- unique(anno$id[anno$subject == sub_i])
            eml$dataset$otherEntity[[k]]$annotation <<- anno_ls[anno$subject == sub_i]
          }
          k
        })
    }
    
  } else if (element == "ResponsibleParty") {
    
    # Responsible party --------------------------------------
    
    if (!is.null(eml$dataset$creator)) {
      r <- assign_responsible_party_id(eml$dataset$creator, anno, rp)
      eml$dataset$creator <- r$responsible.party
      rp <- r$rp
    }
    
    if (!is.null(eml$dataset$contact)) {
      r <- assign_responsible_party_id(eml$dataset$contact, anno, rp)
      eml$dataset$contact <- r$responsible.party
      rp <- r$rp
    }
    
    if (!is.null(eml$dataset$associatedParty)) {
      eml$dataset$associatedParty <- lapply(
        eml$dataset$associatedParty,
        function(k) {
          append_rp(k)
        })
    }
    
    if (!is.null(eml$dataset$project$personnel)) {
      eml$dataset$project$personnel <- lapply(
        eml$dataset$project$personnel,
        function(k) {
          append_rp(k)
        })
    }
    
    if (!is.null(eml$dataset$project$relatedProject)) {
      eml$dataset$project$relatedProject <- lapply(
        eml$dataset$project$relatedProject,
        function(k) {
          k$personnel <- lapply(
            k$personnel,
            function(m) {
              append_rp(m)
            })
          k
        })
    }
    
  }
  
  eml
  
}

#' Assign ResponsibleParty IDs and append recurrences to the rp data frame
#' 
#' This is a helper function for \code{annotate_eml()}
#'
#' @param responsible.party
#'     (emld list) A ResponsibleParty EML node in the emld list format.
#' @param annotations.template
#'     (data.frame) The annotations template created within 
#'     \code{annotate_eml()}.
#'
#' @return
#'     \item{responsible.party}{(emld list) Annotated responsible party node}
#'     \item{rp}{(data.frame) the rp object}
#' 
append_rp <- function(responsible.party, annotations.template) {
  
  subject <- paste(
    c(
      unlist(responsible.party$individualName$givenName), 
      responsible.party$individualName$surName),
    collapse = " ")
  
  message(subject)
  
  if ((subject != "") & any(annotations.template$subject == subject)) {
    
    responsible.party$id <- paste0(
      unique(
        annotations.template$id[
          annotations.template$subject == subject]),
      " ",
      uuid::UUIDgenerate(use.time = TRUE))
    
    rp <- suppressWarnings(
      data.frame(
        id = responsible.party$id,
        annotations.template[ 
          annotations.template$subject == subject,
          c("element", "context", "subject", "predicate_label",
            "predicate_uri", "object_label", "object_uri")
          ]))
    
    list(
      responsible.party = responsible.party, 
      rp = rp)
    
  }
  
}
















#' Update the rp object with ID information
#' 
#' This is a helper function for \code{annotate_eml()}
#'
#' @param responsible.party
#'     (emld list) A ResponsibleParty EML node in the emld list format.
#' @param anno
#'     (data.frame) The annotations template created within 
#'     \code{annotate_eml()}.
#' @param rp
#'     (data.frame) A growing set of responsible party metadata to be used
#'     in \code{annotate_eml()}
#'
#' @return
#'     \item{responsible.party}{(emld list) Annotated responsible party node}
#'     \item{rp}{(data.frame) the rp object}
#' 
assign_responsible_party_id <- function(responsible.party, anno, rp) {
  
  r <- lapply(
    responsible.party,
    function(k) {
      append_rp(
        responsible.party = k,
        annotations.template = anno)
    })
  
  list(
    responsible.party = lapply(r, `[[`, 1),
    rp = data.table::rbindlist(lapply(r, `[[`, 2)))

}