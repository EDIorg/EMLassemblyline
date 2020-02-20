#' Annotate an EML document
#'
#' @description  
#'     Annotate an EML document.
#'
#' @usage 
#'     annotate_eml(
#'       annotations = NULL,
#'       eml.in = NULL,
#'       eml.out = NULL,
#'       x = NULL
#'     )
#'
#' @param annotations
#'     (character; data frame) Path to the annotations.txt metadata template 
#'     for the EML document being annotated. Create annotations.txt with 
#'     \code{template_annotations()}. Alternatively input to this argument
#'     can be a data frame of the annotations.txt template.
#' @param eml.in
#'     (character; emld list) Path to the EML document being annotated. 
#'     Alternatively this input can be supplied as an emld list object created 
#'     by \code{make_eml()} or \code{EMLassemblyline::read_eml()}.
#' @param eml.out
#'     (character) Path to which the annotated EML should be written.
#'
#' @return
#'     If the input to \code{eml.in} is a path to an EML document (.xml) then 
#'     the output will be the annotated EML document written to \code{eml.out}.
#'     If the input to \code{eml.in} is an emld list object then the output 
#'     will be the annotated version of the emld list object.
#'     
#' @details 
#'     All annotatable elements are assigned IDs and their annotations are 
#'     located both immediately under the parent element (subject) within
#'     /eml/dataset when supported and within /eml/dataset/annotations 
#'     node through references. This redundant approach supports variation 
#'     in where EML metadata consumers harvest this information and supports 
#'     the extensibility of annotating elements requiring 
#'     /eml/dataset/annotations references.
#'
#'
annotate_eml <- function(
  annotations = NULL, 
  eml.in = NULL, 
  eml.out = NULL) {
  
  # Validate arguments --------------------------------------------------------
  
  validate_arguments(
    fun.name = 'annotate_eml',
    fun.args = as.list(environment())
  )
  
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

  validate_templates(
    fun.name = "annotate_eml",
    x = x
  )
  
  # Set parameters ------------------------------------------------------------
  
  # Read EML
  
  if (is.character(eml.in)) {
    eml <- EMLassemblyline::read_eml(eml.in)
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
          label = anno$predicate_label[k]
        ),
        valueURI = list(
          anno$object_uri[k],
          label = anno$object_label[k]
        )
      )
    }
  )
  
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
              }
            )
            
          }
        )
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
          }
        )
      }
      
    } else if (element == "ResponsibleParty") {
      
      # A helper function for assigning ResponsibleParty IDs and appending
      # recurrences in the rp data frame.
      
      append_rp <- function(n) {
        sub_i <- paste(unlist(n$individualName), collapse = " ")
        if ((sub_i != "") & any(anno$subject == sub_i)) {
          n$id <- paste0(
            unique(anno$id[anno$subject == sub_i]),
            " ",
            uuid::UUIDgenerate(use.time = TRUE)
          )
          rp <<- rbind(
            rp,
            suppressWarnings(
              data.frame(
                id = n$id,
                anno[
                  anno$subject == sub_i,
                  c("element", "context", "subject", "predicate_label",
                    "predicate_uri", "object_label", "object_uri")
                  ]
              )
            )
          )
        }
        n
      }
      
      if (!is.null(eml$dataset$creator)) {
        eml$dataset$creator <- lapply(
          eml$dataset$creator,
          function(k) {
            append_rp(k)
          }
        )
      }
      
      
      if (!is.null(eml$dataset$contact)) {
        eml$dataset$contact <- lapply(
          eml$dataset$contact,
          function(k) {
            append_rp(k)
          }
        )
      }
      
      if (!is.null(eml$dataset$associatedParty)) {
        eml$dataset$associatedParty <- lapply(
          eml$dataset$associatedParty,
          function(k) {
            append_rp(k)
          }
        )
      }
      
      if (!is.null(eml$dataset$project$personnel)) {
        eml$dataset$project$personnel <- lapply(
          eml$dataset$project$personnel,
          function(k) {
            append_rp(k)
          }
        )
      }
      
      if (!is.null(eml$dataset$project$relatedProject)) {
        eml$dataset$project$relatedProject <- lapply(
          eml$dataset$project$relatedProject,
          function(k) {
            k$personnel <- lapply(
              k$personnel,
              function(m) {
                append_rp(m)
              }
            )
            k
          }
        )
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
          label = anno$predicate_label[k]
        ),
        valueURI = list(
          anno$object_uri[k],
          label = anno$object_label[k]
        )
      )
    }
  )
  
  # Create the /dataset/annotations node and append below.
  
  annotations <- list(
    annotation = lapply(
      seq_along(anno_ls),
      function(k) {
        list(
          references = anno$id[k],
          propertyURI = list(
            label = anno_ls[[k]]$propertyURI$label,
            propertyURI = anno_ls[[k]]$propertyURI[[1]]
          ),
          valueURI = list(
            label = anno_ls[[k]]$valueURI$label,
            valueURI = anno_ls[[k]]$valueURI[[1]]
          )
        )
      }
    )
  )
  
  # Add the /eml/datset/annotations element created above
  
  eml$annotations <- annotations
  
  # Return
  
  if (is.character(eml.in)) {
    EML::write_eml(eml, eml.out)
  } else {
    eml
  }
  
}
