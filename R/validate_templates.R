#' Validate metadata template content
#'
#' @description
#'     Validate the content of `EMLassembline` metadata templates.
#'
#' @usage
#'     validate_templates(
#'       fun.name,
#'       fun.args
#'     )
#'
#' @param fun.name
#'     (character) Function name passed to `validate_x` with 
#'     `as.character(match.call()[[1]])`.
#' @param x
#'     (named list) Use \code{template_arguments()} to create \code{x}.
#'
#' @details
#'     Validation checks are function specific.
#'

validate_templates <- function(fun.name, x){
  
  # make_eml() ----------------------------------------------------------------
  # If called from make_eml()
  
  if (fun.name == 'make_eml'){
    
    # make_eml() - annotations ------------------------------------------------
    # General criteria:
    # 1.) If no annotations.txt then send a best practice alert as a warning.
    # 2.) annotations.txt should have at least one row
    # 3.) Incomplete annotations results in warning.
    # 4.) Missing dataset annotation results in warning (every data package
    #     should have one).
    # 4.) URIs are resolvable
    #
    # When dataTable is present:
    # 1.) Missing dataTable annotations results in warning
    # 2.) Missing attribute annotations results in warning
    # 3.) Missing unit annotations results in warning
    #
    # When otherEntity is present:
    # 1.) Missing otherEntity annotations result in warning
    #
    # When individualName is present:
    # 1.) Missing individualName results in warning
    # 2.) Missing organizationName results in warning
    
    # make_eml() - annotations - general --------------------------------------
    
    if (is.null(x$template$annotations.txt)) {
      
      # If no annotations.txt then send a best practice alert as a warning.
      
      warning(
        paste0(
          "Consider annotating your metadata. Annotations provide precise ",
          "meaning thereby improving human and machine understanding. Use ",
          "the template_annotations() function to create the ",
          "annotations.txt template."
        ),
        call. = FALSE
      )
      
    } else if (!is.null(x$template$annotations.txt)) {
      
      # annotations.txt should have at least one row
      
      if (nrow(x$template$annotations.txt$content) != 0) {
        stop(
          paste0(
            "annotations.txt cannot be empty. Remove this file from path or ",
            "complete it."
          ), 
          call. = FALSE
        )
      }
      
      # Parameterize the annotations checks with a data frame of annotations.txt 
      # but with "" filled with NA_character_. This streamlines the code.
      
      anno <- x$template$annotations.txt$content
      anno[anno == ""] <- NA_character_
      
      # Incomplete annotations results in warning - Expected are a subject 
      # ID, predicate label and URI, object label and URI.
      
      df <- dplyr::select(
        anno,
        c(
          "id", 
          "predicate_label", 
          "predicate_uri", 
          "object_label", 
          "object_uri"
        )
      )
      
      if (any(!complete.cases(df))) {
        warning(
          paste0(
            "Incomplete annotations. A complete annotation requires an ID, ",
            "predicate label, predicate URI, object label, and object URI. One ",
            "or more of these are missing in annotations.txt at rows:\n",
            paste(seq(nrow(df))[!complete.cases(df)], collapse = ", ")
          ),
          call. = FALSE
        )
      }
      
      # Missing dataset annotation results in warning (every data package
      # should have one).
      
      if (!any((anno$element == "dataset") & complete.cases(anno))) {
        warning(
          paste0(
            "The dataset annotation is missing. Consider adding a highlevel ",
            "annotation that describes your dataset. Add this to ",
            "annotations.txt."
          ), 
          call. = FALSE
        )
      }
      
      # URIs are resolvable
      # FIXME: Create function to test whether URIs are resolvable.
      # The test has already been implemented.
      # 
      # resolve_uri <- function(k, cname) {
      #   # k is a vector of URIs
      #   # cname is the vector name
      #   # return value is a logical vector of length k (use_i)
      #   if (any(!use_i)) {
      #     warning(
      #       paste0(
      #         "Non-resolvable URIs in the ", cname, " column of ",
      #         "annotations.txt at lines:",
      #         paste(seq(length(k))[!use_i], collapse = ", ")
      #       ),
      #       call. = FALSE
      #     ) 
      #   }
      # }
      # 
      # resolve_uri(anno$predicate_uri, "predicate_uri")
      # resolve_uri(anno$object_uri, "object_uri")
      
      # make_eml() - annotations - dataTable ----------------------------------
      
      if (!is.null(names(x$data.table))) {
        
        # Missing dataTable annotations results in warning
        
        if (!any(anno$element == "dataTable")) {
          warning(
            paste0(
              "The dataTable annotation is missing. Consider adding an ",
              "annotation that describes your data table(s). Add this to ",
              "annotations.txt."
            ),
            call. = FALSE
          )
        }
        
        # Missing attribute annotations results in warning
        
        if (!any(anno$element == "attribute")) {
          warning(
            paste0(
              "The attribute annotation is missing. Consider adding ",
              "annotations that describe the columns of your data table(s). ",
              "Add these to annotations.txt."
            ),
            call. = FALSE
          )
        }
        
        # Missing unit annotations results in warning
        
        if (!any(anno$element == "unit")) {
          warning(
            paste0(
              "The unit annotation is missing. Consider adding annotations ",
              "that describe the units of each column of your data table(s). ",
              "Add these to annotations.txt."
            ),
            call. = FALSE
          )
        }
        
      }
      
      # make_eml() - annotations - otherEntity ------------------------------
      
      if (!is.null(names(x$other.entity))) {
        
        # Missing otherEntity annotations result in warning
        
        if (!any(anno$element == "otherEntity")) {
          warning(
            paste0(
              "The otherEntity annotation is missing. Consider adding an ",
              "annotation that describes your other data entities. Add this ",
              "to annotations.txt."
            ),
            call. = FALSE
          )
        }
        
      }
      
      # make_eml() - annotations - individualName -----------------------------
      
      # FIXME: A WIP for annotating persons
      # if (!is.null(x$template$personnel.txt)) {
      #   
      #   # Missing individualName results in warning
      #   
      #   if (!any(anno$element == "individualName")) {
      #     warning(
      #       paste0(
      #         "The individualName annotation is missing. Consider adding an ",
      #         "annotation that identifies the creator(s) of these data as a ",
      #         "person. Add this to annotations.txt."
      #       ),
      #       call. = FALSE
      #     )
      #   }
      #   
      #   # make_eml() - annotations - organizationName -------------------------
      #   
      #   # Missing organizationName results in warning
      #   
      #   if (!any(anno$element == "organizationName")) {
      #     warning(
      #       paste0(
      #         "The organizationName annotation is missing. Consider adding ",
      #         "an annotation that describes the organization the creator(s) ",
      #         " is associated with. Add this to annotations.txt."
      #       ),
      #       call. = FALSE
      #     )
      #   }
      #   
      # }
      
    }

        
    # make_eml() - units ------------------------------------------------------
    
    # make_eml() - units - data tables ----------------------------------------
    # Criteria:
    # 1.) Units should be present for all numeric classed attributes
    # 2.) Units should be from the standard unit dictionary or defined in
    #     custom_units.txt.
    
    # Identify attribute templates
    use_i <- stringr::str_detect(names(x$template), "attributes_(?!dataset).*.txt")
    
    # Run the checks if attribute templates exist
    if (any(use_i)) {
      
      # Add custom units to the standard unit list
      u <- EML::get_unitList()$units$id
      if (is.data.frame(x$template$custom_units.txt$content)) {
        u <- c(u, x$template$custom_units.txt$content$id)
      }
      
      for (i in names(x$template)[use_i]) {

        # Read the attributes template
        a <- x$template[[i]]$content
        
        # Numeric classed attributes have units
        if (any((a$class == "numeric") & (a$unit == ""))) {
          stop(
            paste0(
              "Numeric classed attributes require a corresponding unit. The ",
              "attributes '",
              paste(
                a$attributeName[(a$class == "numeric") & (a$unit == "")],
                collapse = ", "
              ),
              "' in the file '",
              i,
              "' are missing units. Please reference the unit dictionary to define ",
              "these (run view_unit_dictionary() to access it) or ",
              "use the custom_units.txt template to define units that can't be ",
              "found in the dictionary."
            ),
            call. = FALSE
          )
        }

        # Units should be from the dictionary or defined in custom_units.txt
        if (!all(unique(a$unit[a$unit != ""]) %in% u)) {
          missing_units <- unique(a$unit[a$unit != ""])[
            !unique(a$unit[a$unit != ""]) %in% u
          ]
          stop(
            paste0(
              "All units require definition. The units '",
              paste(missing_units, collapse = ", "),
              "' cannot be found in the standard unit dictionary or the ",
              "custom_units.txt template. Please reference the unit ",
              "dictionary to define these (run view_unit_dictionary() to",
              " access it) or use the custom_units.txt ",
              "template to define units that can't be found in the ",
              "dictionary."
            ),
            call. = FALSE
          )
        }

      }
      
      
    }

  }
  
}