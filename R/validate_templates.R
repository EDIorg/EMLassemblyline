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
    
    # make_eml() - core templates ---------------------------------------------
    
    # The minimal set of core templates are required for make_eml() to work.
    # FIXME: This requires an additional check for content
    # rtmp <- c("abstract", "intellectual_rights", "keywords", "methods", "personnel")
    # ftmp <- stringr::str_detect(names(x$template), paste(rtmp, collapse = "|"))
    # if (!all(ftmp)) {
    #   stop(
    #     paste0("These required templates are missing: ",
    #       paste(rtmp[!ftmp], collapse = ", "), 
    #       call. = FALSE))
    # }
    
    # make_eml() - units ------------------------------------------------------
    
    # make_eml() - units - data tables ----------------------------------------
    # Criteria:
    # 1.) Units should be present for all numeric classed attributes
    # 2.) Units should be from the standard unit dictionary or defined in
    #     custom_units.txt.
    
    # Identify attribute templates
    use_i <- stringr::str_detect(names(x$template), "attributes_.*.txt")
    
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