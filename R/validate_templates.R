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
  
  # Parameterize --------------------------------------------------------------
  
  attr_tmp <- read_template_attributes()
  
  if (fun.name == 'make_eml'){
    
    # make_eml() --------------------------------------------------------------
    
    # make_eml() - required templates -----------------------------------------
    # The minimal set of core templates are required for make_eml() to work.
    
    r_tmp <- attr_tmp[attr_tmp$required_template, ]
    r <- unlist(
      lapply(
        r_tmp$regexpr,
        function(k) {
          !any(stringr::str_detect(names(x$template), k))
        }))
    if (any(r)) {
      stop(
        paste0("These required templates don't exist or have missing content:\n",
               paste(r_tmp$template_name[r], collapse = ", ")),
        call. = F)
    }
    
    # make_eml() - catvars.txt ------------------------------------------------
    
    # FIXME: Categorical variable templates are expected when table attributes are listed
    # as "categorical"
    
    # All categorical variable codes require definition
    
    use_i <-stringr::str_detect(
      names(x$template), 
      attr_tmp$regexpr[attr_tmp$template_name == "catvars"])
    if (any(use_i)) {
      r <- unlist(
        lapply(
          seq(length(use_i))[use_i],
          function(k) {
            if (any(x$template[[k]]$content$definition == "")) {
              paste0(
                names(x$template[k]), 
                " contains codes without definition. Please define these codes:\n", 
                paste(
                  x$template[[k]]$content$code[
                    x$template[[k]]$content$definition == ""],
                  collapse = ", "))
            }
          }))
      if (!is.null(r)) {
        stop(paste(r, collapse = "\n"), call. = F)
      }
    }
    
    # make_eml() - attributes.txt ---------------------------------------------
    
    # FIXME: Much of these of these function calls are identical. Create a helper
    # to optimize the code.
    
    # FIXME: attributes.txt should be present for each data table
    
    # Remaining dateTimeFormatString prompts have been removed
    
    use_i <-stringr::str_detect(
      names(x$template), 
      attr_tmp$regexpr[attr_tmp$template_name == "attributes"])
    if (any(use_i)) {
      r <- unlist(
        lapply(
          seq(length(use_i))[use_i],
          function(k) {
            use_i <- stringr::str_detect(
              x$template[[k]]$content$dateTimeFormatString, 
              "^!.*!$")
            if (any(use_i)) {
              paste0(
                names(x$template[k]), 
                " contains invalid dateTimeFormatString values. Please check ",
                "these values for the attributes:\n", 
                paste(
                  x$template[[k]]$content$attributeName[use_i], 
                  collapse = ", "))
            }
          }))
      if (!is.null(r)) {
        stop(paste(r, collapse = "\n"), call. = F)
      }
    }
    
    # Each attribute has a definition

    use_i <-stringr::str_detect(
      names(x$template), 
      attr_tmp$regexpr[attr_tmp$template_name == "attributes"])
    if (any(use_i)) {
      r <- unlist(
        lapply(
          seq(length(use_i))[use_i],
          function(k) {
            use_i <- x$template[[k]]$content$attributeDefinition == ""
            if (any(use_i)) {
              paste0(
                names(x$template[k]), 
                " is missing attribute definitions. Please add definitions for ",
                "these attributes:\n", 
                paste(
                  x$template[[k]]$content$attributeName[use_i], 
                  collapse = ", "))
            }
          }))
      if (!is.null(r)) {
        stop(paste(r, collapse = "\n"), call. = F)
      }
    }

    # Each attribute has a class
    
    use_i <-stringr::str_detect(
      names(x$template), 
      attr_tmp$regexpr[attr_tmp$template_name == "attributes"])
    if (any(use_i)) {
      r <- unlist(
        lapply(
          seq(length(use_i))[use_i],
          function(k) {
            use_i <- (x$template[[k]]$content$class == "") & 
              (x$template[[k]]$content$attributeName != "")
            if (any(use_i)) {
              paste0(
                names(x$template[k]), 
                " contains attributes without a class. Please add a class to ",
                "these attributes:\n",
                paste(
                  x$template[[k]]$content$attributeName[use_i], 
                  collapse = ", "))
            }
          }))
      if (!is.null(r)) {
        stop(paste(r, collapse = "\n"), call. = F)
      }
    }

    # FIXME: Accepted classes are numeric, Date, character, categorical
    
    # Each Date class has a dateTimeformatString
    
    use_i <-stringr::str_detect(
      names(x$template), 
      attr_tmp$regexpr[attr_tmp$template_name == "attributes"])
    if (any(use_i)) {
      r <- unlist(
        lapply(
          seq(length(use_i))[use_i],
          function(k) {
            use_i <- (tolower(x$template[[k]]$content$class) == "date") & 
              (x$template[[k]]$content$dateTimeFormatString == "")
            if (any(use_i)) {
              paste0(
                names(x$template[k]), 
                " has attributes classified as 'Date' without a ",
                "corresponding dateTimeFormatString. Please add a format ",
                "string to these attributes:\n",
                paste(
                  x$template[[k]]$content$attributeName[use_i], 
                  collapse = ", "))
            }
          }))
      if (!is.null(r)) {
        stop(paste(r, collapse = "\n"), call. = F)
      }
    }

    # Each numeric class has a unit
    
    use_i <-stringr::str_detect(
      names(x$template), 
      attr_tmp$regexpr[attr_tmp$template_name == "attributes"])
    if (any(use_i)) {
      r <- unlist(
        lapply(
          seq(length(use_i))[use_i],
          function(k) {
            use_i <- (tolower(x$template[[k]]$content$class) == "numeric") & 
              (x$template[[k]]$content$unit == "")
            if (any(use_i)) {
              paste0(
                names(x$template[k]), 
                " has numeric attributes without units. Please add units to ",
                "these attributes:\n",
                paste(
                  x$template[[k]]$content$attributeName[use_i], 
                  collapse = ", "))
            }
          }))
      if (!is.null(r)) {
        stop(paste(r, collapse = "\n"), call. = F)
      }
    }

    # Each missingValueCode has a missingValueCodeExplanation
    
    use_i <-stringr::str_detect(
      names(x$template), 
      attr_tmp$regexpr[attr_tmp$template_name == "attributes"])
    if (any(use_i)) {
      r <- unlist(
        lapply(
          seq(length(use_i))[use_i],
          function(k) {
            use_i <- (x$template[[k]]$content$missingValueCode != "") & 
              (x$template[[k]]$content$missingValueCodeExplanation == "")
            if (any(use_i)) {
              paste0(
                names(x$template[k]), 
                " has missingValueCode(s) without ",
                "missingValueCodeExplanation(s). Please fix this for these ",
                "attributes:\n",
                paste(
                  x$template[[k]]$content$attributeName[use_i], 
                  collapse = ", "))
            }
          }))
      if (!is.null(r)) {
        stop(paste(r, collapse = "\n"), call. = F)
      }
    }

    # Each missingValueCodeExplanation has a non-blank missingValueCode
    
    use_i <-stringr::str_detect(
      names(x$template), 
      attr_tmp$regexpr[attr_tmp$template_name == "attributes"])
    if (any(use_i)) {
      r <- unlist(
        lapply(
          seq(length(use_i))[use_i],
          function(k) {
            use_i <- (x$template[[k]]$content$missingValueCode == "") & 
              (x$template[[k]]$content$missingValueCodeExplanation != "")
            if (any(use_i)) {
              paste0(
                names(x$template[k]), 
                " has missingValueCodeEplanation(s) without ",
                "missingValueCode(s). Please fix this for these ",
                "attributes:\n",
                paste(
                  x$template[[k]]$content$attributeName[use_i], 
                  collapse = ", "))
            }
          }))
      if (!is.null(r)) {
        stop(paste(r, collapse = "\n"), call. = F)
      }
    }
    
    # Each missingValueCode only has 1 entry per column
    
    use_i <-stringr::str_detect(
      names(x$template), 
      attr_tmp$regexpr[attr_tmp$template_name == "attributes"])
    if (any(use_i)) {
      r <- unlist(
        lapply(
          seq(length(use_i))[use_i],
          function(k) {
            use_i <- stringr::str_count(
              x$template[[k]]$content$missingValueCode, 
              '[,]|[\\s]') > 0
            if (any(use_i)) {
              paste0(
                names(x$template[k]), 
                " has more than one missingValueCode. ",
                "Only one missingValueCode per attribute is allowed. ",
                "Please fix your data and metadata for these attributes:\n",
                paste(
                  x$template[[k]]$content$attributeName[use_i], 
                  collapse = ", "))
            }
          }))
      if (!is.null(r)) {
        stop(paste(r, collapse = "\n"), call. = F)
      }
    }
    
    # FIXME: Set attribute names
    # 
    # if (length(attributes$attributeName) != length(df_attributes$attributeName)){
    #   stop(
    #     paste(
    #       'The number of attributes listed in ',
    #       fname_table_attributes[i],
    #       ' does not match the number of columns listed in the corresponding data table. Please correct this. '
    #     )
    #   )
    # }
    
    # FIXME: Numeric classed attributes should not contain characters other than the 
    # specified missing value codes
    # if ((class(raw) == "character") | (class(raw) == "factor")){
    #   stop(paste0('Characters strings found in the column "',
    #               colnames(df_table)[is_numeric[j]],
    #               '" of the file "',
    #               table_names[i],
    #               '". ',
    #               'Please remove these non-numeric characters and try again.'))
    # }

    # make_eml() - geographic_coverage.txt ------------------------------------
    # Only one geographic coverage template is allowed

    use_i <- stringr::str_detect(names(x$template), "bounding_boxes.txt|geographic_coverage.txt")
    if (sum(use_i) > 1) {
      stop(
        paste0(
          "Only one geographic coverage template is allowed. Please remove ",
          "one of these:\n", paste(names(x$template)[use_i], collapse = ",")),
        call. = F)
    }

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




# Helper functions ------------------------------------------------------------




read_template_attributes <- function() {
  data.table::fread(
    system.file(
      '/templates/template_characteristics.txt',
      package = 'EMLassemblyline'), 
    fill = TRUE,
    blank.lines.skip = TRUE)
}




check_duplicate_templates <- function(path) {
  # path = Path to the directory containing metadata templates
  attr_tmp <- read_template_attributes()
  # FIXME: Remove the next line of code once table attributes and categorical 
  # variables have been consolidated into their respective single templates
  # (i.e. "table_attributes.txt" and "table_categorical_variables.txt").
  attr_tmp <- attr_tmp[
    !stringr::str_detect(attr_tmp$template_name, "attributes|catvars"), ]
  for (i in 1:length(attr_tmp$template_name)) {
    use_i <- stringr::str_detect(
      list.files(path), 
      attr_tmp$regexpr[
        attr_tmp$template_name == attr_tmp$template_name[i]])
    if (sum(use_i) > 1) {
      stop(
        paste0(
          "Duplicate '", 
          attr_tmp$template_name[i], 
          "' templates found. There can be only one."),
        call. = F)
    }
  }
}




remove_empty_templates <- function(x) {
  # Removes empty templates (NULL, data frames with 0 rows, or TextType of 0 
  # characters) from the list object created by template_arguments().
  # x = template_arguments()$x
  attr_tmp <- read_template_attributes()
  use_i <- rep(F, length(x$template))
  for (i in 1:length(x$template)) {
    if (is.null(x$template[[i]]$content)) {
      use_i[i] <- T
    } else {
      if (any(attr_tmp$template_name == 
              tools::file_path_sans_ext(names(x$template[i])))) {
        if ((attr_tmp$type[
          attr_tmp$template_name == 
          tools::file_path_sans_ext(names(x$template[i]))]) == "text") {
          if (sum(nchar(unlist(x$template[[i]]))) == 0) {
            use_i[i] <- T
          }
        } else if ((attr_tmp$type[
          attr_tmp$template_name == 
          tools::file_path_sans_ext(names(x$template[i]))]) == "xml") {
          if (length(x$template[[i]]$content$taxonomicClassification) == 0) {
            use_i[i] <- T
          }
        } else {
          if (nrow(x$template[[i]]$content) == 0) {
            use_i[i] <- T
          }
        }
      }
    }
  }
  if (all(use_i)) {
    x["template"] <-list(NULL)
  } else {
    x$template[use_i] <- NULL
  }
  x
}
