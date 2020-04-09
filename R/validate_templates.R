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
        paste0(
          "These templates are required but don't exist or have missing ",
          "content:\n", paste(r_tmp$template_name[r], collapse = ", ")),
        call. = F)
    }
    
    # make_eml() - abstract ---------------------------------------------------
    
    # FIXME: Report non-utf-8 encoded characters (generalize this function for 
    # TextType templates)
    
    # make_eml() - additional_info --------------------------------------------
    
    # FIXME: Report non-utf-8 encoded characters (generalize this function for 
    # TextType templates)
    
    # make_eml() - attributes -------------------------------------------------
    
    if (!is.null(x$data.table)) {
      
      # attributes.txt - attributes.txt should be present for each data table
      
      r <- unlist(
        lapply(
          names(x$data.table),
          function(k) {
            use_i <- stringr::str_detect(
              names(x$template), 
              paste0("attributes_", tools::file_path_sans_ext(k), ".txt"))
            if (!any(use_i)) {
              paste0(k, " is missing an attributes template. Create one with ",
                     "the template_table_attributes() function.")
            }
          }
        )
      )
      if (!is.null(r)) {
        stop(paste(r, collapse = "\n"), call. = F)
      }
      
      # attributeName - All table columns are listed as attributeName
      
      r <- unlist(
        lapply(
          names(x$data.table),
          function(k) {
            use_i <- colnames(x$data.table[[k]]$content) %in% 
              x$template[[
                paste0("attributes_", tools::file_path_sans_ext(k), ".txt")
                ]]$content$attributeName
            if (!all(use_i)) {
              paste0(k, " has columns that are not listed in ", 
                     "attributes_", tools::file_path_sans_ext(k), ".txt.",
                     " Add these columns:\n",
                     paste(colnames(x$data.table[[k]]$content)[!use_i], 
                           collapse = ", "))
            }
          }
        )
      )
      if (!is.null(r)) {
        stop(paste(r, collapse = "\n"), call. = F)
      }
      
      # attributeName - Names follow best practices
      
      check_table(
        x = x, 
        template.name = "attributes", 
        column.a = "attributeName", 
        column.b = "", 
        column.ref = "attributeName",
        test.expression = paste0(
          '(x$template[[k]]$content[[column.a]] != "") & ',
          'stringr::str_detect(x$template[[k]]$content[[column.a]], ',
          '"(%|[:blank:]|([:punct:]^_))")'),
        message.text = paste0(
          " contains attributes that don't follow best practices. ",
          "Consider revising these attributes to contain only alphanumeric ",
          "characters and underscores:"),
        error = F)
      
      # definition - Each attribute has a definition
      
      check_table(
        x = x,
        template.name = "attributes",
        column.a = "attributeName",
        column.b = "attributeDefinition",
        column.ref = "attributeName",
        test.expression = paste0(
          '(x$template[[k]]$content[[column.a]] != "") & ',
          '(x$template[[k]]$content[[column.b]] == "")'),
        message.text = paste0(
          " contains attributes without definition. ",
          "Add definitions to these attributes:"))
      
      
      # class - Each attribute has a class
      
      check_table(
        x = x, 
        template.name = "attributes", 
        column.a = "attributeName", 
        column.b = "class", 
        column.ref = "attributeName",
        test.expression = paste0(
          '(x$template[[k]]$content[[column.a]] != "") & ',
          '(x$template[[k]]$content[[column.b]] == "")'),
        message.text = paste0(
          " contains attributes without a class. ",
          "Add a class to these attributes:"))
      
      # class - Each class is numeric, Date, character, or categorical
      
      check_table(
        x = x, 
        template.name = "attributes", 
        column.a = "attributeName", 
        column.b = "class", 
        column.ref = "attributeName",
        test.expression = paste0(
          '(x$template[[k]]$content[[column.a]] != "") & ',
          '!stringr::str_detect(x$template[[k]]$content[[column.b]],',
          ' "numeric|Date|character|categorical")'),
        message.text = paste0(
          " contains attributes with unsupported classes. ",
          "Accepted classes are 'numeric', 'character', 'Date', and ",
          "'categorical'. Fix the class of these attributes:"))
      
      # class - Each Date class has a dateTimeformatString
      
      check_table(
        x = x, 
        template.name = "attributes", 
        column.a = "class", 
        column.b = "dateTimeFormatString", 
        column.ref = "attributeName",
        test.expression = paste0(
          '(tolower(x$template[[k]]$content[[column.a]]) == "date") & ',
          '(x$template[[k]]$content[[column.b]] == "")'),
        message.text = paste0(
          " has attributes classified as 'Date' without a corresponding date ",
          "time format string. Add a format string to these attributes:"))
      
      # FIXME: class - Numeric classed attributes should not contain characters other than the
      # specified missing value codes (implement this in the metadata quality check functions
      # to be developed? See GitHub issue #46)
      # if ((class(raw) == "character") | (class(raw) == "factor")){
      #   stop(paste0('Characters strings found in the column "',
      #               colnames(df_table)[is_numeric[j]],
      #               '" of the file "',
      #               table_names[i],
      #               '". ',
      #               'Please remove these non-numeric characters and try again.'))
      # }
      
      # class - Numeric classed attributes have units and units should be from 
      # the dictionary or defined in custom_units.txt
      # FIXME: This check should report all invalid content for all attributes.txt
      # templates, not just the first violation encountered.
      
      use_i <- stringr::str_detect(names(x$template), "attributes_.*.txt")
      if (any(use_i)) {
        u <- EML::get_unitList()$units$id
        if (is.data.frame(x$template$custom_units.txt$content)) {
          u <- c(u, x$template$custom_units.txt$content$id)
        }
        for (i in names(x$template)[use_i]) {
          a <- x$template[[i]]$content
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
                "' are missing units. Please reference the unit dictionary to ",
                "define these (run view_unit_dictionary() to access it) or ",
                "use the custom_units.txt template to define units that can't ",
                "be found in the dictionary."
              ),
              call. = FALSE)
          }
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
              call. = FALSE)
          }
        }
      }
      
      # dateTimeFormatString - Remaining dateTimeFormatString prompts have been 
      # removed
      # FIXME: Update this to look for characters not from the date and time 
      # format string character set (e.g. YMD hms)? (implement this in the metadata 
      # quality check functions to be developed? See GitHub issue #46)
      
      check_table(
        x = x, 
        template.name = "attributes", 
        column.a = "attributeName", 
        column.b = "dateTimeFormatString", 
        column.ref = "attributeName",
        test.expression = paste0(
          '(x$template[[k]]$content[[column.a]] != "") & ',
          'stringr::str_detect(x$template[[k]]$content[[column.b]], "^!.*!$")'),
        message.text = paste0(
          " contains invalid date time format strings. Check the format ",
          "strings of these attributes:"))
      
      # missingValueCode - Each missingValueCode has a 
      # missingValueCodeExplanation
      
      check_table(
        x = x, 
        template.name = "attributes", 
        column.a = "missingValueCode", 
        column.b = "missingValueCodeExplanation", 
        column.ref = "attributeName",
        test.expression = paste0(
          '(x$template[[k]]$content[[column.a]] != "") & ',
          '(x$template[[k]]$content[[column.b]] == "")'),
        message.text = paste0(
          " has missing value codes without explanation. ",
          "Add missing value code explanations for these attributes:"))
      
      # missingValueCode - Each missingValueCode only has 1 entry per column
      
      check_table(
        x = x, 
        template.name = "attributes", 
        column.a = "missingValueCode", 
        column.b = "", 
        column.ref = "attributeName",
        test.expression = paste0(
          "stringr::str_count(x$template[[k]]$content[[column.a]], ",
          "'[,]|[\\\\s]') > 0"),
        message.text = paste0(
          " has attributes with more than one missing value code. ",
          "Only one missing value code per attribute is allowed. ",
          "Remove extra mising value codes for these attributes:"))
      
      # missingValueCodeExplanation - Each missingValueCodeExplanation has a 
      # non-blank missingValueCode
      
      check_table(
        x = x, 
        template.name = "attributes", 
        column.a = "missingValueCodeExplanation", 
        column.b = "missingValueCode", 
        column.ref = "attributeName",
        test.expression = paste0(
          '(x$template[[k]]$content[[column.a]] != "") & ',
          '(x$template[[k]]$content[[column.b]] == "")'),
        message.text = paste0(
          " has missing value code explanations without a missing value codes. ",
          "Add missing value codes for these attributes:"))
      
    }
    
    # make_eml() - catvars ----------------------------------------------------
    
    if (!is.null(x$data.table)) {
      
      # catvars.txt - Required when table attributes are listed as 
      # "categorical"
      
      use_i <- stringr::str_detect(
        names(x$template),
        attr_tmp$regexpr[attr_tmp$template_name == "attributes"])
      if (any(use_i)) {
        r <- unlist(
          lapply(
            names(x$template)[use_i],
            function(k) {
              if (any(x$template[[k]]$content$class == "categorical")) {
                use_i <- stringr::str_detect(
                  names(x$template),
                  stringr::str_replace(k, "attributes", "catvars"))
                if (!any(use_i)) {
                  paste0(k, " contains categorical attributes but no categorical ",
                         "variables template can be found. Create one with the ",
                         "template_categorical_variables() function.")
                }
              }
            }
          )
        )
        if (!is.null(r)) {
          stop(paste(r, collapse = "\n"), call. = F)
        }
      }
      
      # codes - All codes require definition
      
      check_table(
        x = x,
        template.name = "catvars",
        column.a = "code",
        column.b = "definition",
        column.ref = "attributeName",
        test.expression = paste0(
          '(x$template[[k]]$content[[column.a]] != "") & ',
          '(x$template[[k]]$content[[column.b]] == "")'),
        message.text = paste0(
          " contains codes without definition. ",
          "Add codes for these attributes:"))
      
      # FIXME: codes - All codes in a table column are listed
      
    }
    
    # make_eml() - geographic_coverage ----------------------------------------
    
    # template options - Only one geographic coverage template is allowed
    
    use_i <- stringr::str_detect(names(x$template), "bounding_boxes.txt|geographic_coverage.txt")
    if (sum(use_i) > 1) {
      stop(
        paste0(
          "Only one geographic coverage template is allowed. Please remove ",
          "one of these:\n", paste(names(x$template)[use_i], collapse = ",")),
        call. = F)
    }
    
    if (any(names(x$template) == "geographic_coverage.txt")) {
      
      # geographicDescription - Each entry requires a north, south, east, and west 
      # bounding coordinate
      
      check_table(
        x = x,
        template.name = "geographic_coverage",
        column.a = "geographicDescription",
        column.b = "",
        column.ref = "geographicDescription",
        test.expression = paste0(
          '(x$template[[k]]$content[[column.a]] != "") & ',
          'as.logical(rowSums(x$template[[k]]$content[ , ',
          'c("northBoundingCoordinate", "southBoundingCoordinate", ',
          '"eastBoundingCoordinate", "westBoundingCoordinate")] == ""))'),
        message.text = paste0(
          " contains missing coordinates. ",
          "Add missing coordinates for these geographic descriptions:"))
      
      # coordinates - Decimal degree is expected
      
      check_table(
        x = x,
        template.name = "geographic_coverage",
        column.a = "geographicDescription",
        column.b = "",
        column.ref = "geographicDescription",
        test.expression = paste0(
          'suppressWarnings((x$template[[k]]$content[[column.a]] != "") & ',
          'is.na(as.numeric(x$template[[k]]$content$northBoundingCoordinate)) | ',
          'is.na(as.numeric(x$template[[k]]$content$southBoundingCoordinate)) | ',
          'is.na(as.numeric(x$template[[k]]$content$eastBoundingCoordinate)) | ',
          'is.na(as.numeric(x$template[[k]]$content$westBoundingCoordinate)))'),
        message.text = paste0(
          " contains non-numeric coordinates. ",
          "Check coordinates of these geographic descriptions:"))

    }
    
    # make_eml() - intellectual_rights ----------------------------------------
    
    # FIXME: Report non-utf-8 encoded characters (generalize this function for 
    # TextType templates)
    
    # make_eml() - methods ----------------------------------------------------
    
    # FIXME: Report non-utf-8 encoded characters (generalize this function for 
    # TextType templates)
    
    # make_eml() - personnel --------------------------------------------------
    
    if (any(names(x$template) == "personnel.txt")) {
      
      # role - At least one creator and contact is listed
      
      use_i <- stringr::str_detect(
        tolower(x$template$personnel.txt$content$role), 
        "creator|contact")
      if (!any(use_i)) {
        stop(
          paste0("personnel.txt is missing a 'creator' and/or a 'contact'. ",
                 "Add these roles to this template."), 
          call. = F)
      }
      
      # role - All personnel have roles
      
      use_i <- x$template$personnel.txt$content$role == ""
      if (any(use_i)) {
        stop(
          paste0("personnel.txt is missing one or more 'role'. Ensure ",
                 "each person in this template has a defined role."), 
          call. = F)
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




check_table <- function(x, template.name, column.a, column.b, column.ref, test.expression, message.text, error = T) {
  # Function to test table criteria among rows
  # x = template_arguments()$x
  # template.name = Short name of the template being checked
  # column.a = Primary column
  # column.b = Secondary column
  # column.ref = Column (attribute) the users will be directed
  # test.expression = Expression that will be evaluated to index rows with issues
  # message.text = Error text to return to the user, prefixed with the table name
  #                and followed by the values of the secondary column that are 
  #                indexed by test.expression.
  attr_tmp <- read_template_attributes()
  use_i <- stringr::str_detect(
    names(x$template), 
    attr_tmp$regexpr[attr_tmp$template_name == template.name])
  if (any(use_i)) {
    r <- unlist(
      lapply(
        seq(length(use_i))[use_i],
        function(k) {
          use_i2 <- eval(parse(text = test.expression))
          if (any(use_i2)) {
            paste0(names(x$template[k]), message.text, "\n",
                   paste(
                     unique(x$template[[k]]$content[[column.ref]][use_i2]),
                     collapse = ", "))
          }
        }))
    if (!is.null(r)) {
      if (isTRUE(error)) {
        stop(paste(r, collapse = "\n"), call. = F)
      } else if (!isTRUE(error)) {
        warning(paste(r, collapse = "\n"), call. = F)
      }
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




# FIXME: Create function to remove user supplied NA from templates (a common 
# issue). EMLassemblyline should be smart enough to ignore these.
