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
        paste0("These required templates don't exist or have missing content:\n",
               paste(r_tmp$template_name[r], collapse = ", ")),
        call. = F)
    }
    
    # make_eml() - attributes.txt ---------------------------------------------
    
    # attributes.txt - attributes.txt should be present for each data table
    
    if (!is.null(x$data.table)) {
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
    }
    
    # attributeName - All table columns are listed as attributeName
    
    if (!is.null(x$data.table)) {
      r <- unlist(
        lapply(
          names(x$data.table),
          function(k) {
            use_i <- colnames(x$data.table[[k]]$content) %in% 
              x$template[[paste0("attributes_", tools::file_path_sans_ext(k), ".txt")]]$content$attributeName
            if (!all(use_i)) {
              paste0(k, " has columns that are not listed in ", 
                     paste0("attributes_", tools::file_path_sans_ext(k), ".txt."),
                     " Please add these columns to the attributes template:\n",
                     paste(colnames(x$data.table[[k]]$content)[!use_i], collapse = ", "))
            }
          }
        )
      )
      if (!is.null(r)) {
        stop(paste(r, collapse = "\n"), call. = F)
      }
    }
    
    # # FIXME: Numeric classed attributes should not contain characters other than the
    # # specified missing value codes
    # if ((class(raw) == "character") | (class(raw) == "factor")){
    #   stop(paste0('Characters strings found in the column "',
    #               colnames(df_table)[is_numeric[j]],
    #               '" of the file "',
    #               table_names[i],
    #               '". ',
    #               'Please remove these non-numeric characters and try again.'))
    # }
    
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
        " is missing attribute definitions. ",
        "Please add definitions for these attributes:"))
    
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
        "Please add a class to these attributes:"))
    
    # FIXME: Accepted classes are numeric, Date, character, categorical
    
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
        " has attributes classified as 'Date' without a ",
        "corresponding dateTimeFormatString. Please add a format ",
        "string to these attributes:"))
    
    # unit - Numeric classed attributes have units and units should be from 
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
              "' are missing units. Please reference the unit dictionary to define ",
              "these (run view_unit_dictionary() to access it) or ",
              "use the custom_units.txt template to define units that can't be ",
              "found in the dictionary."
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
    # FIXME: ? Update this to look for characters not from the date and time 
    # format string character set (e.g. YMD hms).
    
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
        " contains invalid dateTimeFormatString values. ",
        "Please check these values for the attributes:"))
    
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
        " has a missingValueCode without a missingValueCodeExplanation. ",
        "Please fix this for these attributes:"))
    
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
        " has more than one missingValueCode. ",
        "Only one missingValueCode per attribute is allowed. ",
        "Please fix your data and metadata for these attributes:"))
    
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
        " has a missingValueCodeEplanation without a missingValueCode. ",
        "Please fix this for these attributes:"))
    
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
    
    # make_eml() - catvars.txt ------------------------------------------------
    
    # FIXME: Categorical variable templates are expected when table attributes are listed
    # as "categorical"
    
    # All categorical variable codes require definition
    
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
        "Please check codes for these attributes:"))

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




check_table <- function(x, template.name, column.a, column.b, column.ref, test.expression, message.text) {
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
      stop(paste(r, collapse = "\n"), call. = F)
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
