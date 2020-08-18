#' Validate metadata template content
#'
#' @param fun.name
#'     (character) Function name from which \code{validate_templates} is 
#'     called
#' @param x
#'     (named list) The data and metadata object returned by 
#'     \code{template_arguments()}.
#'     
#' @return 
#' \item{template_issues}{Any issues found in the validation process and 
#' returned as a character vector. This object can be saved for later use 
#' and viewed in a human readable form with \code{issues()}}.
#'     
#' @details
#'     Each template has a unique set of metadata providing valuable 
#'     information about corresponding data objects. This function checks
#'     for metadata content according to metadata best practices and the needs
#'     of downstream processes in the EMLassemblyline. Each set of checks is 
#'     applied within the scope of the calling function (i.e. \code{fun.name}),
#'     which each has a unique set of needs and constraints. If the needs of 
#'     down stream processes are not met, then the template content is modified 
#'     and a message returned to the user.
#'     
#'     All issues are compiled into a single report and returned to the user 
#'     with a warning. This approach of not "failing fast" allows the user to 
#'     address several issues at once.
#'
validate_templates <- function(fun.name, x) {
  message("Checking inputs")
  attr_tmp <- read_template_attributes()
  
  if (fun.name == 'make_eml'){
    # Called from make_eml() --------------------------------------------------

    # Initialize object for collecting issue messages
    issues <- c()

    # Abstract
    r <- validate_abstract(x)
    issues <- c(issues, r)
    
    # TODO: Implement a check for the additional_info template including a 
    # check for non-utf-8 encoded characters

    # Categorical variables
    if (any(
      stringr::str_detect(
        names(x$template), 
        attr_tmp$regexpr[attr_tmp$template_name == "attributes"]))) {
      r <- validate_categorical_variables(x)
      issues <- c(issues, r$issues)
      x <- r$x
    }

    # Geographic coverage
    r <- validate_geographic_coverage(x)
    issues <- c(issues, r$issues)
    x <- r$x
    
    # Intellectual rights
    issues <- c(issues, validate_intellectual_rights(x))
    
    # Keywords
    issues <- c(issues, validate_keywords(x))
    
    # Methods
    issues <- c(issues, validate_methods(x))
    
    # Personnel
    r <- validate_personnel(x)
    issues <- c(issues, r$issues)
    x <- r$x
    
    # Table attributes
    r <- validate_table_attributes(x)
    issues <- c(issues, r$issues)
    x <- r$x
    
    # Taxonomic coverage
    r <- validate_taxonomic_coverage(x)
    issues <- c(issues, r$issues)
    x <- r$x
    
    # Return
    if (!is.null(issues)) {
      list2env(list(template_issues = issues), .GlobalEnv)
      warning(
        "Input issues found. Use issues() to see them.", 
        call. = FALSE)
    }
    return(x)
  }
  
}










#' Validate the abstract template
#'
#' @param x
#'     (named list) The data and metadata object returned by 
#'     \code{template_arguments()}.
#'
#' @return
#'     (character or NULL) A character vector of issues if any were found, 
#'     otherwise NULL.
#'     
#' @details 
#'     Checks performed by this function:
#'     \itemize{
#'         \item{Abstract is not empty}
#'     }
#'
validate_abstract <- function(x) {
  msg <- NULL
  attr_tmp <- read_template_attributes()
  missing_abstract <- !any(
    stringr::str_detect(
      names(x$template), 
      attr_tmp$regexpr[attr_tmp$template_name == "abstract"]))
  if (isTRUE(missing_abstract)) {
    msg <- paste0(
      "Missing abstract. An abstract describing the data is recommended ",
      "and should briefly answer: What? Why? Where? When? How?")
  }
  
  # FIXME: Report non-utf-8 encoded characters (generalize this function for 
  # TextType templates)

  if (!is.null(msg)) {
    msg <- paste0(
      "\n",
      "Abstract (Optional)\n",
      paste(
        paste0(seq_along(msg), ". "),
        msg,
        collapse = "\n"), 
      "\n")
  }
  msg
}








#' Validate the categorical variables template (catvars_*.txt)
#'
#' @param x
#'     (named list) The data and metadata object returned by 
#'     \code{template_arguments()}.
#'
#' @return
#'     \item{issues}{(character) Descriptions of issues found in the template 
#'     grouped by "required" and "optional" criteria.}
#'     \item{x}{(named list) If "required" issues are found for a data table, 
#'     then the corresponding categorical variables template is removed. Some
#'     "optional" issues reclassified the categorical variable as "character".}
#'     
#' @details 
#'     This function validates the table attributes classified as categorical
#'     only for data tables that are listed in \code{x}.Checks performed by 
#'     this function:
#'     \itemize{
#'         \item{Categorical metadata is expected for variables classified 
#'         "categorical"}
#'         \item{Categorical variable template olumn names are correct}
#'         \item{Categorical codes are defined}
#'     }
#'
validate_categorical_variables <- function(x) {
  attr_tmp <- read_template_attributes()

  # Categorical variable metadata only matters for specified tables
  output <- lapply(
    names(x$data.table),
    function(table_file) {
      
      # Variables are classified as categorical in the table attributes 
      # template, and are defined in the categorical variables template
      attribute_file <- paste0(
        "attributes_", tools::file_path_sans_ext(table_file), ".txt")
      catvars_file <- paste0(
        "catvars_", tools::file_path_sans_ext(table_file), ".txt")

      # Each issue is logged as "required" or "optional"
      required_issues <- c()
      optional_issues <- c()

      # Categorical metadata is expected for variables classified "categorical"
      if (attribute_file %in% names(x$template)) {
        r <- validate_categorical_variable_template_presence(attribute_file, x)
        required_issues <- c(required_issues, r)
      }

      if (catvars_file %in% names(x$template)) {
        
        # Downstream processes index these metadata by column name
        r <- validate_categorical_variable_column_names(catvars_file, x)
        
        # Categorical codes are meaningless without definition
        r <- validate_categorical_variable_definitions(catvars_file, x)
        required_issues <- c(required_issues, r)
        
        # TODO: codes - All codes in a table column are listed (optional)
        
        # TODO: Categorical variables templates without a matching attributes
        # template should be removed from x (info)
        
      }
      
      # A compiled report of issues helps the user fix them
      if (!is.null(required_issues)) {
        required_issues <- paste0(
          "\n",
          "Categorical variables (", table_file, ", Required) - Variables ",
          "defined as categorical will be reclassified as 'character' until ",
          "these issues are fixed:\n",
          paste(
            paste0(seq_along(required_issues), ". "),
            required_issues,
            collapse = "\n"), 
          "\n")
      }
      if (!is.null(optional_issues)) {
        optional_issues <- paste0(
          "\n",
          "Categorical variables (", table_file, ", Optional):\n",
          paste(
            paste0(seq_along(optional_issues), ". "),
            optional_issues,
            collapse = "\n"), 
          "\n")
      }
      issues <- c(required_issues, optional_issues)
      
      # There's no recovering from required issues, and the most reasonable
      # temporary fix is to reclassify "categorical" variables as "character" 
      # and to drop the categorical variables template from further use.
      if (!is.null(required_issues)) {
        x$template[[attribute_file]]$content$class <<- stringr::str_replace(
          x$template[[attribute_file]]$content$class, "categorical", "character")
        x$template[[catvars_file]] <<- NULL
      }
      
      # Return
      issues
    })

  # Return
  list(issues = unlist(output), x = x)
  
}








#' Check for presence of the categorical variables template
#'
#' @param file.name
#'     (character) The table attributes template to apply this function to. 
#'     Full file name is required.
#' @param x
#'     (named list) The data and metadata object returned by 
#'     \code{template_arguments()}.
#'
#' @return
#'     (character or NULL) A character vector of issues if any were found, 
#'     otherwise NULL.
#'
validate_categorical_variable_template_presence <- function(file.name, x) {
  categorical_variables <- x$template[[file.name]]$content$class %in% 
    "categorical"
  if (any(categorical_variables)) {
    missing_template <- !any(
      stringr::str_detect(
        names(x$template),
        stringr::str_replace(file.name, "attributes", "catvars")))
    if (missing_template) {
      paste0(
        "Missing categorical variable metadata. Variables are listed as ",
        "categorical but no categorical metadata exists.")
    }
  }
}








#' Check column names of categorical variables template
#'
#' @param file.name
#'     (character) The categorical variables template to apply this function 
#'     to. Full file name is required.
#' @param x
#'     (named list) The data and metadata object returned by 
#'     \code{template_arguments()}.
#'
#' @return
#'     (error or NULL) An error of issues if any were found, otherwise NULL.
#'
validate_categorical_variable_column_names <- function(file.name, x) {
  expected_colnames <- colnames(
    data.table::fread(
      system.file(
        '/templates/categorical_variables.txt',
        package = 'EMLassemblyline')))
  found_colnames <- colnames(x$template[[file.name]]$content)
  if (!all(expected_colnames %in% found_colnames)) {
    stop(
      "Unexpected column names in ", file.name, ". Expected columns are: ", 
      paste(expected_colnames, collapse = ", "), call. = FALSE)
  }
}








#' Check for categorical variable definitions
#'
#' @param file.name
#'     (character) The categorical variables template to apply this function 
#'     to. Full file name is required.
#' @param x
#'     (named list) The data and metadata object returned by 
#'     \code{template_arguments()}.
#'
#' @return
#'     (character or NULL) A character vector of issues if any were found, 
#'     otherwise NULL.
#'
validate_categorical_variable_definitions <- function(file.name, x) {
  attr_tmp <- read_template_attributes()
  missing_definitions <- x$template[[file.name]]$content$definition == ""
  if (any(missing_definitions)) {
    "Missing code definitions. Codes are meaningless without definition."
  }
}








#' Validate geographic coverage metadata
#'
#' @param x 
#'     (named list) The data and metadata object returned by 
#'     \code{template_arguments()}.
#'
#' @return
#'     \item{issues}{(character) Descriptions of issues found in the template 
#'     grouped by "required" and "optional" criteria.}
#'     \item{x}{(named list) If "required" issues are found, then the template 
#'     is removed.}
#'     
#' @details 
#'     This function compiles geographic coverage input as 
#'     \code{geographic.coordinates} and \code{geographic.description} to 
#'     \code{make_eml()}, and listed in the geographic_coverage.txt template,
#'     and returns all unique values in the geographic_coverage.txt template.
#'     
#'     Checks performed by this function:
#'     \itemize{
#'         \item{Column names are correct}
#'         \item{Geographic coverage is not missing}
#'         \item{Each entry is complete}
#'         \item{Decimal degree is expected}
#'         \item{The cordinate range is valid}
#'     }
#'
validate_geographic_coverage <- function(x) {
  
  # Objects for catching required and optional issues
  required_issues <- c()
  optional_issues <- c()
  
  # Handle deprecated template
  if (any(names(x$template) == "bounding_boxes.txt")) {
    stop(
      paste0(
        "Template 'bounding_boxes.txt' is deprecated; please use ", 
        "'geographic_coverage.txt' instead."),
      call. = F)
  }
  
  # Column names are correct
  if (any(names(x$template) == "geographic_coverage.txt")) {
    r <- validate_geographic_coverage_column_names(x)
  }
  
  # Compile geographic coverage from allowed sources
  x <- compile_geographic_coverage(x)
  
  # geographic coverage is not missing
  r <- validate_geographic_coverage_presence(x)
  required_issues <- c(required_issues, r)
  
  if (any(names(x$template) == "geographic_coverage.txt")) {
    
    # Complete cases - Each entry must be complete
    r <- validate_geographic_coverage_completeness(x)
    required_issues <- c(required_issues, r)
    
    # Coordinate format - Decimal degree is expected
    r <- validate_geographic_coverage_coordinate_format(x)
    required_issues <- c(required_issues, r)
    
    # Coordinate format - Coordinate range
    r <- validate_geographic_coverage_coordinate_range(x)
    required_issues <- c(required_issues, r)
    
  }
  
  # Compile issues
  if (!is.null(required_issues)) {
    required_issues <- paste0(
      "\n",
      "Geographic coverage (Required) - Geographic coverage will be ",
      "dropped from the EML until these issues are fixed:\n",
      paste(
        paste0(seq_along(required_issues), ". "),
        required_issues,
        collapse = "\n"), 
      "\n")
  }
  if (!is.null(optional_issues)) {
    optional_issues <- paste0(
      "\n",
      "Geographic coverage (Optional):\n",
      paste(
        paste0(seq_along(optional_issues), ". "),
        optional_issues,
        collapse = "\n"), 
      "\n")
  }
  issues <- c(required_issues, optional_issues)
  
  # Drop the geographic coverage template if required issues are found
  if (!is.null(required_issues)) {
    x$template$geographic_coverage.txt <- NULL
  }
  
  # Return
  list(issues = issues, x = x)

}







#' Check geographic coverage template presence
#'
#' @param x
#'     (named list) The data and metadata object returned by 
#'     \code{template_arguments()}.
#'
#' @return
#'     (character or NULL) A character vector of issues if any were found, 
#'     otherwise NULL.
#'
validate_geographic_coverage_presence <- function(x) {
  missing_geographic_coverage <- !any(
    names(x$template) == "geographic_coverage.txt")
  if (isTRUE(missing_geographic_coverage)) {
    paste0(
      "Missing geographic coverage metadata. Geographic coverage describes ",
      "the locations and areas where these data were collected.")
  }
}








#' Check column names of geographic coverage template
#'
#' @param x 
#'     (list) The data and metadata object returned by 
#'     \code{template_arguments()}.
#'
#' @return
#'     (Error or NULL) An error if any issues were found, 
#'     otherwise NULL.
#'
validate_geographic_coverage_column_names <- function(x) {
  template <- data.table::fread(
    system.file(
      '/templates/geographic_coverage.txt',
      package = 'EMLassemblyline'))
  expected_colnames <- colnames(template)
  found_colnames <- colnames(x$template$geographic_coverage.txt$content)
  if (!all(expected_colnames %in% found_colnames)) {
    stop(
      "Unexpected column names in the geographic coverage template. Expected ",
      " columns are:\n",
      paste(expected_colnames, collapse = ", "),
      call. = FALSE)
  }
}








#' Check completeness of the geographic coverage template
#'
#' @param x 
#'     (list) The data and metadata object returned by 
#'     \code{template_arguments()}.
#'
#' @return
#'     \item{character}{Description of validation issues}
#'     \item{NULL}{If no issues were found}
#'
validate_geographic_coverage_completeness <- function(x) {
  x$template$geographic_coverage.txt$content[
    x$template$geographic_coverage.txt$content == ""] <- NA
  incomplete_rows <- !complete.cases(
    x$template$geographic_coverage.txt$content)
  if (any(incomplete_rows)) {
    paste0(
      "Incomplete geographic definitions. Geographic definitions require a ",
      "north, south, east, and west coordinate, as well as a location ",
      "name/description. These entries have incomplete definitions: ",
      paste(which(incomplete_rows), collapse = ", "))
  }
}








#' Check for decimal degree format in the geographic coverage template
#'
#' @param x 
#'     (list) The data and metadata object returned by 
#'     \code{template_arguments()}.
#'
#' @return
#'     \item{character}{Description of validation issues}
#'     \item{NULL}{If no issues were found}
#'
validate_geographic_coverage_coordinate_format <- function(x) {
  coordinates <- x$template$geographic_coverage.txt$content[, -1]
  complete_definitions <- complete.cases(
    x$template$geographic_coverage.txt$content)
  coerced_characters <- complete.cases(
    suppressWarnings(as.data.frame(lapply(coordinates, as.numeric))))
  rows_with_characters <- coerced_characters != complete_definitions
  if (any(rows_with_characters)) {
    paste0(
      "Unsupported coordinate formats. Decimal degrees are required. These ",
      "entries have non-numeric coordinates: ",
      paste(which(rows_with_characters), collapse = ", "))
  }
}








#' Check geographic coordinate ranges
#'
#' @param x 
#'     (list) The data and metadata object returned by 
#'     \code{template_arguments()}.
#'
#' @return
#'     \item{character}{Description of validation issues}
#'     \item{NULL}{If no issues were found}
#'
validate_geographic_coverage_coordinate_range <- function(x) {
  coordinates <- suppressWarnings(
    as.data.frame(
      lapply(
        x$template$geographic_coverage.txt$content[, -1], 
        as.numeric)))
  out_of_range <- unique(
    c(
      unlist(
        lapply(
          coordinates[, c("northBoundingCoordinate", 
                          "southBoundingCoordinate")],
          function(x) {
            which(abs(x) > 90)
          })),
      unlist(
        lapply(
          coordinates[, c("eastBoundingCoordinate", 
                          "westBoundingCoordinate")],
          function(x) {
            which(abs(x) > 180)
          }))))
  if (any(out_of_range)) {
    paste0(
      "Unsupported coordinate range. Latitude values should range from -90 ",
      "to 90 and longitude values from -180 to 180. These entries are out ",
      "of range: ",
      paste(out_of_range, collapse = ", "))
  }
}








#' Validate the intellectual rights template
#'
#' @param x 
#'     (list) The data and metadata object returned by 
#'     \code{template_arguments()}.
#'
#' @return
#'     \item{character}{Description of validation issues}
#'     \item{NULL}{If no issues were found}
#'     
#' @details 
#'     Checks performed by this function:
#'     \itemize{
#'         \item{Intellectual rights is not empty}
#'     }
#'
validate_intellectual_rights <- function(x) {
  msg <- NULL
  attr_tmp <- read_template_attributes()
  missing_intellectual_rights <- !any(
    stringr::str_detect(
      names(x$template), 
      attr_tmp$regexpr[attr_tmp$template_name == "intellectual_rights"]))
  if (isTRUE(missing_intellectual_rights)) {
    msg <- paste0(
      "Missing intellectual rights. An intellectual rights license is ",
      "recommended.")
  }
  
  # FIXME: Report non-utf-8 encoded characters (generalize this function for 
  # TextType templates)
  
  if (!is.null(msg)) {
    msg <- paste0(
      "\n",
      "Intellectual rights (Optional)\n",
      paste(
        paste0(seq_along(msg), ". "),
        msg,
        collapse = "\n"), 
      "\n")
  }
  msg
}








#' Validate the keywords template
#'
#' @param x 
#'     (list) The data and metadata object returned by 
#'     \code{template_arguments()}.
#'
#' @return
#'     \item{character}{Description of validation issues}
#'     \item{NULL}{If no issues were found}
#'     
#' @details 
#'     Checks performed by this function:
#'     \itemize{
#'         \item{Keywords is not empty}
#'     }
#'
validate_keywords <- function(x) {
  msg <- NULL
  attr_tmp <- read_template_attributes()
  missing_keywords <- !any(
    stringr::str_detect(
      names(x$template), 
      attr_tmp$regexpr[attr_tmp$template_name == "keywords"]))
  if (isTRUE(missing_keywords)) {
    msg <- paste0("Missing keywords. Keywords are recommended.")
  }
  if (!is.null(msg)) {
    msg <- paste0(
      "\n",
      "Keywords (Optional)\n",
      paste(
        paste0(seq_along(msg), ". "),
        msg,
        collapse = "\n"), 
      "\n")
  }
  msg
}








#' Validate the methods template
#'
#' @param x 
#'     (list) The data and metadata object returned by 
#'     \code{template_arguments()}.
#'
#' @return
#'     \item{character}{Description of validation issues}
#'     \item{NULL}{If no issues were found}
#'     
#' @details 
#'     Checks performed by this function:
#'     \itemize{
#'         \item{Methods is not empty}
#'     }
#'
validate_methods <- function(x) {
  msg <- NULL
  attr_tmp <- read_template_attributes()
  missing_methods <- !any(
    stringr::str_detect(
      names(x$template), 
      attr_tmp$regexpr[attr_tmp$template_name == "methods"]))
  if (isTRUE(missing_methods)) {
    msg <- paste0(
      "Missing methods. Methods are recommended and should describe (in ",
      "detail) how the data were created.")
  }

  # FIXME: Report non-utf-8 encoded characters (generalize this function for 
  # TextType templates)
  
  if (!is.null(msg)) {
    msg <- paste0(
      "\n",
      "Methods (Optional)\n",
      paste(
        paste0(seq_along(msg), ". "),
        msg,
        collapse = "\n"), 
      "\n")
  }
  msg
}







#' Validate the personnel template
#'
#' @param x 
#'     (list) The data and metadata object returned by 
#'     \code{template_arguments()}.
#'
#' @return
#'     \item{issues}{(character) Descriptions of issues found in the template}
#'     \item{x}{(list) A potentially modified \code{x} if relevant issues were
#'     found}
#'     
#' @details 
#'     Checks performed by this function:
#'     \itemize{
#'         \item{Personnel template is not empty}
#'         \item{Template column names are correct}
#'         \item{At least one creator and contact is listed}
#'         \item{All personnel have roles}
#'         \item{Principal investigator is present}
#'         \item{Project information is present}
#'         \item{Only one publisher is listed}
#'     }
#'     
#'     Checks are grouped by required and optional criteria. If any required
#'     checks fail, then the entire template is removed from \code{x}.
#'
validate_personnel <- function(x) {
  
  # Objects for catching required and optional issues
  required_issues <- c()
  optional_issues <- c()
  
  # personnel template is not missing
  r <- validate_personnel_presence(x)
  required_issues <- c(required_issues, r)

  if (any(names(x$template) == "personnel.txt")) {
    
    # Column names are correct
    r <- validate_personnel_column_names(x)
    
    # role - At least one creator and contact is listed
    r <- validate_personnel_creator(x)
    required_issues <- c(required_issues, r)
    r <- validate_personnel_contact(x)
    required_issues <- c(required_issues, r)
    
    # role - All personnel have roles
    r <- validate_personnel_roles(x)
    required_issues <- c(required_issues, r)
    
    # principal investigator - Is recommended
    r <- validate_personnel_pi(x)
    optional_issues <- c(optional_issues, r)
    
    # project information - projectTitle, fundingAgency, and fundingNumber 
    # should be included with each principal investigator
    r <- validate_personnel_project(x)
    optional_issues <- c(optional_issues, r)
    
    # publisher - Only one publisher is allowed and the first will be used.
    r <- validate_personnel_publisher(x)
    optional_issues <- c(optional_issues, r$issues)
    x <- r$x

  }
  
  # Compile issues
  if (!is.null(required_issues)) {
    required_issues <- paste0(
      "\n",
      "Personnel (Required) - Personnel (creators, contacts, etc.) will be ",
      "dropped from the EML until these issues are fixed:\n",
      paste(
        paste0(seq_along(required_issues), ". "),
        required_issues,
        collapse = "\n"), 
      "\n")
  }
  if (!is.null(optional_issues)) {
    optional_issues <- paste0(
      "\n",
      "Personnel (Optional):\n",
      paste(
        paste0(seq_along(optional_issues), ". "),
        optional_issues,
        collapse = "\n"), 
      "\n")
  }
  issues <- c(required_issues, optional_issues)
  
  # Drop the personnel template if required issues are found
  if (!is.null(required_issues)) {
    x$template$personnel.txt <- NULL
  }
  
  # Return
  list(issues = issues, x = x)
  
}







#' Validate personnel template presence
#'
#' @param x 
#'     (list) The data and metadata object returned by 
#'     \code{template_arguments()}.
#'
#' @return
#'     \item{character}{Description of validation issues}
#'     \item{NULL}{If no issues were found}
#'     
#' @details 
#'     Checks performed by this function:
#'     \itemize{
#'         \item{Personnel template is not empty}
#'     }
#'
validate_personnel_presence <- function(x) {
  attr_tmp <- read_template_attributes()
  missing_personnel <- !any(
    stringr::str_detect(
      names(x$template), 
      attr_tmp$regexpr[attr_tmp$template_name == "personnel"]))
  if (isTRUE(missing_personnel)) {
    paste0(
      "Missing personnel metadata. Personnel metadata assigns attribution ",
      "to the creators, principal investigators and other personnel ",
      "associated with these data, and provides a point of contact for any ",
      "user questions.")
  }
}








#' Check column names of personnel template
#'
#' @param x 
#'     (list) The data and metadata object returned by 
#'     \code{template_arguments()}.
#'
#' @return
#'     \item{error}{If column names are invalid}
#'     \item{NULL}{If no issues were found}
#'
validate_personnel_column_names <- function(x) {
  template <- data.table::fread(
    system.file(
      '/templates/personnel.txt',
      package = 'EMLassemblyline'))
  expected_colnames <- colnames(template)
  found_colnames <- colnames(x$template$personnel.txt$content)
  if (!all(expected_colnames %in% found_colnames)) {
    stop(
      "Unexpected column names in the personnel template. ",
      "Expected columns are:\n",
      paste(expected_colnames, collapse = ", "),
      call. = FALSE)
  }
}







#' Check for creator in personnel template
#'
#' @param x 
#'     (list) The data and metadata object returned by 
#'     \code{template_arguments()}.
#'
#' @return
#'     \item{character}{Description of validation issues}
#'     \item{NULL}{If no issues were found}
#'
validate_personnel_creator <- function(x) {
  creators <- tolower(
    x$template$personnel.txt$content$role) == "creator"
  if (!any(creators)) {
    paste0("Missing creator. At least one creator is required.")
  }
}







#' Check for contact in personnel template
#'
#' @param x 
#'     (list) The data and metadata object returned by 
#'     \code{template_arguments()}.
#'
#' @return
#'     \item{character}{Description of validation issues}
#'     \item{NULL}{If no issues were found}
#'
validate_personnel_contact <- function(x) {
  contacts <- tolower(
    x$template$personnel.txt$content$role) == "contact"
  if (!any(contacts)) {
    paste0("Missing contact. At least one contact is required.")
  }
}







#' Check for defined roles in personnel template
#'
#' @param x 
#'     (list) The data and metadata object returned by 
#'     \code{template_arguments()}.
#'
#' @return
#'     \item{character}{Description of validation issues}
#'     \item{NULL}{If no issues were found}
#'
validate_personnel_roles <- function(x) {
  missing_roles <- x$template$personnel.txt$content$role == ""
  if (any(missing_roles)) {
    paste0("Missing role. Each person requires a role.")
  }
}








#' Check for principal investigator in personnel template
#'
#' @param x 
#'     (list) The data and metadata object returned by 
#'     \code{template_arguments()}.
#'
#' @return
#'     \item{character}{Description of validation issues}
#'     \item{NULL}{If no issues were found}
#'
validate_personnel_pi <- function(x) {
  principal_investigators <- tolower(
    x$template$personnel.txt$content$role) == "pi"
  if (!any(principal_investigators)) {
    paste0(
      "Missing Principal Investigator. Including the PI of the project from ",
      "which the data originate is recommended.")
  }
  
}








#' Check for project information in the personnel template
#'
#' @param x 
#'     (list) The data and metadata object returned by 
#'     \code{template_arguments()}.
#'
#' @return
#'     \item{character}{Description of validation issues}
#'     \item{NULL}{If no issues were found}
#'     
#' @details 
#'     Project info should be included with each PI.
#'
validate_personnel_project <- function(x) {
  project_info <- x$template$personnel.txt$content[
    tolower(x$template$personnel.txt$content$role) == "pi",
    which(
      colnames(x$template$personnel.txt$content) %in% 
        c("projectTitle", "fundingAgency", "fundingNumber"))]
  if (nrow(project_info) > 0) {
    project_info[project_info == ""] <- NA_character_
    missing_project_info <- !complete.cases(project_info)
    if (any(missing_project_info)) {
      paste0(
        "Missing funding information. Including the project title, ",
        "funding agency, and corresponding funding number for all principal ",
        "investigators is recommended.")
    }
  }
}








#' Check for one publisher in the personnel template
#'
#' @param x 
#'     (list) The data and metadata object returned by 
#'     \code{template_arguments()}.
#'
#' @return
#'     \item{character}{Description of validation issues, or NULL if no issues
#'     were found}
#'     \item{x}{(list) A potentially modified \code{x} if relevant issues were
#'     found}
#'     
#' @details 
#'     Only one publisher is allowed.
#'
validate_personnel_publisher <- function(x) {
  msg <- NULL
  publisher <- which(
    tolower(x$template$personnel.txt$content$role) == "publisher")
  if (length(publisher) > 1) {
    msg <- "Too many publishers. Only the first will be used."
    excess_publishers <- publisher[publisher!= min(publisher)]
    x$template$personnel.txt$content <- x$template$personnel.txt$content[
      -c(excess_publishers), ]
  }
  list(issues = msg, x = x)
}







#' Validate the table attributes template (attributes_*.txt)
#'
#' @param x
#'     (named list) The data and metadata object returned by 
#'     \code{template_arguments()}.
#'
#' @return
#'     \item{issues}{(character) Descriptions of issues found in the template 
#'     grouped by "required" and "optional" criteria.}
#'     \item{x}{(named list) If "required" issues are found, then the template 
#'     is removed along with the corresponding data table.}
#'     
#' @details 
#'     Checks performed on each data tables attribute metadata:
#'     \itemize{
#'         \item{Attribute metadata is present for each data table}
#'         \item{Template columns are correctly named}
#'         \item{All columns of a table are listed as attributes}
#'         \item{All columns of a table are listed in the same order as 
#'         attributes}
#'         \item{All attributes are defined}
#'         \item{All attributes are assigned a class}
#'         \item{Classes are of the expected type}
#'         \item{Numeric classes are assigned units}
#'         \item{All units are from the standard dictionary or defined}
#'         \item{Date classed attributes have a format specifier}
#'         \item{Date and time format specifiers are not empty}
#'         \item{Only one missing value code is allowed per attribute}
#'         \item{Missing value code is not a blank/white space}
#'         \item{All missing value codes are defined}
#'         \item{False "numeric" attributes are reclassified as "character"}
#'     }
#'
validate_table_attributes <- function(x) {
  attr_tmp <- read_template_attributes()
  
  # Attribute metadata only matters for specified tables
  output <- lapply(
    names(x$data.table),
    function(table_file) {
      
      # Attribute metadata is specified in the table attributes template
      attribute_file <- paste0(
        "attributes_", tools::file_path_sans_ext(table_file), ".txt")
      
      # Each issue is logged as "required" or "optional"
      required_issues <- c()
      optional_issues <- c()
      
      # Attribute metadata is expected for each table
      r <- validate_table_attribute_template_presence(attribute_file, x)
      required_issues <- c(required_issues, r)
      
      if (attribute_file %in% names(x$template)) {
        
        # Downstream processes index these metadata by column name
        r <- validate_table_attribute_template_column_names(attribute_file, x)

        # All columns of a table should be listed
        r <- validate_table_attribute_name_presence(
          attribute_file, table_file, x)
        required_issues <- c(required_issues, r)
        
        # All columns of a table should be listed and in the same order
        r <- validate_table_attribute_name_order(
          attribute_file, table_file, x)
        required_issues <- c(required_issues, r)

        # Attributes are meaningless without definition
        r <- validate_table_attribute_definitions(attribute_file, x)
        required_issues <- c(required_issues, r)

        # All attributes must be assigned a class
        r <- validate_table_attribute_class_presence(attribute_file, x)
        required_issues <- c(required_issues, r)

        # All attributes must be assigned a class of the expected type
        r <- validate_table_attribute_class_type(attribute_file, x)
        required_issues <- c(required_issues, r)

        # Numeric attributes must have a unit
        r <- validate_table_attribute_unit_presence(attribute_file, x)
        required_issues <- c(required_issues, r)
        
        # Units are meaningless without definition
        r <- validate_table_attribute_unit_definition(attribute_file, x)
        required_issues <- c(required_issues, r)

        # Date attributes must have a format specifier
        r <- validate_table_attribute_date_format_presence(attribute_file, x)
        required_issues <- c(required_issues, r)

        # TODO: Date and time specifier represents one of the preferred formats
        # r <- validate_table_attribute_date_format_specifier(attribute_file, x)
        # optional_issues <- c(optional_issues, r)

        # Only one missing value code per attribute is supported
        r <- validate_table_attribute_missing_value_code_quantity(
          attribute_file, x)
        optional_issues <- c(optional_issues, r)

        # Missing value code is not a blank/white space
        r <- validate_table_attribute_missing_value_code_ws(
          attribute_file, x)
        required_issues <- c(required_issues, r)

        # Missing value code is meaningless without definition and vise versa
        r <- validate_table_attribute_missing_value_code_definition(
          attribute_file, x)
        required_issues <- c(required_issues, r)
        
        # An inaccurately assigned numeric class (i.e. contains characters 
        # other than the specified missing value code) will be reclassified
        # as character until the data are corrected
        r <- validate_table_attribute_false_numeric(
          attribute_file, table_file, x)
        optional_issues <- c(optional_issues, r$issues)
        if (!is.null(r$false_numeric_attributes)) {
          x$template[[attribute_file]]$content$class[
            x$template[[attribute_file]]$content$attributeName %in% 
              r$false_numeric_attributes] <<- "character"
        }

      }
      
      # A compiled report of issues helps the user fix them
      if (!is.null(required_issues)) {
        required_issues <- paste0(
          "\n",
          "Data table attributes (", table_file, ", Required) - Data table ",
          "attributes will be dropped from the EML until these issues are ",
          "fixed:\n",
          paste(
            paste0(seq_along(required_issues), ". "),
            required_issues,
            collapse = "\n"), 
          "\n")
      }
      if (!is.null(optional_issues)) {
        optional_issues <- paste0(
          "\n",
          "Data table attributes (", table_file, ", Optional):\n",
          paste(
            paste0(seq_along(optional_issues), ". "),
            optional_issues,
            collapse = "\n"), 
          "\n")
      }
      issues <- c(required_issues, optional_issues)
      
      # There's no recovering from required issues, and the most reasonable
      # fix is to drop the table attributes template from further use.
      if (!is.null(required_issues)) {
        x$template[[attribute_file]] <<- NULL
        x$data.table[[table_file]] <<- NULL
      }

      # Return
      issues
    })
  
  # Return
  list(issues = unlist(output), x = x)
  
}









#' Check for presence of the table attributes template
#'
#' @param file.name
#'     (character) The table attributes template to apply this function to. 
#'     Full file name is required.
#' @param x 
#'     (list) The data and metadata object returned by 
#'     \code{template_arguments()}.
#'
#' @return
#'     \item{character}{Description of validation issues}
#'     \item{NULL}{If no issues were found}
#'
validate_table_attribute_template_presence <- function(file.name, x) {
  if (!(file.name %in% names(x$template))) {
    paste0(
      "Missing attributes metadata. Attributes metadata describe important ", 
      "elements of a data table.")
  }
}








 
#' Check column names of table attributes template
#'
#' @param file.name
#'     (character) The categorical variables template to apply this function 
#'     to. Full file name is required.
#' @param x 
#'     (list) The data and metadata object returned by 
#'     \code{template_arguments()}.
#'
#' @return
#'     \item{error}{If column names are invalid}
#'     \item{NULL}{If no issues were found}
#'
validate_table_attribute_template_column_names <- function(file.name, x) {
  expected_colnames <- colnames(
    data.table::fread(
      system.file(
        '/templates/table_attributes.txt',
        package = 'EMLassemblyline')))
  found_colnames <- colnames(x$template[[file.name]]$content)
  if (!all(expected_colnames %in% found_colnames)) {
    stop(
      "Unexpected column names in ", file.name, ". Expected columns are: ",
      paste(expected_colnames, collapse = ", "), call. = FALSE)
  }
}








#' Check all column names of a data table are listed
#'
#' @param template.name
#'     (character) The table attributes template to apply this function 
#'     to. Full file name is required.
#' @param data.name
#'     (character) The data table to apply this function to. Full file name 
#'     is required.
#' @param x 
#'     (list) The data and metadata object returned by 
#'     \code{template_arguments()}.
#'
#' @return
#'     \item{character}{Description of validation issues}
#'     \item{NULL}{If no issues were found}
#'
validate_table_attribute_name_presence <- function(template.name, data.name, x) {
  expected_colnames <- colnames(x$data.table[[data.name]]$content)
  found_colnames <- x$template[[template.name]]$content$attributeName
  if (!all(expected_colnames %in% found_colnames)) {
    paste0(
      "Mising attribute names. These attributes are listed in the data but ",
      "not the metadata: ",
      paste(
        expected_colnames[!(expected_colnames %in% found_colnames)], 
        collapse = ", "), ". ")
  }
}








#' Check all column names are listed in the same order as in the data
#'
#' @param template.name
#'     (character) The table attributes template to apply this function 
#'     to. Full file name is required.
#' @param data.name
#'     (character) The data table to apply this function to. Full file name 
#'     is required.
#' @param x 
#'     (list) The data and metadata object returned by 
#'     \code{template_arguments()}.
#'
#' @return
#'     \item{character}{Description of validation issues}
#'     \item{NULL}{If no issues were found}
#'     
#' @details 
#'     This function will not run unless all attributes listed in 
#'     \code{template.name} are also listed in \code{data.name}.
#'
validate_table_attribute_name_order <- function(template.name, data.name, x) {
  all_columns_present <- is.null(
    validate_table_attribute_name_presence(
      template.name, data.name, x))
  if (all_columns_present) {
    expected_colnames <- colnames(x$data.table[[data.name]]$content)
    found_colnames <- x$template[[template.name]]$content$attributeName
    if (!all(found_colnames == expected_colnames)) {
      paste0(
        "Mismatched attribute order. Listed attributes are not in the ",
        "same order as the data. The expected order is: ",
        paste(expected_colnames, collapse = ", "), ". ")
    }
  }
}








#' Check for attribute definitions
#'
#' @param file.name
#'     (character) The table attribute template to apply this function 
#'     to. Full file name is required.
#' @param x 
#'     (list) The data and metadata object returned by 
#'     \code{template_arguments()}.
#'
#' @return
#'     \item{character}{Description of validation issues}
#'     \item{NULL}{If no issues were found}
#'
validate_table_attribute_definitions <- function(file.name, x) {
  missing_definitions <- 
    x$template[[file.name]]$content$attributeDefinition == ""
  if (any(missing_definitions)) {
    paste0(
      "Missing definitions. Attributes are meaningless without definition. ",
      "These attributes have missing definitions: ",
      paste(
        x$template[[file.name]]$content$attributeName[missing_definitions],
        collapse = ", "))
  }
}








#' Check for attribute class presence
#'
#' @param file.name
#'     (character) The table attribute template to apply this function 
#'     to. Full file name is required.
#' @param x 
#'     (list) The data and metadata object returned by 
#'     \code{template_arguments()}.
#'
#' @return
#'     \item{character}{Description of validation issues}
#'     \item{NULL}{If no issues were found}
#'
validate_table_attribute_class_presence <- function(file.name, x) {
  missing_classes <- x$template[[file.name]]$content$class == ""
  if (any(missing_classes)) {
    paste0(
      "Missing classes. These attributes have missing classes: ",
      paste(
        x$template[[file.name]]$content$attributeName[missing_classes],
        collapse = ", "))
  }
}








#' Check for attribute class type
#'
#' @param file.name
#'     (character) The table attribute template to apply this function 
#'     to. Full file name is required.
#' @param x 
#'     (list) The data and metadata object returned by 
#'     \code{template_arguments()}.
#'
#' @return
#'     \item{character}{Description of validation issues}
#'     \item{NULL}{If no issues were found}
#'
validate_table_attribute_class_type <- function(file.name, x) {
  classes <- x$template[[file.name]]$content$class
  invalid_classes <- 
    !(classes %in% c("Date", "character", "categorical", "numeric")) & 
    (classes != "")
  if (any(invalid_classes)) {
    paste0(
      "Unsupported classes. Supported classes are: numeric, character, ",
      "categorical, Date. These attributes have unsupported classes: ",
      paste(
        x$template[[file.name]]$content$attributeName[invalid_classes],
        collapse = ", "))
  }
}









#' Check each numeric attribute has a unit
#'
#' @param file.name
#'     (character) The table attribute template to apply this function 
#'     to. Full file name is required.
#' @param x 
#'     (list) The data and metadata object returned by 
#'     \code{template_arguments()}.
#'
#' @return
#'     \item{character}{Description of validation issues}
#'     \item{NULL}{If no issues were found}
#'
validate_table_attribute_unit_presence <- function(file.name, x) {
  missing_units <- 
    (tolower(x$template[[file.name]]$content$class) == "numeric") & 
    (x$template[[file.name]]$content$unit == "")
  if (any(missing_units)) {
    paste0(
      "Missing units. Attributes with a numeric class require units. ",
      "These attributes have missing units: ",
      paste(
        x$template[[file.name]]$content$attributeName[missing_units],
        collapse = ", "))
  }
}








#' Check for unit definitions
#'
#' @param file.name
#'     (character) The table attribute template to apply this function 
#'     to. Full file name is required.
#' @param x 
#'     (list) The data and metadata object returned by 
#'     \code{template_arguments()}.
#'
#' @return
#'     \item{character}{Description of validation issues}
#'     \item{NULL}{If no issues were found}
#'
validate_table_attribute_unit_definition <- function(file.name, x) {
  defined_units <- c(
    EML::get_unitList()$units$id,
    x$template$custom_units.txt$content$id)
  undefined_units <- 
    !((x$template[[file.name]]$content$unit %in% defined_units) | 
    (x$template[[file.name]]$content$unit == ""))
  if (any(undefined_units)) {
    paste0(
      "Undefined units. Units must be from the EML standard unit dictionary ",
      "or defined as custom units. The units of these attributes are ",
      "undefined: ",
      paste(
        x$template[[file.name]]$content$attributeName[undefined_units],
        collapse = ", "))
  }
}








#' Check presence of date and time format specifier
#'
#' @param file.name
#'     (character) The table attributes template to apply this function 
#'     to. Full file name is required.
#' @param x 
#'     (list) The data and metadata object returned by 
#'     \code{template_arguments()}.
#'
#' @return
#'     \item{character}{Description of validation issues}
#'     \item{NULL}{If no issues were found}
#'
validate_table_attribute_date_format_presence <- function(file.name, x) {
  missing_format_specifier <- 
    (tolower(x$template[[file.name]]$content$class) == "date") & 
    ((x$template[[file.name]]$content$dateTimeFormatString == "") |
       stringr::str_detect(
         x$template[[file.name]]$content$dateTimeFormatString, "^!.*!$"))
  if (any(missing_format_specifier)) {
    paste0(
      "Missing datetime format strings. Each attribute with a 'Date' ",
      "class must also specify a datetime format string. These attributes ",
      "have missing datetime format strings: ",
      paste(
        x$template[[file.name]]$content$attributeName[missing_format_specifier],
        collapse = ", "))
  }
}








# TODO: Date and time specifier represents one of the preferred formats
# validate_table_attribute_date_format_specifier(attribute_file, x)
# optional_issues <- c(optional_issues, r)








#' Check the quantity of missing value codes
#'
#' @param file.name
#'     (character) The table attributes template to apply this function 
#'     to. Full file name is required.
#' @param x 
#'     (list) The data and metadata object returned by 
#'     \code{template_arguments()}.
#'
#' @return
#'     \item{character}{Description of validation issues}
#'     \item{NULL}{If no issues were found}
#'
validate_table_attribute_missing_value_code_quantity <- function(
  file.name, x) {
  multiple_missing_value_codes <- stringr::str_detect(
    x$template[[file.name]]$content$missingValueCode,
    ",|;|:|\\|")
  if (any(multiple_missing_value_codes)) {
    paste0(
      "Multiple missing value codes. Only one is allowed. It appears there ",
      "may be more than one missing value code specified for these ",
      "attributes: ",
      paste(
        x$template[[file.name]]$content$attributeName[multiple_missing_value_codes],
        collapse = ", "))
  }
}








#' Check missing value code for white space
#'
#' @param file.name
#'     (character) The table attributes template to apply this function 
#'     to. Full file name is required.
#' @param x 
#'     (list) The data and metadata object returned by 
#'     \code{template_arguments()}.
#'
#' @return
#'     \item{character}{Description of validation issues}
#'     \item{NULL}{If no issues were found}
#'
validate_table_attribute_missing_value_code_ws <- function(file.name, x) {
  white_space_missing_value_codes <- stringr::str_detect(
    x$template[[file.name]]$content$missingValueCode,
    "^[:space:]$")
  if (any(white_space_missing_value_codes)) {
    paste0(
      "Unsupported missing value codes. White space (e.g. space, tab) is not ",
      "a supported missing value code. These attributes have white space ",
      "missing value codes: ",
      paste(
        x$template[[file.name]]$content$attributeName[white_space_missing_value_codes],
        collapse = ", "))
  }
}








#' Check each missing value code has a definition and vise versa
#'
#' @param file.name
#'     (character) The table attributes template to apply this function 
#'     to. Full file name is required.
#' @param x 
#'     (list) The data and metadata object returned by 
#'     \code{template_arguments()}.
#'
#' @return
#'     \item{character}{Description of validation issues}
#'     \item{NULL}{If no issues were found}
#'
validate_table_attribute_missing_value_code_definition <- function(
  file.name, x) {
  incomplete_code_definition <- 
    ((x$template[[file.name]]$content$missingValueCode == "") &
    (x$template[[file.name]]$content$missingValueCodeExplanation != "")) | 
    ((x$template[[file.name]]$content$missingValueCode != "") &
       (x$template[[file.name]]$content$missingValueCodeExplanation == ""))
  if (any(incomplete_code_definition)) {
    paste0(
      "Incomplete missing value code/definition pairs. Each missing value ",
      "must have a missing value code definition and vise versa. These ",
      "attributes are missing one or the other: ",
      paste(
        x$template[[file.name]]$content$attributeName[incomplete_code_definition],
        collapse = ", "))
  }
}








#' Check numeric attributes for non-numeric values
#'
#' @param template.name
#'     (character) The table attributes template to apply this function 
#'     to. Full file name is required.
#' @param data.name
#'     (character) The data table to apply this function to. Full file name 
#'     is required.
#' @param x 
#'     (list) The data and metadata object returned by 
#'     \code{template_arguments()}.
#'
#' @return
#'     \item{character or NULL}{A description of validation issues if any are
#'     found, otherwise NULL.}
#'     \item{x}{(list) A potentially modified \code{x} if relevant issues were
#'     found}
#'
validate_table_attribute_false_numeric <- function(
  template.name, data.name, x) {
  
  msg <- NULL
  numeric_attributes <- x$template[[template.name]]$content$attributeName[
    x$template[[template.name]]$content$class %in% "numeric"]
  false_numeric_attributes <- lapply(
    numeric_attributes,
    function(attribute) {
      row_number <- which(
        x$template[[template.name]]$content$attributeName %in% attribute)
      missing_value_code <- 
        x$template[[template.name]]$content$missingValueCode[row_number]
      column_data <- x$data.table[[data.name]]$content[[attribute]]
      column_data[column_data %in% missing_value_code] <- NA
      na_before_coercion <- sum(is.na(column_data))
      na_after_coercion <- suppressWarnings(sum(is.na(as.numeric(column_data))))
      if (na_after_coercion != na_before_coercion) {
        attribute
      }
    })
  false_numeric_attributes <- unlist(false_numeric_attributes)
  if (!is.null(false_numeric_attributes)) {
    msg <- paste0(
      "Inaccurate classes. These attributes are classified as 'numeric' but ",
      "in the data contain character strings other than those listed as ",
      "missing value codes in the data and will therefore be reclassified ",
      "as 'character': ", paste(false_numeric_attributes, collapse = ", "))
  }
  list(issues = msg, false_numeric_attributes = false_numeric_attributes)
}








#' Validate the taxonomic_coverage template
#'
#' @param x
#'     (named list) The data and metadata object returned by 
#'     \code{template_arguments()}.
#'
#' @return
#'     \item{issues}{(character) Descriptions of issues found in the template 
#'     grouped by "required" and "optional" criteria.}
#'     \item{x}{(named list) If "required" issues are found, then the template 
#'     is removed.}
#'     
#' @details 
#'     Checks performed by this function:
#'     \itemize{
#'         \item{Template column names}
#'         \item{Taxonomic authority}
#'         \item{Complete entries}
#'     }
#'     
#'     Manipulations performed by this function:
#'     \itemize{
#'         \item{Use raw names when a resolved name is missing. This ensures
#'         the taxon is not ommitted from the taxonomic coverage metadata.}
#'     }
#'
validate_taxonomic_coverage <- function(x) {
  
  # Objects for catching required and optional issues
  required_issues <- c()
  optional_issues <- c()

  if (any(names(x$template) == "taxonomic_coverage.txt")) {
    
    # Default manipulation - Use raw names when a resolved name is missing
    missing <- x$template$taxonomic_coverage.txt$content$name_resolved == ""
    x$template$taxonomic_coverage.txt$content$name_resolved[missing] <- 
      x$template$taxonomic_coverage.txt$content$name[missing]
    
    # Column names are correct
    r <- validate_taxonomic_coverage_column_names(x)

    # authority_system is supported
    r <- validate_taxonomic_coverage_authority_system(x)
    required_issues <- c(required_issues, r)
    
    # Complete entries are required
    r <- validate_taxonomic_coverage_completeness(x)
    required_issues <- c(required_issues, r)
    
  }
  
  # Compile issues
  if (!is.null(required_issues)) {
    required_issues <- paste0(
      "\n",
      "Taxonomic coverage (Required) - Taxonomic coverage will be dropped ",
      "from the EML until these issues are fixed:\n",
      paste(
        paste0(seq_along(required_issues), ". "),
        required_issues,
        collapse = "\n"), 
      "\n")
  }
  if (!is.null(optional_issues)) {
    optional_issues <- paste0(
      "\n",
      "Taxonomic coverage (Optional):\n",
      paste(
        paste0(seq_along(optional_issues), ". "),
        optional_issues,
        collapse = "\n"), 
      "\n")
  }
  issues <- c(required_issues, optional_issues)
  
  # Drop the taxonomic coverage template if required issues are found
  if (!is.null(required_issues)) {
    x$template$taxonomic_coverage.txt <- NULL
  }
  
  # Return
  list(issues = issues, x = x)
  
}








#' Check column names of taxonomic coverage template
#'
#' @param x 
#'     (list) The data and metadata object returned by 
#'     \code{template_arguments()}.
#'
#' @return
#'     \item{error}{If column names are invalid}
#'     \item{NULL}{If no issues were found}
#'
validate_taxonomic_coverage_column_names <- function(x) {
  template <- data.table::fread(
    system.file(
      '/templates/taxonomic_coverage.txt',
      package = 'EMLassemblyline'))
  expected_colnames <- colnames(template)
  found_colnames <- colnames(x$template$taxonomic_coverage.txt$content)
  if (!all(expected_colnames %in% found_colnames)) {
    stop(
      "Unsupported column names in taxonomic coverage template:\n",
      paste(
        found_colnames[!(found_colnames %in% expected_colnames)],
        collapse = ", "), "\n",
      "Expected column names:\n",
      paste(expected_colnames, collapse = ", "),
      call. = FALSE)
  }
}







#' Check authority_system values in the taxonomic coverage template
#'
#' @param x 
#'     (list) The data and metadata object returned by 
#'     \code{template_arguments()}.
#'
#' @return
#'     \item{error}{If column names are invalid}
#'     \item{NULL}{If no issues were found}
#'
validate_taxonomic_coverage_authority_system <- function(x) {
  gnr_list <- as.data.frame(taxize::gnr_datasources())
  authorities_supported <- gnr_list$title[
    gnr_list$id %in% taxonomyCleanr::view_taxa_authorities()$id]
  authorities_found <- 
    x$template$taxonomic_coverage.txt$content$authority_system
  unsupported_authorities <- !((authorities_found %in% authorities_supported) | 
    (authorities_found == ""))
  if (any(unsupported_authorities)) {
    paste0(
      "Unsupported authorities for entries: ",
      paste(which(unsupported_authorities), collapse = ", "), ". ",
      "Supported authorities are: ",
      paste(authorities_supported, collapse = ", "))
  }
}








#' Check completeness of the taxonomic coverage template
#'
#' @param x 
#'     (list) The data and metadata object returned by 
#'     \code{template_arguments()}.
#'
#' @return
#'     \item{error}{If column names are invalid}
#'     \item{NULL}{If no issues were found}
#'
validate_taxonomic_coverage_completeness <- function(x) {
  authorities <- x$template$taxonomic_coverage.txt$content$authority_system
  ids <- x$template$taxonomic_coverage.txt$content$authority_id
  incomplete_entries <- !((authorities != "")) == 
    ((ids != "") & (!is.na(ids)))
  if (any(incomplete_entries)) {
    paste0(
      "Missing inputs. A taxonomic authority and corresponding identifier is ",
      "missing from entries: ",
      paste(which(incomplete_entries), collapse = ", "))
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








#' Compile geographic coverage from multiple sources
#'
#' @param x 
#'     (list) The data and metadata object returned by 
#'     \code{template_arguments()}.
#'     
#' @return
#'     \item{x}{(list) With geographic coverage compiled from multiple input 
#'     sources into the geographic_coverage template.}
#'
#' @details 
#'     Combine multiple sources of geographic coverage and remove duplicate 
#'     entries. This info can be supplied in the \code{geographic.coverage} and 
#'     \code{geographic.description} arguments of \code{make_eml()} as well as 
#'     in the geographic_coverage and bounding_boxes templates.
#'     
compile_geographic_coverage <- function(x) {
  
  geographic.coordinates <- NULL
  geographic.description <- NULL
  
  make_eml_args <- try(sys.call(which = -3), silent = TRUE)
  if (class(make_eml_args) != "try-error") {
    if (!is.null(make_eml_args$geographic.coordinates) &
        !is.null(make_eml_args$geographic.description)) {
      geographic.coordinates <- make_eml_args$geographic.coordinates
      geographic.description <- make_eml_args$geographic.description
    }
  }
  
  x$template$geographic_coverage.txt$content <- unique.data.frame(
    rbind(
      data.frame(
        geographicDescription = character(0),
        northBoundingCoordinate = character(0),
        southBoundingCoordinate = character(0),
        eastBoundingCoordinate = character(0),
        westBoundingCoordinate = character(0),
        stringsAsFactors = F),
      data.frame(
        geographicDescription = as.character(geographic.description),
        northBoundingCoordinate = as.character(geographic.coordinates[1]),
        southBoundingCoordinate = as.character(geographic.coordinates[3]),
        eastBoundingCoordinate = as.character(geographic.coordinates[2]),
        westBoundingCoordinate = as.character(geographic.coordinates[4]),
        stringsAsFactors = F),
      data.frame(
        geographicDescription = x$template$bounding_boxes.txt$content$geographicDescription,
        northBoundingCoordinate = x$template$bounding_boxes.txt$content$northBoundingCoordinate,
        southBoundingCoordinate = x$template$bounding_boxes.txt$content$southBoundingCoordinate,
        eastBoundingCoordinate = x$template$bounding_boxes.txt$content$eastBoundingCoordinate,
        westBoundingCoordinate = x$template$bounding_boxes.txt$content$westBoundingCoordinate,
        stringsAsFactors = F),
      data.frame(
        geographicDescription = x$template$geographic_coverage.txt$content$geographicDescription,
        northBoundingCoordinate = x$template$geographic_coverage.txt$content$northBoundingCoordinate,
        southBoundingCoordinate = x$template$geographic_coverage.txt$content$southBoundingCoordinate,
        eastBoundingCoordinate = x$template$geographic_coverage.txt$content$eastBoundingCoordinate,
        westBoundingCoordinate = x$template$geographic_coverage.txt$content$westBoundingCoordinate,
        stringsAsFactors = F)))
  if (nrow(x$template$geographic_coverage.txt$content) == 0) {
    x$template$geographic_coverage.txt <- NULL
  }
  x
}








#' View validation issues
#'
#' @return
#'     A message listing any validation issues
#'     
#' @description 
#'     Validation functions return a list of validation issues to the global 
#'     environment in an \code{issues} object. The \code{view_issues()} 
#'     function wraps these issues in \code{message()} to provide a human 
#'     readable form.
#'     
#' @export
#'
issues <- function() {
  if (exists("template_issues", envir = .GlobalEnv)) {
    message(template_issues)
  } else {
    message("No issues found")
  }
}


# FIXME: Create function to remove user supplied NA from templates (a common 
# issue). EMLassemblyline should be smart enough to ignore these.
