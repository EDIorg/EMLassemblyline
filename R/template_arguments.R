#' Initialize inputs to \code{EMLassemblyline} functions
#'
#' @description  
#'     Initialize the full list of possible inputs to \code{EMLassemblyline}
#'     functions (i.e. function arguments, metadata templates, and data 
#'     objects). This enables bridging of upstream metadata and data sources 
#'     (e.g. metabases, databases, Excel files, web forms, etc.).
#'
#' @param path 
#'     (character) Path to the directory containing \code{EMLassemblyline} 
#'     metadata templates.
#' @param data.path
#'     (character) Path to the directory containing \code{data.table} and 
#'     \code{other.entity} data objects.
#' @param data.table
#'     (character) File names of data tables. If more than one, then supply as 
#'     a vector (e.g. \code{data.table = c('decomp.csv', 'nitrogen.csv')}).
#' @param other.entity
#'     (character) File names of other entity data objects. If more than one, 
#'     then supply as a vector (e.g. 
#'     \code{other.entity = c('ancillary_data.zip', 'processing_and_analysis.R')}).
#' @param sep
#'     (character) Data table field delimiter. Use this if this function fails
#'     to read your \code{data.table}.
#' @param empty
#'     (logical) Initialize the output with a set of empty metadata templates?
#'     This option is useful when wanting to transfer metadata directly into
#'     the template files without having to call the templating functions.
#'
#' @return 
#'     (named list) A list of all \code{EMLassemblyline} arguments, specified 
#'     metadata templates and data objects.
#'     
#' @examples
#' # Output arguments and empty versions of metadata templates
#' 
#' r <- template_arguments(empty = TRUE)
#' 
#' # Output arguments, completed versions of metadata templates, and data objects
#' 
#' mydir <- file.copy(
#'   system.file('/examples/pkg_260', package = 'EMLassemblyline'), 
#'   tempdir(),
#'   recursive = TRUE)
#' 
#' setwd(paste0(tempdir(), "/pkg_260"))
#' 
#' r <- template_arguments(
#'   path = "./metadata_templates",
#'   data.path = "./data_objects",
#'   data.table = c("decomp.csv", "nitrogen.csv"),
#'   other.entity = c("ancillary_data.zip", "processing_and_analysis.R"))
#'     
#' @export
#'

template_arguments <- function(
  path = NULL, 
  data.path = NULL, 
  data.table = NULL,
  other.entity = NULL, 
  sep = NULL,
  empty = FALSE) {
  
  # Validate arguments --------------------------------------------------------
  
  validate_arguments(
    fun.name = 'template_arguments',
    fun.args = as.list(environment()))
  
  # Parameterize --------------------------------------------------------------
  
  # If empty = TRUE, then set path to /inst/templates so empty templates can
  # be read.
  
  if (isTRUE(empty)) {
    path <- system.file("/templates", package = "EMLassemblyline")
  }
  
  # Get attributes of template files and arguments
  
  attr_arg <- data.table::fread(
    file = system.file(
      '/templates/arguments.txt',
      package = 'EMLassemblyline'),
    fill = TRUE,
    blank.lines.skip = TRUE)
  
  attr_tmp <- read_template_attributes()
  
  # Initialize arguments ------------------------------------------------------
  
  output <- vector('list', nrow(attr_arg))
  names(output) <- attr_arg$argument_name
  
  # Initialize templates ------------------------------------------------------
  
  # Distinguish metadata templates from other files located at path.
  
  if (!is.null(path)) {
    path_files <- list.files(path)
    if (!length(path_files) == 0) {
      is_template <- rep(FALSE, length(path_files))
      for (i in 1:length(path_files)){
        is_template[i] <- any(
          stringr::str_detect(path_files[i], attr_tmp$regexpr))
      }
      templates <- vector('list', length(path_files[is_template]))
      names(templates) <- path_files[is_template]
    }
  } else {
    templates <- NULL
  }
  
  # Add empty versions of all metadata templates if argument empty = TRUE.
  # Only include .txt extensions to prevent duplication.
  
  if (isTRUE(empty)) {
    path_files <- list.files(path)
    is_template <- rep(FALSE, length(path_files))
    for (i in 1:length(path_files)){
      is_template[i] <- any(
        stringr::str_detect(path_files[i], attr_tmp$regexpr))
    }
    templates <- vector('list', length(path_files[is_template]))
    names(templates) <- path_files[is_template]
    templates[stringr::str_detect(names(templates), ".docx|.md")] <- NULL
  }
  
  # Initialize data tables ----------------------------------------------------
  
  if (!is.null(data.table)){
    data_table <- EDIutils::validate_file_names(
      path = data.path,
      data.files = data.table)
    data_tables <- vector('list', length(data_table))
    names(data_tables) <- data_table
  } else {
    data_tables <- NULL
  }
  
  # Initialize other entities -------------------------------------------------
  
  if (!is.null(other.entity)){
    other_entity <- EDIutils::validate_file_names(
      path = data.path,
      data.files = other.entity)
    other_entities <- vector('list', length(other_entity))
    names(other_entities) <- other_entity
  } else {
    other_entities <- NULL
  }
  
  # Read templates ------------------------------------------------------------
  
  if (!is.null(path)) {
    
    # Helper functions for reading templates
    
    read_tbl <- function(f) {
      as.data.frame(
        data.table::fread(
          file = f,
          fill = TRUE,
          blank.lines.skip = TRUE,
          sep = "\t",
          colClasses = rep(
            "character", 
            max(utils::count.fields(f, sep = "\t")))))
    }
    
    read_txt <- function(f) {
      if (stringr::str_detect(
        basename(f), 
        paste(
          attr_tmp$regexpr[
            (attr_tmp$type == "text") & (attr_tmp$template_name != "methods")],
          collapse = "|"))) {
        EML::set_TextType(file = f)
      } else if (stringr::str_detect(
        basename(f), 
        attr_tmp$regexpr[attr_tmp$template_name == "methods"])) {
        EML::set_methods(methods_file = f)
      }
    }
    
    # Loop through each metadata template found at path
    
    tfound <- names(templates)
    for (i in 1:length(tfound)){
      
      # Read abstract ---------------------------------------------------------
      
      if (stringr::str_detect(
        tfound[i], 
        attr_tmp$regexpr[attr_tmp$template_name == "abstract"])) {
        templates[[i]]$content <- read_txt(
          paste0(path, '/', tfound[i]))
      }
      
      # Read additional information -------------------------------------------
      
      if (stringr::str_detect(
        tfound[i], 
        attr_tmp$regexpr[attr_tmp$template_name == "additional_info"])) {
        templates[[i]]$content <- read_txt(
          paste0(path, '/', tfound[i]))
      }
      
      # Read attributes (data table) ------------------------------------------
      
      if (stringr::str_detect(
        tfound[i], 
        attr_tmp$regexpr[attr_tmp$template_name == "attributes"])) {
        templates[[i]]$content <- read_tbl(
          paste0(path, "/", tfound[i]))
      }
      
      # Read categorical variables --------------------------------------------
      
      if (stringr::str_detect(
        tfound[i], 
        attr_tmp$regexpr[attr_tmp$template_name == "catvars"])) {
        templates[[i]]$content <- read_tbl(
          paste0(path, "/", tfound[i]))
      }
      
      # Read custom units -----------------------------------------------------
      
      if (stringr::str_detect(
        tfound[i], 
        attr_tmp$regexpr[attr_tmp$template_name == "custom_units"])) {
        templates[[i]]$content <- read_tbl(
          paste0(path, "/", tfound[i]))
      }
      
      # Read geographic bounding boxes ----------------------------------------
      
      if (stringr::str_detect(
        tfound[i], 
        attr_tmp$regexpr[attr_tmp$template_name == "bounding_boxes"])) {
        templates[[i]]$content <- read_tbl(
          paste0(path, "/", tfound[i]))
      }
      
      # Read geographic coverage ----------------------------------------------
      
      if (stringr::str_detect(
        tfound[i], 
        attr_tmp$regexpr[attr_tmp$template_name == "geographic_coverage"])) {
        templates[[i]]$content <- read_tbl(
          paste0(path, "/", tfound[i]))
      }
      
      # Read intellectual rights ----------------------------------------------
      
      if (stringr::str_detect(
        tfound[i], 
        attr_tmp$regexpr[attr_tmp$template_name == "intellectual_rights"])) {
        templates[[i]]$content <- read_txt(
          paste0(path, '/', tfound[i]))
      }
      
      # Read keywords ---------------------------------------------------------
      
      if (stringr::str_detect(
        tfound[i], 
        attr_tmp$regexpr[attr_tmp$template_name == "keywords"])) {
        templates[[i]]$content <- read_tbl(
          paste0(path, "/", tfound[i]))
      }
      
      # Read methods ----------------------------------------------------------
      
      if (stringr::str_detect(
        tfound[i], 
        attr_tmp$regexpr[attr_tmp$template_name == "methods"])) {
        templates[[i]]$content <- read_txt(
          paste0(path, '/', tfound[i]))
      }
      
      # Read personnel --------------------------------------------------------
      
      if (stringr::str_detect(
        tfound[i], 
        attr_tmp$regexpr[attr_tmp$template_name == "personnel"])) {
        templates[[i]]$content <- read_tbl(
          paste0(path, "/", tfound[i]))
      }
      
      # Read taxonomic coverage -----------------------------------------------
      
      if (stringr::str_detect(
        tfound[i], 
        attr_tmp$regexpr[attr_tmp$template_name == "taxonomicCoverage"])) {
        templates[[i]]$content <- list(
          taxonomicClassification = EML::read_eml(
            paste0(path, '/', tfound[i]))$taxonomicClassification)
      } else if (stringr::str_detect(
        tfound[i], 
        attr_tmp$regexpr[attr_tmp$template_name == "taxonomic_coverage"])) {
        templates[[i]]$content <- read_tbl(
          paste0(path, "/", tfound[i]))
      }
      
    }
    
  }
  
  # Read data tables ----------------------------------------------------------
  
  if (!is.null(data.table)) {
    for (i in 1:length(data.table)) {
      if (is.null(sep)){
        data_tables[[i]]$content <- as.data.frame(
          data.table::fread(
            file = paste0(data.path, "/", data.table[i]),
            fill = TRUE,
            blank.lines.skip = TRUE))
      } else {
        data_tables[[i]]$content <- utils::read.table(
          file = paste0(data.path, "/", data.table[i]),
          header = T,
          sep = sep,
          quote = "\"",
          as.is = TRUE,
          comment.char = "")
      }
    }
  }
  
  # Read other entities -------------------------------------------------------
  
  # if (!is.null(other.entity)) {
  #   # for (i in 1:length(other.entity)) {
  #   #   other_entities[[i]]$content <- NA
  #   # }
  # }
  
  # Combine components & return -----------------------------------------------
  
  output$x <- list(
    template = templates,
    data.table = data_tables,
    other.entity = other_entities)
  
  output
  
}
