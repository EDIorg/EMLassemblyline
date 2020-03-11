#' Initialize list of arguments and contents to \code{EMLassemblyline} functions
#'
#' @description  
#'     Initialize the list of arguments for EMLassemblyline functions including
#'     \code{x}, an argument containing metadata template and data file 
#'     contents. This approach enables programatic interfacing of upstream 
#'     metadata sources (e.g. 
#'     \href{https://github.com/lter/LTER-core-metabase}{LTER-core-metabase}) 
#'     to \code{EMLassemblyline} functions.
#'
#' @usage 
#'     template_arguments(
#'       path = NULL,
#'       data.path = NULL,
#'       data.table = NULL,
#'       other.entity = NULL,
#'       sep = NULL
#'     )
#'
#' @param path 
#'     (character) Path to the directory containing \code{EMLassemblyline} 
#'     metadata templates.
#' @param data.path
#'     (character) Path to the directory containing \code{data.table} and 
#'     \code{other.entity}.
#' @param data.table
#'     (character) Data table name. If more than one, then supply as a vector 
#'     of character strings (e.g. 
#'     \code{data.table = c('concentrations.csv', 'characteristics.csv')}).
#' @param other.entity
#'     (character) Other entity name. If more than one, then supply as a vector 
#'     of character strings (e.g. 
#'     \code{other.entity = c('ancillary_data.zip', 'quality_control.R')}).
#' @param sep
#'     (character) Data table field delimiter. Use this argument if 
#'     \code{template_arguments} fails to automatically identify field delimiters of 
#'     \code{data.table}.
#'
#' @details  
#'     The full named list structure is returned irrespective of NULL inputs,
#'     so content can be added downstream (e.g. \code{EMLassemblyline} templating 
#'     functions).
#'
#' @return 
#'     (named list) A list of all \code{EMLassemblyline} arguments with this
#'     structure:
#'     \itemize{
#'     \item{\strong{data.path} (character) Path to where the data 
#'     files are stored.}
#'     \item{\strong{data.table} (character) Data table name. If more 
#'     than one, supply as a vector of character strings (e.g. 
#'     \code{data.table = c('concentrations.csv', 'characteristics.csv')}).}
#'     \item{\strong{data.table.description} (character) Data table 
#'     description. Brief description of data tables. If more than 
#'     one data table, then supply as a vector of character strings in 
#'     the same order as listed in \code{data.table}.}
#'     \item{\strong{data.table.quote.character} (character) Quote 
#'     character used in data tables. If more than one, then supply as 
#'     a vector of character strings in the same order as listed in 
#'     \code{data.table}. This argument is required only if 
#'     \code{data.table} contain quotations. If the quote character is 
#'     a quotation, then enter \code{"\\""}. If the quote character is an 
#'     apostrophe, then enter \code{"\\'"}.}
#'     \item{\strong{data.url} (character) A publicly accessible URL to 
#'     data tables and other entities of this dataset.}
#'     \item{\strong{dataset.title} (character) Dataset title.}
#'     \item{\strong{dir.name} (character) Name of parent directory. 
#'     Subdirectories have predefined names.}
#'     \item{\strong{eml.path} (character) Path to where EML will be 
#'     written.}
#'     \item{\strong{geographic.coordinates} (character) Geographic coordinates 
#'     delineating the bounding area or point of a dataset, in decimal 
#'     degrees. This argument is not required if using 
#'     bounding_boxes.txt. Values must be listed in this order: 
#'     North, East, South, West. Longitudes West of the prime meridian 
#'     and latitudes South of the equator are negative. If representing 
#'     a point, repeat the latitude for North and South, and repeat the 
#'     longitude for East and West (e.g. 
#'     \code{geographic.coordinates = c('28.38', '-119.95', '28.38', '-119.95')}).}
#'     \item{\strong{geographic.description} (character) Geographic 
#'     description. Don't use this argument if using the 
#'     geographic_coverage.txt template.}
#'     \item{\strong{lat.col} (character) Name of latitude column.}
#'     \item{\strong{license} (character) License under which the data 
#'     will be released.}
#'     \item{\strong{lon.col} (character) Name of longitude column.}
#'     \item{\strong{maintenance.description} (character) Data collection 
#'     status. Can be "ongoing" or "completed".}
#'     \item{\strong{other.entity} (character) Other entity name. If 
#'     more than one, then supply as a vector of character strings. 
#'     Other entities should be located at \code{data.path}.}
#'     \item{\strong{other.entity.description} (character) Other entity 
#'     description. If more than one, then supply as a vector of 
#'     descriptions in the same order as listed in \code{other.entity}.}
#'     \item{\strong{package.id} (character) Data package identifier. 
#'     Missing \code{package.id} is assigned a default value of 
#'     'edi.101.1'. Any data repository package ID may be used.}
#'     \item{\strong{path} (character) Path to where template files are
#'     stored.}
#'     \item{\strong{provenance} (character) EDI Data Repository Data 
#'     package ID(s) corresponding to parent datasets from which this 
#'     dataset was created (e.g. 'knb-lter-cap.46.3'). If more than one, 
#'     then supply as a vector of character strings.}
#'     \item{\strong{return.obj} (logical) Return the EML as an R 
#'     object of class EML object.}
#'     \item{\strong{sep} (character) Field delimiter to be used when 
#'     reading data tables.}
#'     \item{\strong{site.col} (character) Name of site column, where
#'     site is the name of the location specified by \code{lat.col} and 
#'     \code{lon.col}.}
#'     \item{\strong{taxa.authority} (integer) An ordered numeric vector of 
#'     ID's corresponding to data sources (i.e. taxonomic authorities) 
#'     you'd like to query, in the order of decreasing preference. Run 
#'     \code{view_taxa_authorities()} to see supported data sources. Columns 
#'     "resolve_sci_taxa", and "resolve_comm_taxa" correspond to scientific 
#'     and common searches.}
#'     \item{\strong{taxa.col} (character) Name of column in 
#'     \code{taxa.table}containing taxonomic names.}
#'     \item{\strong{taxa.name.type} (character) Taxonomic name type. Can be: 
#'     "scientific", "common", or "both".}
#'     \item{\strong{taxa.table} (character) Name of data table containing 
#'     \code{taxa.col}.}
#'     \item{\strong{temporal.coverage} (character) Beginning and ending 
#'     dates of the dataset as a vector of character strings in the 
#'     format \strong{YYYY-MM-DD}.}
#'     \item{\strong{user.domain} (character) Domain of the 
#'     \code{user.id}. If more than one, then supply as a vector of 
#'     character strings in the same order corresponding to listed under
#'     \code{user.id}. Valid options for EDI are "LTER" and "EDI". Other 
#'     data repository IDs may be used. NULL user.domain defaults to 
#'     "someuserdomain".}
#'     \item{\strong{user.id} (character) ID of data repository user 
#'     account. If more than one, supply as a vector of character 
#'     strings. NULL user.id defaults to "someuserid".}
#'     \item{\strong{write.file} (logical) Write file to 
#'     \code{eml.path}.}
#'     \item{\strong{x} (named list) List of metadata template, data 
#'     tables, and other entities.}
#'       \itemize{
#'       \item{\strong{template} - Metadata template files}
#'         \itemize{
#'         \item{\strong{abstract.txt} Abstract template}
#'           \itemize{
#'           \item{\strong{content} \code{list} containing 
#'           abstract, NA otherwise}
#'           }
#'         \item{\strong{additional_info.txt} Additional information template}
#'           \itemize{
#'           \item{\strong{content} \code{list} containing 
#'           additional info, NA otherwise}
#'           }
#'         \item{\strong{attributes_name.txt} Attributes template where 
#'         "name" is the corresponding \code{data.table}.}
#'           \itemize{
#'           \item{\strong{content} Data frame of attributes template, 
#'           NA otherwise.}
#'           }
#'         \item{\strong{bounding_boxes.txt} Bounding boxes template}
#'           \itemize{
#'           \item{\strong{content} Data frame of bounding boxes 
#'           template, NA otherwise.}
#'           }
#'         \item{\strong{catvars_name.txt} Categorical variables template 
#'           where "name" is the corresponding \code{data.table}.}
#'           \itemize{
#'           \item{\strong{content} Data frame of categorical variables 
#'           template, NA otherwise.}
#'           }
#'         \item{\strong{custom_units.txt} Custom units template}
#'           \itemize{
#'           \item{\strong{content} Data frame of custom units template, 
#'           NA otherwise.}
#'           }
#'         \item{\strong{geographic_coverage.txt} Geographic coverage 
#'           template}
#'           \itemize{
#'           \item{\strong{content} Data frame of geographic coverage 
#'           template, NA otherwise.}
#'           }
#'         \item{\strong{intellectual_rights.txt} Intellectual rights 
#'           template}
#'           \itemize{
#'           \item{\strong{content} \code{list} containing 
#'           intellectual rights template, NA otherwise.}
#'           }
#'         \item{\strong{keywords.txt} Keywords template}
#'           \itemize{
#'           \item{\strong{content} Data frame of keywords template, 
#'           NA otherwise.}
#'           }
#'         \item{\strong{methods.txt} Methods template}
#'           \itemize{
#'           \item{\strong{content} \code{list} containing 
#'           methods, NA otherwise.}
#'           }
#'         \item{\strong{personnel.txt} Personnel template}
#'           \itemize{
#'           \item{\strong{content} Data frame of personnel template, 
#'           NA otherwise.}
#'           }
#'         \item{\strong{taxonomicCoverage.xml} Taxonomic coverage EML 
#'           element}
#'           \itemize{
#'           \item{\strong{content} \code{list}, 
#'           NA otherwise}
#'           }     
#'           }
#'        \item{\strong{data.table} Data tables}
#'        \itemize{
#'           \item{\strong{data.table.name.ext} Name of data table including 
#'           file extension, NULL otherwise.}
#'           \itemize{
#'                 \item{\strong{content} Data frame of data table.}
#'            }
#'          }
#'        \item{\strong{other.entity} Other entities}
#'        \itemize{
#'            \item{\strong{other.entity.name.ext} Name of other entity 
#'            including file extension, NULL otherwise.}
#'            \itemize{
#'                 \item{\strong{content} NA}
#'            }
#'        }
#'      }
#'     }
#'     
#' @export
#'

template_arguments <- function(
  path = NULL, 
  data.path = NULL, 
  data.table = NULL,
  other.entity = NULL, 
  sep = NULL) {
  
  # Validate arguments --------------------------------------------------------
  
  validate_arguments(
    fun.name = 'template_arguments',
    fun.args = as.list(environment()))
  
  # Parameterize --------------------------------------------------------------
  
  # Get attributes of template files and arguments
  
  attr_arg <- data.table::fread(
    file = system.file(
      '/templates/arguments.txt',
      package = 'EMLassemblyline'),
    fill = TRUE,
    blank.lines.skip = TRUE)
  
  attr_tmp <- data.table::fread(
    system.file(
      '/templates/template_characteristics.txt',
      package = 'EMLassemblyline'), 
    fill = TRUE,
    blank.lines.skip = TRUE)
  
  # Initialize arguments ------------------------------------------------------
  
  output <- vector('list', nrow(attr_arg))
  names(output) <- attr_arg$argument_name

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
  }
  
  # Combine initialized components --------------------------------------------
  # Combine initialized arguments, data tables, and other entities. Return the
  # list object if there are no templates to add.
  
  if (is.null(path)) {
    output$x <- list(
      template = NULL,
      data.table = data_tables,
      other.entity = other_entities)
    return(output)
  } else {
    output$x <- list(
      template = templates,
      data.table = data_tables,
      other.entity = other_entities)
  }
  
  # Read templates ------------------------------------------------------------
  
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
  
  templates <- names(output$x$template)
  for (i in 1:length(templates)){
    
    # Read abstract -----------------------------------------------------------
    
    if (stringr::str_detect(
      templates[i], 
      attr_tmp$regexpr[attr_tmp$template_name == "abstract"])) {
      output$x$template[[i]]$content <- read_txt(
        paste0(path, '/', templates[i]))
    }
    
    # Read additional information ---------------------------------------------
    
    if (stringr::str_detect(
      templates[i], 
      attr_tmp$regexpr[attr_tmp$template_name == "additional_info"])) {
      output$x$template[[i]]$content <- read_txt(
        paste0(path, '/', templates[i]))
    }
    
    # Read attributes (data table) --------------------------------------------
    
    if (stringr::str_detect(
      templates[i], 
      attr_tmp$regexpr[attr_tmp$template_name == "attributes"])) {
      output$x$template[[i]]$content <- read_tbl(
        paste0(path, "/", templates[i]))
    }
    
    # Read categorical variables ----------------------------------------------
    
    if (stringr::str_detect(
      templates[i], 
      attr_tmp$regexpr[attr_tmp$template_name == "catvars"])) {
      output$x$template[[i]]$content <- read_tbl(
        paste0(path, "/", templates[i]))
    }
    
    # Read custom units -------------------------------------------------------
    
    if (stringr::str_detect(
      templates[i], 
      attr_tmp$regexpr[attr_tmp$template_name == "custom_units"])) {
      output$x$template[[i]]$content <- read_tbl(
        paste0(path, "/", templates[i]))
    }

    # Read geographic bounding boxes ------------------------------------------
    
    if (stringr::str_detect(
      templates[i], 
      attr_tmp$regexpr[attr_tmp$template_name == "bounding_boxes"])) {
      output$x$template[[i]]$content <- read_tbl(
        paste0(path, "/", templates[i]))
    }

    # Read geographic coverage ------------------------------------------------
    
    if (stringr::str_detect(
      templates[i], 
      attr_tmp$regexpr[attr_tmp$template_name == "geographic_coverage"])) {
      output$x$template[[i]]$content <- read_tbl(
        paste0(path, "/", templates[i]))
    }
    
    # Read intellectual rights ------------------------------------------------
    
    if (stringr::str_detect(
      templates[i], 
      attr_tmp$regexpr[attr_tmp$template_name == "intellectual_rights"])) {
      output$x$template[[i]]$content <- read_txt(
        paste0(path, '/', templates[i]))
    }
    
    # Read keywords -----------------------------------------------------------
    
    if (stringr::str_detect(
      templates[i], 
      attr_tmp$regexpr[attr_tmp$template_name == "keywords"])) {
      output$x$template[[i]]$content <- read_tbl(
        paste0(path, "/", templates[i]))
    }
    
    # Read methods ------------------------------------------------------------
    
    if (stringr::str_detect(
      templates[i], 
      attr_tmp$regexpr[attr_tmp$template_name == "methods"])) {
      output$x$template[[i]]$content <- read_txt(
        paste0(path, '/', templates[i]))
    }
    
    # Read personnel ----------------------------------------------------------
    
    if (stringr::str_detect(
      templates[i], 
      attr_tmp$regexpr[attr_tmp$template_name == "personnel"])) {
      output$x$template[[i]]$content <- read_tbl(
        paste0(path, "/", templates[i]))
    }
    
    # Read taxonomic coverage -------------------------------------------------
    
    if (stringr::str_detect(
      templates[i], 
      attr_tmp$regexpr[attr_tmp$template_name == "taxonomicCoverage"])) {
      output$x$template[[i]]$content <- EML::read_eml(
        paste0(path, '/', templates[i]))
      output$x$template$taxonomicCoverage.xml$content <- list(
        taxonomicClassification = output$x$template$taxonomicCoverage.xml$content$taxonomicClassification)
    } else if (stringr::str_detect(
      templates[i], 
      attr_tmp$regexpr[attr_tmp$template_name == "taxonomic_coverage"])) {
      output$x$template[[i]]$content <- read_tbl(
        paste0(path, "/", templates[i]))
    }

  }
  
  # Read data tables ----------------------------------------------------------
  # FIXME: Update table readers
  # If data tables exist ...
  
  if (!is.null(data.table)){
    
    # For all data tables ...
    
    for (i in 1:length(output$x$data.table)){
      
      # If delimiter is undefined ...
      
      if (is.null(sep)){
        
        delim_guess <- suppressWarnings(
          EDIutils::detect_delimeter(
            path = data.path, 
            data.files = names(output$x$data.table[i]), 
            os = EDIutils::detect_os()
          )
        )
        
        output$x$data.table[[i]]$content <- as.data.frame(
          data.table::fread(
            file = paste0(data.path, '/', names(output$x$data.table[i])),
            fill = TRUE,
            blank.lines.skip = TRUE
          )
        )
        
        # If delimiter is defined ...
        
      } else {
        
        output$x$data.table[[i]]$content <- utils::read.table(
          file = paste0(
            data.path,
            '/',
            names(output$x$data.table[i])
          ),
          header = T,
          sep = sep,
          quote = "\"",
          as.is = TRUE,
          comment.char = ""
        )
        
      }
      
    }

  }
  
  # Read other entities -------------------------------------------------------
  
  # If other entities exist ...
  
  if (!is.null(other.entity)){
    
    # For all other entities ...
    
    for (i in 1:length(output$x$other.entity)){
      
      output$x$other.entity[[i]]$content <- NA
      
    }
    
  }
  
  # Return --------------------------------------------------------------------
  
  output
  
}
