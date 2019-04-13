#' Initialize list of arguments for EMLassemblyline functions
#'
#' @description  
#'     Initialize the list of arguments for EMLassemblyline functions including
#'     \code{x}, an argument containing metadata template and data file 
#'     content. This approach enables programatic interfacing of upstream 
#'     metadata sources (e.g. LTER-core-metabase 
#'     \link{https://github.com/lter/LTER-core-metabase}) to 
#'     \code{EMLassemblyline} functions.
#'
#' @usage 
#'     make_arguments(
#'       path = NULL,
#'       data.path = NULL,
#'       data.table = NULL,
#'       other.entity = NULL,
#'       sep = NULL
#'     )
#'
#' @param path 
#'     (character) Path to metadata templates.
#' @param data.path
#'     (character) Path to data tables and other entities.
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
#'     \code{make_arguments} fails to automatically identify field delimiters of 
#'     \code{data.table}.
#'
#' @note 
#'     The full named list structure is returned irrespective of NULL inputs,
#'     so content can be added downstream. If valid inputs are supplied, then
#'     the respective list structures are populated. Pass the output of this 
#'     function to \code{make_template_core} to initilize (or add) core 
#'     template data frames, text objects, or XML objects.
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
#'     a quotation, then enter "\\"". If the quote character is an 
#'     apostrophe, then enter "\\'".}
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
#'     \code{bounding_boxes.txt}. Values must be listed in this order: 
#'     North, East, South, West. Longitudes West of the prime meridian 
#'     and latitudes South of the equator are negative. If representing 
#'     a point, repeat the latitude for North and South, and repeat the 
#'     longitude for East and West (e.g. 
#'     \code{geographic.coordinates = c('28.38', '-119.95', '28.38', '-119.95')}).}
#'     \item{\strong{geographic.description} (character) Geographic 
#'     description. Don't use this argument if using the 
#'     \code{bounding_boxes.txt} template.}
#'     \item{\strong{lat.col} (character) Name of latitude column.}
#'     \item{\strong{license} (character) License under which the data 
#'     will be released.}
#'     \item{\strong{lon.col} (character) Name of longitude column.}
#'     \item{\strong{maintenance.description} (character) Indicator of 
#'     whether data collection is 'ongoing' or 'completed'.}
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
#'     object of class \code{EML object}.}
#'     \item{\strong{sep} (character) Field delimiter to be used when 
#'     reading data tables.}
#'     \item{\strong{site.col} (character) Name of site column, where
#'     site is the name of the location specified by \code{lat.col} and 
#'     \code{lon.col}.}
#'     \item{\strong{temporal.coverage} (character) Beginning and ending 
#'     dates of the dataset as a vector of character strings in the 
#'     format \code{YYYY-MM-DD}.}
#'     \item{\strong{user.domain} (character) Domain of the 
#'     \code{user.id}. If more than one, then supply as a vector of 
#'     character strings in the same order corresponding to listed under
#'     \code{user.id}. Valid options for EDI are 'LTER' and 'EDI'. Other 
#'     data repository IDs may be used. NULL user.domain defaults to 
#'     'someuserdomain'.}
#'     \item{\strong{user.id} (character) ID of data repository user 
#'     account. If more than one, supply as a vector of character 
#'     strings. NULL user.id defaults to 'someuserid'.}
#'     \item{\strong{write.file} (logical) Write file to 
#'     \code{eml.path}.}
#'     \item{\strong{x} (named list) List of metadata template, data 
#'     tables, and other entities.}
#'       \itemize{
#'       \item{\strong{template} - Metadata template files}
#'         \itemize{
#'         \item{\strong{abstract.txt} Abstract template}
#'           \itemize{
#'           \item{\strong{content} \code{EML::TextType} containing 
#'           abstract, NA otherwise}
#'           }
#'         \item{\strong{additional_info.txt} Additional information template}
#'           \itemize{
#'           \item{\strong{content} \code{EML::TextType} containing 
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
#'           \item{\strong{content} \code{EML::TextType} containing 
#'           intellectual rights template, NA otherwise.}
#'           }
#'         \item{\strong{keywords.txt} Keywords template}
#'           \itemize{
#'           \item{\strong{content} Data frame of keywords template, 
#'           NA otherwise.}
#'           }
#'         \item{\strong{methods.txt} Methods template}
#'           \itemize{
#'           \item{\strong{content} \code{EML::Methods} containing 
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
#'           \item{\strong{content} \code{EML::taxonomicCoverage}, 
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
#'             
#'             }
#'     }
#'     
#' @export
#'

make_arguments <- function(
  path = NULL, 
  data.path = NULL, 
  data.table = NULL,
  other.entity = NULL, 
  sep = NULL
  ){
  
  # Validate arguments --------------------------------------------------------
  
  validate_arguments(
    fun.name = 'make_arguments',
    fun.args = as.list(environment())
  )
  
  # Parameterize --------------------------------------------------------------
  
  # Get template file and argument attributes
  
  attr.templates <- utils::read.table(
    file = system.file(
      '/templates/template_characteristics.txt',
      package = 'EMLassemblyline'
    ), 
    header = T,
    sep = '\t',
    as.is = T
  )
  
  attr.args <- utils::read.table(
    file = system.file(
      '/templates/arguments.txt',
      package = 'EMLassemblyline'
    ), 
    header = T,
    sep = '\t',
    as.is = T
  )
  
  # Initialize arguments ------------------------------------------------------
  
  arguments <- vector(
    'list',
    nrow(attr.args)
  )
  
  names(arguments) <- attr.args$argument_name

  # Initialize data tables ----------------------------------------------------
  
  if (!is.null(data.table)){
    
      data_table <- EDIutils::validate_file_names(
        path = data.path,
        data.files = data.table
      )
      
      data_tables <- vector('list', length(data_table))
      
      names(data_tables) <- data_table
      
  } else {
    
    data_tables <- NULL
    
  }
  
  # Initialize other entities -------------------------------------------------
  
  if (!is.null(other.entity)){
    
    other_entity <- EDIutils::validate_file_names(
      path = data.path,
      data.files = other.entity
    )
    
    other_entities <- vector('list', length(other_entity))
    
    names(other_entities) <- other_entity
    
  } else {
    
    other_entities <- NULL
    
  }
  
  # Initialize templates ------------------------------------------------------
  
  if (!is.null(path)){
    
    path_files <- list.files(path)
    
    # If template files exist ...
    
    if (!length(path_files) == 0){
      
      is_template <- rep(FALSE, length(path_files))
      
      for (i in 1:length(path_files)){
        
        is_template[i] <- any(
          stringr::str_detect(
            path_files[i], 
            attr.templates$regexpr
          )
        )
        
      }
      
      templates <- vector(
        'list', 
        length(path_files[is_template])
      )
      
      names(templates) <- path_files[is_template]
      
    }
    
  } else {
    
    path_files <- ''
    
  }
  
  # Add missing core templates
  
  missing_core_templates <- c()
  
  i_core <- seq(nrow(attr.templates))[
    attr.templates$core_template == TRUE
    ]
  
  for (i in 1:length(i_core)){
    
    use_i <- stringr::str_detect(
      path_files,
      attr.templates$regexpr[i_core[i]]
    )
    
    # If missing then add it
    
    if (!any(use_i)){
      
      missing_template <- vector('list', 1)
      
      names(missing_template) <- attr.templates$regexpr[i_core[i]]
      
      missing_core_templates <- c(missing_core_templates, missing_template)
      
    }
    
  }
  
  if (length(missing_core_templates) > 0){
    
    if (exists('templates')){
      
      templates <- c(templates, missing_core_templates)
      
    } else {
      
      templates <- missing_core_templates
      
    }
    
  }
  
  # Combine initialized components --------------------------------------------
  
  arguments$x <- list(
    template = templates,
    data.table = data_tables,
    other.entity = other_entities
  )
  
  output <- arguments
  
  # Read templates ------------------------------------------------------------
  # Most templates require unique read calls, therefore we don't have
  # generalized read functions (yet).
  
  templates <- names(output$x$template)
  
  # For all templates ...
  
  for (i in 1:length(templates)){
    
    # Read abstract -----------------------------------------------------------
    
    if (stringr::str_detect(string = templates[i], pattern = 'abstract.txt')){
      
      if (file.exists(paste0(path, '/', templates[i]))){
        
        output$x$template[[i]]$content <- methods::as(
          EML::set_TextType(
            file = paste0(
              path, 
              '/', 
              templates[i]
            )
          ),
          'abstract'
        )
        
      } else {
        
        output$x$template[[i]]$content <- NA_character_
        
      }

    }
    
    # Read additional information ---------------------------------------------
    
    if (stringr::str_detect(string = templates[i], pattern = 'additional_info.txt')){
      
      if (file.exists(paste0(path, '/', templates[i]))){
        
        output$x$template[[i]]$content <- methods::as(
          EML::set_TextType(
            file = paste0(
              path, 
              '/', 
              templates[i]
            )
          ),
          'additionalInfo'
        )
        
      } else {
        
        output$x$template[[i]]$content <- NA_character_
        
      }
      
    }
    
    # Read attributes (data table) --------------------------------------------
    
    if (stringr::str_detect(string = templates[i], pattern = 'attributes_[:graph:]*.txt')){
      
      if (file.exists(paste0(path, '/', templates[i]))){
        
        output$x$template[[i]]$content <- utils::read.table(
          file = paste0(
            path, 
            '/', 
            templates[i]
          ), 
          header = TRUE,
          sep = '\t',
          quote = "\"",
          as.is = TRUE,
          comment.char = "",
          fill = T,
          colClasses = rep("character", 7)
        )
        
        output$x$template[[i]]$content <- output$x$template[[i]]$content[ ,1:7]
        
        colnames(output$x$template[[i]]$content) <- c(
          "attributeName",
          "attributeDefinition",
          "class",
          "unit",
          "dateTimeFormatString",
          "missingValueCode",
          "missingValueCodeExplanation"
        )
        
      } else {
        
        output$x$template[[i]]$content <- NA_character_
        
      }
      
    }
    
    # Read categorical variables ----------------------------------------------
    
    if (stringr::str_detect(string = templates[i], pattern = 'catvars_[:graph:]*.txt')){
      
      if (file.exists(paste0(path, '/', templates[i]))){
        
        output$x$template[[i]]$content <- utils::read.table(
          paste0(
            path, 
            '/', 
            templates[i]
          ),
          header = T,
          sep="\t",
          quote="\"",
          as.is=TRUE,
          comment.char = "",
          fill = T,
          colClasses = rep("character", 3)
        )
        
        output$x$template[[i]]$content <- output$x$template[[i]]$content[ ,1:3]
        
        colnames(output$x$template[[i]]$content) <- c(
          "attributeName",
          "code",
          "definition"
        )
        
      } else {
        
        output$x$template[[i]]$content <- NA_character_
        
      }
      
    }
    
    # Read custom units -------------------------------------------------------
    
    if (stringr::str_detect(string = templates[i], pattern = 'custom_units.txt')){
      
      if (file.exists(paste0(path, '/', templates[i]))){
        
        output$x$template[[i]]$content <- utils::read.table(
          paste0(
            path, 
            '/', 
            templates[i]
          ),
          header = T,
          sep="\t",
          quote="\"",
          as.is=TRUE,
          comment.char = "",
          fill = T,
          colClasses = rep("character", 5)
        )
        
        output$x$template[[i]]$content <- output$x$template[[i]]$content[ ,1:5]
        
        colnames(output$x$template[[i]]$content) <- c(
          "id",
          "unitType",
          "parentSI",
          "multiplierToSI",
          "description"
        )
        
      } else {
        
        output$x$template[[i]]$content <- NA_character_
        
      }
      
    }

    # Read geographic bounding boxes ------------------------------------------
    
    if (stringr::str_detect(string = templates[i], pattern = 'bounding_boxes.txt')){
      
      if (file.exists(paste0(path, '/', templates[i]))){
        
        output$x$template[[i]]$content <- suppressMessages(
          as.data.frame(
            utils::read.table(
              paste0(
                path, 
                '/', 
                templates[i]
              ), 
              header = T, 
              sep = '\t', 
              quote = "\"", 
              as.is = T, 
              comment.char = ''
            )
          )
        )
        
      } else {
        
        output$x$template[[i]]$content <- NA_character_
        
      }
      
    }

    # Read geographic coverage ------------------------------------------------
    
    if (stringr::str_detect(string = templates[i], pattern = 'geographic_coverage.txt')){
      
      if (file.exists(paste0(path, '/', templates[i]))){
        
        output$x$template[[i]]$content <- utils::read.table(
          paste0(
            path, 
            '/', 
            templates[i]
          ),
          header = T,
          sep="\t",
          quote="\"",
          as.is=TRUE,
          comment.char = "",
          fill = T,
          na.strings = "NA",
          colClasses = c("numeric","numeric","character"),
          fileEncoding = "UTF-8"
        )
        
        output$x$template[[i]]$content <- output$x$template[[i]]$content[ ,1:3]
        
        colnames(output$x$template[[i]]$content) <- c(
          "latitude",
          "longitude",
          "site"
        )
        
        output$x$template[[i]]$content$latitude <- as.character(
          output$x$template[[i]]$content$latitude
        )
        
        output$x$template[[i]]$content$longitude <- as.character(
          output$x$template[[i]]$content$longitude
        )
        
        output$x$template[[i]]$content$site <- as.character(
          output$x$template[[i]]$content$site
        )
        
      } else {
        
        output$x$template[[i]]$content <- NA_character_
        
      }
      
    }
    
    # Read intellectual rights ------------------------------------------------
    
    if (stringr::str_detect(string = templates[i], pattern = 'intellectual_rights.txt')){
      
      if (file.exists(paste0(path, '/', templates[i]))){
        
        output$x$template[[i]]$content <- methods::as(
          EML::set_TextType(
            file = paste0(
              path, 
              '/', 
              templates[i]
            )
          ),
          'intellectualRights'
        )
        
      } else {
        
        output$x$template[[i]]$content <- NA_character_
        
      }
      
    }
    
    # Read keywords -----------------------------------------------------------
    
    if (stringr::str_detect(string = templates[i], pattern = 'keywords.txt')){
      
      if (file.exists(paste0(path, '/', templates[i]))){
        
        output$x$template[[i]]$content <- utils::read.table(
          file = paste0(
            path, 
            '/', 
            templates[i]
          ), 
          header = T,
          sep="\t",
          quote="\"",
          as.is=TRUE,
          comment.char = "",
          fill = T,
          colClasses = rep("character", 2)
        )
        
        output$x$template[[i]]$content <- output$x$template[[i]]$content[ ,1:2]
        
        colnames(output$x$template[[i]]$content) <- c(
          "keyword", 
          "keywordThesaurus"
        )
        
      } else {
        
        output$x$template[[i]]$content <- NA_character_
        
      }
      
    }
    
    # Read methods ------------------------------------------------------------
    
    if (stringr::str_detect(string = templates[i], pattern = 'methods.txt')){
      
      if (file.exists(paste0(path, '/', templates[i]))){
        
        output$x$template[[i]]$content <- EML::set_methods(
          methods_file = paste0(
            path, 
            '/', 
            templates[i]
          )
        )
        
      } else {
        
        output$x$template[[i]]$content <- NA_character_
        
      }
      
    }
    
    # Read personnel ----------------------------------------------------------
    
    if (stringr::str_detect(string = templates[i], pattern = 'personnel.txt')){
      
      if (file.exists(paste0(path, '/', templates[i]))){
        
        output$x$template[[i]]$content <- utils::read.table(
          paste0(
            path, 
            '/', 
            templates[i]
          ),
          header = T,
          sep="\t",
          quote="\"",
          as.is=TRUE,
          comment.char = "",
          fill = T,
          colClasses = rep("character", 10)
        )
        
        output$x$template[[i]]$content <- output$x$template[[i]]$content[ ,1:10]
        
        colnames(output$x$template[[i]]$content) <- c(
          "givenName",
          "middleInitial",
          "surName",
          "organizationName",
          "electronicMailAddress",
          "userId",
          "role",
          "projectTitle",
          "fundingAgency",
          "fundingNumber"
        )
        
      } else {
        
        output$x$template[[i]]$content <- NA_character_
        
      }
      
    }
    
    # Read taxonomic coverage -------------------------------------------------
    
    if (stringr::str_detect(string = templates[i], pattern = 'taxonomicCoverage.xml')){
      
      if (file.exists(paste0(path, '/', templates[i]))){
        
        output$x$template[[i]]$content <- methods::as(
          EML::read_eml(
            paste0(
              path, 
              '/', 
              templates[i]
            )
          ),
          'taxonomicCoverage'
        )
        
      } else {
        
        output$x$template[[i]]$content <- NA_character_
        
      }
      
    }

  }
  
  # Read data tables ----------------------------------------------------------
  
  # If data tables exist ...
  
  if (!is.null(data.table)){
    
    # For all data tables ...
    
    for (i in 1:length(output$x$data.table)){
      
      # If delimiter is undefined ...
      
      if (is.null(sep)){
        
        delim_guess <- EDIutils::detect_delimeter(
          path = data.path, 
          data.files = names(output$x$data.table[i]), 
          os = EDIutils::detect_os()
        )
        
        output$x$data.table[[i]]$content <- utils::read.table(
          file = paste0(
            data.path,
            '/',
            names(output$x$data.table[i])
          ),
          header = T,
          sep = delim_guess,
          quote = "\"",
          as.is = TRUE,
          comment.char = ""
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
