#' Read template and data files into a list structure
#'
#' @description  
#'     Read metadata templates, data files, and function arguments into an R 
#'     list structure as an alternative input/output to \code{EMLassemblyline} 
#'     functions (i.e. rather than supplying the files themselves).
#'     
#'     This approach enables programatic interfacing of upstream sources to 
#'     \code{EMLassemblyline} (e.g. a metabase or database). See below for 
#'     valid list structure.
#'
#' @usage 
#'     read_files(
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
#'     \code{read_files} fails to automatically identify field delimiters of 
#'     \code{data.table}.
#'
#' @note 
#'     The full named list structure is returned irrespective of NULL inputs 
#'     however, named fields will have NULL values.
#'
#' @return 
#'     (named list) Named list with this internal structure:
#'     \itemize{
#'         \item{\strong{template} Metadata template files}
#'         \itemize{
#'             \item{\strong{abstract.txt} Abstract template}
#'             \itemize{
#'                 \item{\strong{content} \code{EML::TextType} containing 
#'                 abstract, NA otherwise}
#'              }
#'              \item{\strong{additional_info.txt} Additional information 
#'              template}
#'              \itemize{
#'                  \item{\strong{content} \code{EML::TextType} containing 
#'                  additional info, NA otherwise}
#'              }
#'              \item{\strong{attributes_name.txt} Attributes template where 
#'              "name" is the corresponding \code{data.table}.}
#'              \itemize{
#'                  \item{\strong{content} Data frame of attributes template, 
#'                  NA otherwise.}
#'              }
#'              \item{\strong{bounding_boxes.txt} Bounding boxes template}
#'              \itemize{
#'                 \item{\strong{content} Data frame of bounding boxes 
#'                 template, NA otherwise.}
#'              }
#'              \item{\strong{catvars_name.txt} Categorical variables template 
#'              where "name" is the corresponding \code{data.table}.}
#'              \itemize{
#'                 \item{\strong{content} Data frame of categorical variables 
#'                 template, NA otherwise.}
#'              }
#'              \item{\strong{custom_units.txt} Custom units template}
#'              \itemize{
#'                 \item{\strong{content} Data frame of custom units template, 
#'                 NA otherwise.}
#'              }
#'              \item{\strong{geographic_coverage.txt} Geographic coverage 
#'              template}
#'              \itemize{
#'                 \item{\strong{content} Data frame of geographic coverage 
#'                 template, NA otherwise.}
#'              }
#'              \item{\strong{intellectual_rights.txt} Intellectual rights 
#'              template}
#'              \itemize{
#'                 \item{\strong{content} \code{EML::TextType} containing 
#'                 intellectual rights template, NA otherwise.}
#'              }
#'              \item{\strong{keywords.txt} Keywords template}
#'              \itemize{
#'                 \item{\strong{content} Data frame of keywords template, 
#'                 NA otherwise.}
#'              }
#'              \item{\strong{methods.txt} Methods template}
#'              \itemize{
#'                 \item{\strong{content} \code{EML::Methods} containing 
#'                 methods, NA otherwise.}
#'              }
#'              \item{\strong{personnel.txt} Personnel template}
#'              \itemize{
#'                 \item{\strong{content} Data frame of personnel template, 
#'                 NA otherwise.}
#'              }
#'              \item{\strong{taxonomicCoverage.xml} Taxonomic coverage EML 
#'              element}
#'              \itemize{
#'                 \item{\strong{content} \code{EML::taxonomicCoverage}, 
#'                 NA otherwise}
#'              }     
#'             }
#'        \item{\strong{data.table} Data tables}
#'        \itemize{
#'            \item{\strong{data.table.name.ext} Name of data table including 
#'            file extension, NULL otherwise.}
#'            \itemize{
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
#'        \item{\strong{argument} A list of all \code{EMLassemblyline} 
#'        arguments.}
#'        \itemize{
#'            \item{\strong{data.path} (character) Path to where the data 
#'            files are stored.}
#'            \item{\strong{data.table} (character) Data table name. If more 
#'            than one, supply as a vector of character strings (e.g. 
#'            \code{data.table = c('concentrations.csv', 'characteristics.csv')}).}
#'            \item{\strong{data.table.description} (character) Data table 
#'            description. Brief description of data tables. If more than 
#'            one data table, then supply as a vector of character strings in 
#'            the same order as listed in \code{data.table}.}
#'            \item{\strong{data.table.quote.character} (character) Quote 
#'            character used in data tables. If more than one, then supply as 
#'            a vector of character strings in the same order as listed in 
#'            \code{data.table}. This argument is required only if 
#'            \code{data.table} contain quotations. If the quote character is 
#'            a quotation, then enter "\\"". If the quote character is an 
#'            apostrophe, then enter "\\'".}
#'            \item{\strong{data.url} (character) A publicly accessible URL to 
#'            data tables and other entities of this dataset.}
#'            \item{\strong{dataset.title} (character) Dataset title.}
#'            \item{\strong{dir.name} (character) Name of parent directory. 
#'            Subdirectories have predefined names.}
#'            \item{\strong{eml.path} (character) Path to where EML will be 
#'            written.}
#'            \item{\strong{geographic.coordinates} (character) Geographic coordinates 
#'            delineating the bounding area or point of a dataset, in decimal 
#'            degrees. This argument is not required if using 
#'            \code{bounding_boxes.txt}. Values must be listed in this order: 
#'            North, East, South, West. Longitudes West of the prime meridian 
#'            and latitudes South of the equator are negative. If representing 
#'            a point, repeat the latitude for North and South, and repeat the 
#'            longitude for East and West (e.g. 
#'            \code{geographic.coordinates = c('28.38', '-119.95', '28.38', '-119.95')}).}
#'            \item{\strong{geographic.description} (character) Geographic 
#'            description. Don't use this argument if using the 
#'            \code{bounding_boxes.txt} template.}
#'            \item{\strong{lat.col} (character) Name of latitude column.}
#'            \item{\strong{license} (character) License under which the data 
#'            will be released.}
#'            \item{\strong{lon.col} (character) Name of longitude column.}
#'            \item{\strong{maintenance.description} (character) Indicator of 
#'            whether data collection is 'ongoing' or 'completed'.}
#'            \item{\strong{other.entity} (character) Other entity name. If 
#'            more than one, then supply as a vector of character strings. 
#'            Other entities should be located at \code{data.path}.}
#'            \item{\strong{other.entity.description} (character) Other entity 
#'            description. If more than one, then supply as a vector of 
#'            descriptions in the same order as listed in \code{other.entity}.}
#'            \item{\strong{package.id} (character) Data package identifier. 
#'            Missing \code{package.id} is assigned a default value of 
#'            'edi.101.1'. Any data repository package ID may be used.}
#'            \item{\strong{path} (character) Path to where template files are
#'            stored.}
#'            \item{\strong{provenance} (character) EDI Data Repository Data 
#'            package ID(s) corresponding to parent datasets from which this 
#'            dataset was created (e.g. 'knb-lter-cap.46.3'). If more than one, 
#'            then supply as a vector of character strings.}
#'            \item{\strong{return.obj} (logical) Return the EML as an R 
#'            object of class \code{EML object}.}
#'            \item{\strong{sep} (character) Field delimiter to be used when 
#'            reading data tables.}
#'            \item{\strong{site.col} (character) Name of site column, where
#'            site is the name of the location specified by \code{lat.col} and 
#'            \code{lon.col}.}
#'            \item{\strong{temporal.coverage} (character) Beginning and ending 
#'            dates of the dataset as a vector of character strings in the 
#'            format \code{YYYY-MM-DD}.}
#'            \item{\strong{user.domain} (character) Domain of the 
#'            \code{user.id}. If more than one, then supply as a vector of 
#'            character strings in the same order corresponding to listed under
#'            \code{user.id}. Valid options for EDI are 'LTER' and 'EDI'. Other 
#'            data repository IDs may be used. NULL user.domain defaults to 
#'            'someuserdomain'.}
#'            \item{\strong{user.id} (character) ID of data repository user 
#'            account. If more than one, supply as a vector of character 
#'            strings. NULL user.id defaults to 'someuserid'.}
#'            \item{\strong{write.file} (logical) Write file to 
#'            \code{eml.path}.}
#'        }
#'     }
#'     
#'     
#' @export
#'

read_files <- function(
  path = NULL, 
  data.path = NULL, 
  data.table = NULL,
  other.entity = NULL, 
  sep = NULL
  ){
  
  # Validate arguments --------------------------------------------------------
  
  validate_arguments(
    fun.name = 'read_files',
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
  
  # Initialize list of template files
  
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
  
  # Add any missing core template files as NULL
  
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

  # List data.table
  
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
  
  # List other.entity
  
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
  
  # List arguments
  
  arguments <- vector(
    'list',
    nrow(attr.args)
  )
  
  names(arguments) <- attr.args$argument_name
  
  # Initialize outgoing list of templates, data tables, and other entities
  
  output <- list(
    template = templates,
    data.table = data_tables,
    other.entity = other_entities,
    argument = arguments
  )
  
  # Add template files to outgoing list ---------------------------------------
  # Most templates have unique read settings therefore requiring unique calls
  
  templates <- names(output$template)
  
  # For all templates ...
  
  for (i in 1:length(output$template)){
    
    # If abstract ...
    
    if (stringr::str_detect(string = templates[i], pattern = 'abstract.txt')){
      
      if (file.exists(paste0(path, '/', templates[i]))){
        
        output$template[[i]]$content <- methods::as(
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
        
        output$template[[i]]$content <- NA_character_
        
      }

    }
    
    # If additional_info ...
    
    if (stringr::str_detect(string = templates[i], pattern = 'additional_info.txt')){
      
      if (file.exists(paste0(path, '/', templates[i]))){
        
        output$template[[i]]$content <- methods::as(
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
        
        output$template[[i]]$content <- NA_character_
        
      }
      
    }
    
    # If attributes_*.txt ...
    
    if (stringr::str_detect(string = templates[i], pattern = 'attributes_[:graph:]*.txt')){
      
      if (file.exists(paste0(path, '/', templates[i]))){
        
        output$template[[i]]$content <- utils::read.table(
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
        
        output$template[[i]]$content <- output$template[[i]]$content[ ,1:7]
        
        colnames(output$template[[i]]$content) <- c(
          "attributeName",
          "attributeDefinition",
          "class",
          "unit",
          "dateTimeFormatString",
          "missingValueCode",
          "missingValueCodeExplanation"
        )
        
      } else {
        
        output$template[[i]]$content <- NA_character_
        
      }
      
    }
    
    # If bounding_boxes.txt ...
    
    if (stringr::str_detect(string = templates[i], pattern = 'bounding_boxes.txt')){
      
      if (file.exists(paste0(path, '/', templates[i]))){
        
        output$template[[i]]$content <- suppressMessages(
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
        
        output$template[[i]]$content <- NA_character_
        
      }
      
    }
    
    # If catvars_*.txt ...
    
    if (stringr::str_detect(string = templates[i], pattern = 'catvars_[:graph:]*.txt')){
      
      if (file.exists(paste0(path, '/', templates[i]))){
        
        output$template[[i]]$content <- utils::read.table(
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
        
        output$template[[i]]$content <- output$template[[i]]$content[ ,1:3]
        
        colnames(output$template[[i]]$content) <- c(
          "attributeName",
          "code",
          "definition"
        )
        
      } else {
        
        output$template[[i]]$content <- NA_character_
        
      }
      
    }
    
    # If custom_units.txt ...
    
    if (stringr::str_detect(string = templates[i], pattern = 'custom_units.txt')){
      
      if (file.exists(paste0(path, '/', templates[i]))){
        
        output$template[[i]]$content <- utils::read.table(
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
        
        output$template[[i]]$content <- output$template[[i]]$content[ ,1:5]
        
        colnames(output$template[[i]]$content) <- c(
          "id",
          "unitType",
          "parentSI",
          "multiplierToSI",
          "description"
        )
        
      } else {
        
        output$template[[i]]$content <- NA_character_
        
      }
      
    }
    
    # If geographic_coverage.txt ...
    
    if (stringr::str_detect(string = templates[i], pattern = 'geographic_coverage.txt')){
      
      if (file.exists(paste0(path, '/', templates[i]))){
        
        output$template[[i]]$content <- utils::read.table(
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
        
        output$template[[i]]$content <- output$template[[i]]$content[ ,1:3]
        
        colnames(output$template[[i]]$content) <- c(
          "latitude",
          "longitude",
          "site"
        )
        
        output$template[[i]]$content$latitude <- as.character(
          output$template[[i]]$content$latitude
        )
        
        output$template[[i]]$content$longitude <- as.character(
          output$template[[i]]$content$longitude
        )
        
        output$template[[i]]$content$site <- as.character(
          output$template[[i]]$content$site
        )
        
      } else {
        
        output$template[[i]]$content <- NA_character_
        
      }
      
    }
    
    # If intellectual_rights ...
    
    if (stringr::str_detect(string = templates[i], pattern = 'intellectual_rights.txt')){
      
      if (file.exists(paste0(path, '/', templates[i]))){
        
        output$template[[i]]$content <- methods::as(
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
        
        output$template[[i]]$content <- NA_character_
        
      }
      
    }
    
    # If keywords.txt ...
    
    if (stringr::str_detect(string = templates[i], pattern = 'keywords.txt')){
      
      if (file.exists(paste0(path, '/', templates[i]))){
        
        output$template[[i]]$content <- utils::read.table(
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
        
        output$template[[i]]$content <- output$template[[i]]$content[ ,1:2]
        
        colnames(output$template[[i]]$content) <- c(
          "keyword", 
          "keywordThesaurus"
        )
        
      } else {
        
        output$template[[i]]$content <- NA_character_
        
      }
      
    }
    
    # If methods ...
    
    if (stringr::str_detect(string = templates[i], pattern = 'methods.txt')){
      
      if (file.exists(paste0(path, '/', templates[i]))){
        
        output$template[[i]]$content <- EML::set_methods(
          methods_file = paste0(
            path, 
            '/', 
            templates[i]
          )
        )
        
      } else {
        
        output$template[[i]]$content <- NA_character_
        
      }
      
    }
    
    # If personnel.txt ...
    
    if (stringr::str_detect(string = templates[i], pattern = 'personnel.txt')){
      
      if (file.exists(paste0(path, '/', templates[i]))){
        
        output$template[[i]]$content <- utils::read.table(
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
        
        output$template[[i]]$content <- output$template[[i]]$content[ ,1:10]
        
        colnames(output$template[[i]]$content) <- c(
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
        
        output$template[[i]]$content <- NA_character_
        
      }
      
    }
    
    # If taxonomicCoverage ...
    
    if (stringr::str_detect(string = templates[i], pattern = 'taxonomicCoverage.xml')){
      
      if (file.exists(paste0(path, '/', templates[i]))){
        
        output$template[[i]]$content <- methods::as(
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
        
        output$template[[i]]$content <- NA_character_
        
      }
      
    }

  }
  
  # Add data tables to outgoing list ------------------------------------------
  
  # If data tables exist ...
  
  if (!is.null(data.table)){
    
    # For all data tables ...
    
    for (i in 1:length(output$data.table)){
      
      # If delimiter is undefined ...
      
      if (is.null(sep)){
        
        delim_guess <- EDIutils::detect_delimeter(
          path = data.path, 
          data.files = names(output$data.table[i]), 
          os = EDIutils::detect_os()
        )
        
        output$data.table[[i]]$content <- utils::read.table(
          file = paste0(
            data.path,
            '/',
            names(output$data.table[i])
          ),
          header = T,
          sep = delim_guess,
          quote = "\"",
          as.is = TRUE,
          comment.char = ""
        )
        
        # If delimiter is defined ...
        
      } else {
        
        output$data.table[[i]]$content <- utils::read.table(
          file = paste0(
            data.path,
            '/',
            names(output$data.table[i])
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
  
  # Add other entities to outgoing list ---------------------------------------
  
  # If other entities exist ...
  
  if (!is.null(other.entity)){
    
    # For all other entities ...
    
    for (i in 1:length(output$other.entity)){
      
      output$other.entity[[i]]$content <- NA
      
    }
    
  }
  
  # Return --------------------------------------------------------------------
  
  output
  
}
