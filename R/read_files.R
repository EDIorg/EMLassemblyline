#' Read template and data files into a list structure
#'
#' @description  
#'     Read metadata templates, data files, and function arguments into an R 
#'     list structure as an alternative input to \code{EMLassemblyline} functions 
#'     (i.e. rather than supplying the files themselves).
#'     
#'     This function enables programatic interfacing of upstream sources with 
#'     \code{EMLassemblyline} (e.g. a metabase or database). See below for valid 
#'     list structure.
#'
#' @usage 
#'     read_files(
#'       path,
#'       data.path = path,
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
#'     (character) Data table name(s). If more than one, then supply as a 
#'     vector of character strings (e.g. 
#'     \code{data.table = c("concentrations.csv", "characteristics.csv")}).
#' @param other.entity
#'     (character) Name(s) of \code{other.entity}(s) in this dataset. Use
#'     \code{other.entity} for all non-\code{data.table} files. \code{other.entity}(s) should 
#'     be stored at \code{data.path}.
#' @param sep
#'     (character) Delimiter of \code{data.table}. Use this argument if \code{read_files()}
#'     fails to automatically identify \code{data.table} field delimiters.
#'
#' @note 
#'     Paths to data tables and other entities must be valid when passing the 
#'     list structure to \code{make_eml()}. Paths to other content is optional.
#'
#' @return 
#'     (named list) Named list with this internal structure.:
#'     \itemize{
#'         \item{\strong{template} Metadata template files}
#'         \itemize{
#'             \item{\strong{abstract.txt} Abstract template}
#'             \itemize{
#'                 \item{\strong{content} EML::TextType containing abstract, \code{NA} otherwise}
#'                 \item{\strong{path} Path to file, \code{NA} otherwise}
#'              }
#'              \item{\strong{additional_info.txt} Additional information template}
#'              \itemize{
#'                  \item{\strong{content} EML::TextType containing additional info, \code{NA} otherwise}
#'                  \item{\strong{path} Path to file, \code{NA} otherwise}
#'              }
#'              \item{\strong{attributes_name.txt} Attributes template where "name" is name of \code{data.table} described by the attributes. Don't include if missing.}
#'              \itemize{
#'                  \item{\strong{content} Data frame of attributes template, \code{NA} otherwise}
#'                  \item{\strong{path} Path to file, \code{NA} otherwise}
#'              }
#'              \item{\strong{bounding_boxes.txt} Bounding boxes template}
#'              \itemize{
#'                 \item{\strong{content} Data frame of bounding boxes template, \code{NA} otherwise}
#'                 \item{\strong{path} Path to file, \code{NA} otherwise}
#'              }
#'              \item{\strong{catvars_name.txt} Categorical variables template where name is name of \code{data.table} described by the catvars. Don't include if missing.}
#'              \itemize{
#'                 \item{\strong{content} Data frame of categorical variables template, \code{NA} otherwise}
#'                 \item{\strong{path} Path to file, \code{NA} otherwise}
#'              }
#'              \item{\strong{custom_units.txt} Custom units template}
#'              \itemize{
#'                 \item{\strong{content} Data frame of custom units template, \code{NA} otherwise}
#'                 \item{\strong{path} Path to file, \code{NA} otherwise}
#'              }
#'              \item{\strong{geographic_coverage.txt} Geographic coverage template}
#'              \itemize{
#'                 \item{\strong{content} Data frame of geographic coverage template, \code{NA} otherwise}
#'                 \item{\strong{path} Path to file, \code{NA} otherwise}
#'              }
#'              \item{\strong{intellectual_rights.txt} Intellectual rights template}
#'              \itemize{
#'                 \item{\strong{content} EML::TextType containing intellectual rights, \code{NA} otherwise}
#'                 \item{\strong{path} Path to file, \code{NA} otherwise}
#'              }
#'              \item{\strong{keywords.txt} Keywords template}
#'              \itemize{
#'                 \item{\strong{content} Data frame of keywords template, \code{NA} otherwise}
#'                 \item{\strong{path} Path to file, \code{NA} otherwise}
#'              }
#'              \item{\strong{methods.txt} Methods template}
#'              \itemize{
#'                 \item{\strong{content} EML::Methods containing methods, \code{NA} otherwise}
#'                 \item{\strong{path} Path to file, \code{NA} otherwise}
#'              }
#'              \item{\strong{personnel.txt} Personnel template}
#'              \itemize{
#'                 \item{\strong{content} Data frame of personnel template, \code{NA} otherwise}
#'                 \item{\strong{path} Path to file, \code{NA} otherwise}
#'              }
#'              \item{\strong{taxonomicCoverage.xml} Taxonomic coverage EML element}
#'              \itemize{
#'                 \item{\strong{content} EML::taxonomicCoverage, \code{NA} otherwise}
#'                 \item{\strong{path} Path to file, \code{NA} otherwise}
#'              }                    
#'             }
#'        \item{\strong{data.table} Data tables}
#'        \itemize{
#'            \item{\strong{data.table.name.ext} Name of data table including file extension, \code{NULL} otherwise}
#'            \itemize{
#'                 \item{\strong{content} Data frame of data table}
#'                 \item{\strong{path} Path to data table (required if \code{data.table} exists)}
#'            }
#'          }
#'        \item{\strong{other.entity} Other entities}
#'        \itemize{
#'            \item{\strong{other.entity.name.ext} Name of other entity including file extension, \code{NULL} otherwise}
#'            \itemize{
#'                 \item{\strong{content} Set to \code{NA}}
#'                 \item{\strong{path} Path to other entity (required if \code{data.table} exists)}
#'            }
#'        }
#'        \item{\strong{argument} A list of arguments to be passed to \code{make_eml()}. 
#'        Other \code{EMLassemblyline} functions don't yet accept this input.}
#'        \itemize{
#'            \item{\strong{data.path} (character) Path to where the data files are 
#'            stored. This path must be valid when passing the list structure 
#'            to \code{make_eml()} because this is where checksum values are 
#'            calculated.}
#'            \item{\strong{data.table} (character) Data table name(s). If more than 
#'            one, then supply as a vector of character strings (e.g. 
#'            \code{data.table = c("concentrations.csv", "characteristics.csv")}).}
#'            \item{\strong{data.table.description} (character) Data table 
#'            description(s). Brief description of \code{data.table}. If more than 
#'            one, then supply as a vector of character strings in the same 
#'            order as listed in \code{data.table}.}
#'            \item{\strong{data.table.quote.character} (character) Quote character(s) 
#'            used in \code{data.table}. If more than one, then supply as a vector 
#'            of character strings in the same order as listed in \code{data.table}. 
#'            This argument is required only if your data contain quotations. 
#'            If the quote character is a quotation, then enter \code{"\\""}. If 
#'            the quote character is an apostrophe, then enter \code{"\\'"}.}
#'            \item{\strong{data.url} (character) The URL of where your data can be 
#'            downloaded by a data repository. This argument is not required, 
#'            if the data will be manually uploaded to a data repository.}
#'            \item{\strong{dataset.title} (character) Dataset title. Be descriptive 
#'            (more than 5 words) and consider including the temporal coverage 
#'            (e.g. "GLEON: Long term lake chloride concentrations from North 
#'            America and Europe: 1940-2016").}
#'            \item{\strong{eml.path} (character) Path to where the EML will be 
#'            written.}
#'            \item{\strong{geographic.coordinates} (character) Geographic coordinates 
#'            delineating the bounding area or point of a dataset, in decimal 
#'            degrees. This argument is not required if using 
#'            \code{bounding_boxes.txt}. Values must be listed in this order: North, 
#'            East, South, West. Longitudes West of the prime meridian and 
#'            latitudes South of the equator are negative. If representing a 
#'            point, repeat the latitude for North and South, and repeat the 
#'            longitude for East and West (e.g. 
#'            \code{geographic.coordinates = c('28.38', '-119.95', '28.38', '-119.95')}).}
#'            \item{\strong{geographic.description} (character) Geographic 
#'            description. Don't use this argument if geographic coordinates in 
#'            the \code{bounding_boxes.txt} template.}
#'            \item{\strong{maintenance.description} (character) Indicator of whether 
#'            data collection is \code{ongoing} or \code{completed}.}
#'            \item{\strong{other.entity} (character) Name(s) of \code{other.entity}(s) in 
#'            this dataset. Use \code{other.entity} for all non-\code{data.table} files. 
#'            \code{other.entity}(s) should be stored at \code{data.path}.}
#'            \item{\strong{other.entity.description} (character) Description(s) of 
#'            \code{other.entity}(s). If more than one, then supply as a vector of 
#'            descriptions in the same order as listed in \code{other.entity}.}
#'            \item{\strong{package.id} (character) EDI Data Repository Data package 
#'            ID for this dataset. A missing package ID defaults to 
#'            \code{edi.101.1}.}
#'            \item{\strong{path} (character) Path to where the template(s) will be 
#'            stored.}
#'            \item{\strong{provenance} (character) EDI Data Repository Data package 
#'            ID(s) corresponding to parent datasets from which this dataset 
#'            was created (e.g. \code{knb-lter-cap.46.3}).}
#'            \item{\strong{return.obj} (logical) Return the EML as an R object of 
#'            class \code{EML object}.}
#'            \item{\strong{temporal.coverage} (character) Beginning and ending dates 
#'            of the dataset as a vector of character strings in the format 
#'            \code{YYYY-MM-DD}.}
#'            \item{\strong{user.domain} (character) Domain of the \code{user.id}(s). Valid 
#'            options for EDI are \code{LTER} and \code{EDI}. If more than one, supply 
#'            as a vector of character strings in the same order as 
#'            corresponding \code{user.id}(s). This argument is not required.}
#'            \item{\strong{user.id} (character) ID(s) of data repository user 
#'            account(s). If more than one, supply as a vector of character 
#'            strings. Contact EDI (info@@environmentaldatainitiative.org) to 
#'            obtain one. This argument is not required.}
#'            \item{\strong{write.file} (logical) Write file to \code{eml.path}.}
#'        }
#'     }
#'     
#' @export
#'

read_files <- function(path, data.path = path, data.table = NULL, 
                       other.entity = NULL, sep = NULL){
  
  # Parameterize --------------------------------------------------------------
  
  # Get template file attributes
  
  attr.templates <- utils::read.table(
    file = system.file(
      '/templates/template_characteristics.txt',
      package = 'EMLassemblyline'
    ), 
    header = T,
    sep = '\t',
    as.is = T
  )
  
  # Get argument attributes
  
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
  
  # Add any missing core template files as NULL
  
  missing_core_templates <- c()
  
  i_core <- seq(nrow(attr.templates))[
    attr.templates$core_template == TRUE
  ]
  
  # For each core template file ...
  
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
        
        output$template[[i]]$path <- path
        
      } else {
        
        output$template[[i]]$content <- NA_character_
        
        output$template[[i]]$path <- NA_character_
        
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
        
        output$template[[i]]$path <- path
        
      } else {
        
        output$template[[i]]$content <- NA_character_
        
        output$template[[i]]$path <- NA_character_
        
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
        
        output$template[[i]]$path <- path
        
      } else {
        
        output$template[[i]]$content <- NA_character_
        
        output$template[[i]]$path <- NA_character_
        
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
        
        output$template[[i]]$path <- path
        
      } else {
        
        output$template[[i]]$content <- NA_character_
        
        output$template[[i]]$path <- NA_character_
        
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
        
        output$template[[i]]$path <- path
        
      } else {
        
        output$template[[i]]$content <- NA_character_
        
        output$template[[i]]$path <- NA_character_
        
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
        
        output$template[[i]]$path <- path
        
      } else {
        
        output$template[[i]]$content <- NA_character_
        
        output$template[[i]]$path <- NA_character_
        
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
        
        output$template[[i]]$path <- path
        
      } else {
        
        output$template[[i]]$content <- NA_character_
        
        output$template[[i]]$path <- NA_character_
        
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
        
        output$template[[i]]$path <- path
        
      } else {
        
        output$template[[i]]$content <- NA_character_
        
        output$template[[i]]$path <- NA_character_
        
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
        
        output$template[[i]]$path <- path
        
      } else {
        
        output$template[[i]]$content <- NA_character_
        
        output$template[[i]]$path <- NA_character_
        
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
        
        output$template[[i]]$path <- path
        
      } else {
        
        output$template[[i]]$content <- NA_character_
        
        output$template[[i]]$path <- NA_character_
        
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
        
        output$template[[i]]$path <- path
        
      } else {
        
        output$template[[i]]$content <- NA_character_
        
        output$template[[i]]$path <- NA_character_
        
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
        
        output$template[[i]]$path <- path
        
      } else {
        
        output$template[[i]]$content <- NA_character_
        
        output$template[[i]]$path <- NA_character_
        
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
        
        output$data.table[[i]]$path <- data.path
        
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
        
        output$data.table[[i]]$path <- data.path
        
      }
      
    }

  }
  
  # Add other entities to outgoing list ---------------------------------------
  
  # If other entities exist ...
  
  if (!is.null(other.entity)){
    
    # For all other entities ...
    
    for (i in 1:length(output$other.entity)){
      
      output$other.entity[[i]]$content <- NA
      
      output$other.entity[[i]]$path <- data.path
      
    }
    
  }
  
  # Return --------------------------------------------------------------------
  
  output
  
}
