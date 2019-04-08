#' Read template and data files into a list structure
#'
#' @description  
#'     Read metadata templates and data files into an R list structure as an 
#'     alternative input to `EMLassemblyline` functions (i.e. rather than 
#'     supplying the files themselves).
#'     
#'     This function is primarily used for testing and demonstration purposes
#'     however, this approach is generally useful for interfacing upstream 
#'     sources with `EMLassemblyline` (e.g. a metabase or database). See below 
#'     for list structure.
#'
#' @usage read_files(path, data.path = path, data.table = NULL, 
#' other.entity = NULL, sep = NULL)
#'
#' @param path 
#'     (character) Path to metadata templates.
#' @param data.path
#'     (character) Path to data tables and other entities.
#' @param data.table
#'     (character) Data table name(s). If more than one, then supply as a 
#'     vector of character strings (e.g. 
#'     `data.table = c("concentrations.csv", "characteristics.csv")`).
#' @param other.entity
#'     (character) Name(s) of `other.entity`(s) in this dataset. Use
#'     `other.entity` for all non-`data.table` files. `other.entity`(s) should 
#'     be stored at `data.path`.
#' @param sep
#'     (character) Delimiter of `data.table`. Use this argument if `read_files`
#'     fails to automatically identify `data.table` field delimiters.
#'
#' @note 
#'     Paths to data tables and other entities must be present for 
#'     auto-generated metadata content.
#'
#' @return 
#'     (named list) Named list with this internal structure.:
#'     \itemize{
#'         \item `template` Metadata template files
#'         \itemize{
#'             \item{`abstract.txt` Abstract template}
#'             \itemize{
#'                 \item{`content` EML::TextType containing abstract, `NA` otherwise}
#'                 \item{`path` Path to file, `NA` otherwise}
#'              }
#'              \item{`additional_info.txt` Additional information template}
#'              \itemize{
#'                  \item{`content` EML::TextType containing additional info, `NA` otherwise}
#'                  \item{`path` Path to file, `NA` otherwise}
#'              }
#'              \item{`attributes_*.txt` Attributes template where * is name of `data.table` described by the attributes. Don't include if missing.}
#'              \itemize{
#'                  \item{`content` Data frame of attributes template, `NA` otherwise}
#'                  \item{`path` Path to file, `NA` otherwise}
#'              }
#'              \item{`bounding_boxes.txt` Bounding boxes template}
#'              \itemize{
#'                 \item{`content` Data frame of bounding boxes template, `NA` otherwise}
#'                 \item{`path` Path to file, `NA` otherwise}
#'              }
#'              \item{`catvars_*.txt` Categorical variables template where * is name of `data.table` described by the catvars. Don't include if missing.}
#'              \itemize{
#'                 \item{`content` Data frame of categorical variables template, `NA` otherwise}
#'                 \item{`path` Path to file, `NA` otherwise}
#'              }
#'              \item{`custom_units.txt` Custom units template}
#'              \itemize{
#'                 \item{`content` Data frame of custom units template, `NA` otherwise}
#'                 \item{`path` Path to file, `NA` otherwise}
#'              }
#'              \item{`geographic_coverage.txt` Geographic coverage template}
#'              \itemize{
#'                 \item{`content` Data frame of geographic coverage template, `NA` otherwise}
#'                 \item{`path` Path to file, `NA` otherwise}
#'              }
#'              \item{`intellectual_rights.txt` Intellectual rights template}
#'              \itemize{
#'                 \item{`content` EML::TextType containing intellectual rights, `NA` otherwise}
#'                 \item{`path` Path to file, `NA` otherwise}
#'              }
#'              \item{`keywords.txt` Keywords template}
#'              \itemize{
#'                 \item{`content` Data frame of keywords template, `NA` otherwise}
#'                 \item{`path` Path to file, `NA` otherwise}
#'              }
#'              \item{`methods.txt` Methods template}
#'              \itemize{
#'                 \item{`content` EML::Methods containing methods, `NA` otherwise}
#'                 \item{`path` Path to file, `NA` otherwise}
#'              }
#'              \item{`personnel.txt` Personnel template}
#'              \itemize{
#'                 \item{`content` Data frame of personnel template, `NA` otherwise}
#'                 \item{`path` Path to file, `NA` otherwise}
#'              }
#'              \item{`taxonomicCoverage.xml` Taxonomic coverage EML element}
#'              \itemize{
#'                 \item{`content` EML::taxonomicCoverage, `NA` otherwise}
#'                 \item{`path` Path to file, `NA` otherwise}
#'              }                    
#'             }
#'        \item{`data.table`}
#'        \itemize{
#'            \item{`data.table.name.ext` Name of data table including file extension, `NULL` otherwise}
#'            \itemize{
#'                 \item{`content` Data frame of data table}
#'                 \item{`path` Path to data table (required if `data.table` exists)}
#'            }
#'          }
#'        \item{`other.entity`}
#'        \itemize{
#'            \item{`other.entity.name.ext` Name of other entity including file extension, `NULL` otherwise}
#'            \itemize{
#'                 \item{`content` Set to `NA`}
#'                 \item{`path` Path to other entity (required if `data.table` exists)}
#'            }
#'        
#'        }
#'     
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
  
  # Initialize outgoing list of templates, data tables, and other entities
  
  output <- list(
    template = templates,
    data.table = data_tables,
    other.entity = other_entities
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
