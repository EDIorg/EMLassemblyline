#' Read templates and data files into a list structure as an alternative input 
#' to `EMLassemblyline` functions
#'
#' @description  
#'     Use this function to read metadata templates and data into an R list
#'     object for input to `EMLassemblyline` functions, rather than supplying
#'     the files themselves.
#'     
#'     This function is primarily used for testing and demonstration purposes
#'     however, this approach is generally useful for inputing metadata and 
#'     data from a meta- or data-base and other upstream sources. See below for
#'     list object structure.
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
#'     (named list) Named list with this internal structure (NOTE: use `NA` for
#'     non-existant content):
#'     \itemize{
#'         \item `template` Metadata template files
#'         \itemize{
#'             \item{`abstract.txt` Abstract template}
#'             \itemize{
#'                 \item{`content` EML::TextType containing abstract}
#'                 \item{`path` Path to file}
#'              }
#'              \item{`additional_info.txt` Additional information template}
#'              \itemize{
#'                  \item{`content` EML::TextType containing additional info}
#'                  \item{`path` Path to file}
#'              }
#'              \item{`attributes_*.txt` Attributes template where * is name of `data.table` described by the attributes}
#'              \itemize{
#'                  \item{`content` Data frame of attributes template}
#'                  \item{`path` Path to file}
#'              }
#'              \item{`bounding_boxes.txt` Bounding boxes template}
#'              \itemize{
#'                 \item{`content` Data frame of bounding boxes template}
#'                 \item{`path` Path to file}
#'              }
#'              \item{`catvars_*.txt` Categorical variables template where * is name of `data.table` described by the catvars}
#'              \itemize{
#'                 \item{`content` Data frame of categorical variables template}
#'                 \item{`path` Path to file}
#'              }
#'              \item{`custom_units.txt` Custom units template}
#'              \itemize{
#'                 \item{`content` Data frame of custom units template}
#'                 \item{`path` Path to file}
#'              }
#'              \item{`geographic_coverage.txt` Geographic coverage template}
#'              \itemize{
#'                 \item{`content` Data frame of geographic coverage template}
#'                 \item{`path` Path to file}
#'              }
#'              \item{`intellectual_rights.txt` Intellectual rights template}
#'              \itemize{
#'                 \item{`content` EML::TextType containing intellectual rights}
#'                 \item{`path` Path to file}
#'              }
#'              \item{`keywords.txt` Keywords template}
#'              \itemize{
#'                 \item{`content` Data frame of keywords template}
#'                 \item{`path` Path to file}
#'              }
#'              \item{`methods.txt` Methods template}
#'              \itemize{
#'                 \item{`content` EML::Methods containing methods}
#'                 \item{`path` Path to file}
#'              }
#'              \item{`personnel.txt` Personnel template}
#'              \itemize{
#'                 \item{`content` Data frame of personnel template}
#'                 \item{`path` Path to file}
#'              }
#'              \item{`taxonomicCoverage.xml` Taxonomic coverage EML element}
#'              \itemize{
#'                 \item{`content` EML::taxonomicCoverage}
#'                 \item{`path` Path to file}
#'              }                    
#'             }
#'        \item{`data.table`}
#'        \itemize{
#'            \item{`data.table.name.ext` Name of data table including file extension}
#'            \itemize{
#'                 \item{`content` Data frame of data table}
#'                 \item{`path` Path to data table (required)}
#'            }
#'          }
#'        \item{`other.entity`}
#'        \itemize{
#'            \item{`other.entity.name.ext` Name of other entity including file extension}
#'            \itemize{
#'                 \item{`content` Set to `NA`}
#'                 \item{`path` Path to other entity (required)}
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
  
  # List template files
  
  path_files <- list.files(path)
  
  is_template <- rep(FALSE, length(path_files))
  
  for (i in 1:length(path_files)){
    
    is_template[i] <- any(
      stringr::str_detect(
        path_files[i], 
        attr.templates$regexpr
      )
    )
    
  }
  
  templates <- vector('list', length(path_files[is_template]))
  
  names(templates) <- path_files[is_template]
  
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
    
    other_entities <- NA_character_
    
  }
  
  # Initialize outgoing list of templates, data tables, and other entities
  
  output <- list(
    template = templates,
    data.table = data_tables,
    other.entity = other_entities
  )
  
  # Add template files to outgoing list ---------------------------------------
  
  # For all templates ...
  
  for (i in 1:length(output$template)){
      
    use_i <- stringr::str_detect(names(output$template[i]), attr.templates$regexpr)
    
    # For non-delimited templates ...
    
    if (is.na(attr.templates$delimiter[use_i])){
      
      # Read file as abstract, additional information, intellectual rights,
      # methods, or taxonomicCoverage
      
      if ((attr.templates$template_name[use_i] == 'abstract') | 
          (attr.templates$template_name[use_i] == 'additional_info') |
          (attr.templates$template_name[use_i] == 'intellectual_rights')){
        
        output$template[[i]]$content <- EML::set_TextType(
          file = paste0(
            path, 
            '/', 
            names(output$template[i])
          )
        )
        
        output$template[[i]]$path <- path
        
      } else if (attr.templates$template_name[use_i] == 'methods'){
        
        output$template[[i]]$content <- EML::set_methods(
          methods_file = paste0(
            path, 
            '/', 
            path_files[i]
          )
        )
        
        output$template[[i]]$path <- path
        
      } else if (attr.templates$template_name[use_i] == 'taxonomicCoverage'){
        
        output$template[[i]]$content <- EML::read_eml(
          paste0(
            path, 
            '/',
            path_files[i]
          )
        )
        
        output$template[[i]]$path <- path
        
      }
      
    # For delimited templates ...
      
    } else if (attr.templates$delimiter[use_i] == '\\t'){
      
      # Read tab delimited table
      
      output$template[[i]]$content <- utils::read.table(
        file = paste0(
          path,
          '/',
          path_files[i]
        ), 
        header = T,
        sep = '\t',
        as.is = T
      )
      
      output$template[[i]]$path <- path
      
    }

  }
  
  # Add data tables to outgoing list ------------------------------------------
  
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
  
  # Add other entities to outgoing list ---------------------------------------
  
  # For all other entities ...
  
  for (i in 1:length(output$other.entity)){
    
    output$other.entity[[i]]$content <- NA
    
    output$other.entity[[i]]$path <- data.path
    
  }
  
  # Return --------------------------------------------------------------------
  
  output
  
}
