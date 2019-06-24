#' Create taxonomic coverage template
#'
#' @description  
#'     Use this function to extract the unique taxa of a data table and 
#'     try to resolve (match) to a taxonomic authority system (e.g. 
#'     \href{https://www.itis.gov/}{ITIS}) and return for user supplied 
#'     inputs if necessary. This information is later used to list the full
#'     hierarchical rank names in the metadata.
#'     \href{https://ediorg.github.io/EMLassemblyline/articles/edit_metadata_templates.html}{Instructions for editing this template.}
#'
#' @usage 
#'     template_taxonomic_coverage(
#'       path,
#'       data.path = path,
#'       taxa.table,
#'       taxa.col,
#'       taxa.name.type,
#'       taxa.authority,
#'       write.file = TRUE,
#'       x = NULL
#'     )
#'
#' @param path 
#'     (character) Path to the metadata template directory.
#' @param data.path
#'     (character) Path to the data directory.
#' @param taxa.table
#'     (character) Table name containing \code{taxa.col}. If inputting more 
#'     than one table, then supply as a list (e.g. 
#'     \code{c('table.1', 'table.2')}).
#' @param taxa.col
#'     (character) Column name containing taxa names. Species binomials are
#'     accepted. If inputting more than one table, then supply as a list
#'     \code{c('taxa.col.table.1', 'taxa.col.table.2')}.
#' @param taxa.name.type
#'     (character) Taxa name type. Can be: 
#'     \code{scientific}, \code{common}, or \code{both}.
#' @param taxa.authority
#'     (integer) Taxonomic authorities to resolve to as an ordered numeric vector 
#'     of ID's decreasing in preference. See the list of supported authorities 
#'     with \code{taxonomyCleanr::view_taxa_authorities()}. Columns 
#'     "resolve_sci_taxa", and "resolve_comm_taxa" list authorites supporting 
#'     scientific and common searches, respectively.
#' @param write.file
#'     (logical; optional) Whether to write the template file.
#' @param x
#'     (named list; optional) Alternative input to 
#'     \code{template_taxonomic_coverage()}. Use \code{template_arguments()} 
#'     to create \code{x}.
#'
#' @return 
#'     \strong{taxonomic_coverage.txt} The tab delimited taxonomic coverage 
#'     template. This file is written to \code{path} unless using \code{x},
#'     in which case the template is added to 
#'     \strong{/x/templates/taxonomic_coverage.txt}. Non-resolved taxa result 
#'     NA.
#'     
#' @details 
#'     \code{template_taxonomic_coverage()} searches the most preferred 
#'     taxonomic authority for all unique taxa listed in \code{taxa.col} 
#'     returning the authority name and corresponding taxa identifier for 
#'     direct matches (no fuzzy searching), then the next most preferred 
#'     taxonomic authority is search for taxa that have not yet been resolved. 
#'     This process repeats for subsequently listed authorities. "NA" is 
#'     returned when an authority match is not made.
#'     
#'     When taxonomic_coverage.txt is passed to \code{make_eml()}, the 
#'     authority information is used to get the hierarchical rank names of 
#'     resolved taxa and rendered into the "taxonomicCoverage" element of EML.
#'     
#'     Existing taxonomic_coverage.txt will not be overwritten by subsequent 
#'     calls to \code{template_taxonomic_coverage()}.
#'
#' @examples 
#' # Initialize data package directory for template_taxonomic_coverage()
#' file.copy(
#'   from = system.file('/examples/pkg_255', package = 'EMLassemblyline'),
#'   to = tempdir(),
#'   recursive = TRUE
#' )
#' 
#' # Set working directory
#' setwd(paste0(tempdir(), '/pkg_255'))
#' 
#' # View directory contents (NOTE: taxonomic_coverage.txt doesn't exist)
#' dir('./metadata_templates')
#' 
#' # Template taxonomic coverage
#' template_taxonomic_coverage(
#'   path = './metadata_templates',
#'   data.path = './data_objects',
#'   taxa.table = 'decomp.csv',
#'   taxa.col = 'taxa',
#'   taxa.authority = c(3,11),
#'   taxa.name.type = 'scientific'
#' )
#' 
#' # View directory contents (NOTE: taxonomic_coverage.txt exists)
#' dir('./metadata_templates')
#' 
#' # Rerunning template_taxonomic_coverage() does not overwrite file
#' template_taxonomic_coverage(
#'   path = './metadata_templates',
#'   data.path = './data_objects',
#'   taxa.table = 'decomp.csv',
#'   taxa.col = 'taxa',
#'   taxa.authority = c(3,11),
#'   taxa.name.type = 'scientific'
#' )
#' 
#' # Clean up
#' unlink('.', recursive = TRUE)
#'
#' @export
#'

template_taxonomic_coverage <- function(
  path, 
  data.path = path, 
  taxa.table,
  taxa.col,
  taxa.name.type,
  taxa.authority, 
  write.file = TRUE,
  x = NULL
){
  
  message('Templating taxonomic coverage ...')
  
  # Validate arguments --------------------------------------------------------
  
  # Validate path usage before passing arguments to validate_arguments()
  # When not using x, inputs are expected from path and data.path.
  # When using x, only data.path is used.
  
  if (is.null(x) & missing(path)){
    stop('Input argument "path" is missing.')
  } else if (!is.null(x) & missing(path)){
    path <- NULL
    data.path <- NULL
  }
  
  # Pass remaining arguments to validate_arguments()
  
  validate_arguments(
    fun.name = 'template_taxonomic_coverage',
    fun.args = as.list(environment())
  )
  
  # Read data -----------------------------------------------------------------
  
  # Create x if it doesn't exist
  
  if (is.null(x)){
    
    # Validate file name
    
    taxa.table <- EDIutils::validate_file_names(
      path = data.path, 
      data.files = taxa.table
    )
    
    # Read templates and data.table into list
    
    x <- template_arguments(
      data.path = data.path,
      data.table = taxa.table
    )
    
    x <- x$x
    
    data_read_2_x <- TRUE
    
    # Does file exist?
    
    f_exists <- file.exists(
      paste0(
        path,
        '/taxonomic_coverage.txt'
      )
    )
    
  } else if (!is.null(x)){
    
    # Does file exist?
    
    f_exists <- is.data.frame(x$template$taxonomic_coverage.txt$content)
    
  }
  
  # Check if taxonomic_coverage.txt exists ------------------------------------
  
  if (isTRUE(f_exists)){
    
    message('taxonomic_coverage.txt already exists!')
    
  } else {
    
    # Identify unique taxa of each taxa.table ---------------------------------
    
    taxa_raw <- unique(
      unlist(
        lapply(
          seq_along(x$data.table),
          function(i){
            unique(
              x$data.table[[taxa.table[i]]]$content[ , taxa.col[i]]
            )
          }
        )
      )
    )
    
    # Resolve scientific names ------------------------------------------------
    
    if (taxa.name.type == 'scientific'){
      
      # Initialize output
      
      output_scientific <- data.frame(
        name = taxa_raw,
        name_type = rep('scientific', length(taxa_raw)),
        name_resolved = rep(NA_character_, length(taxa_raw)),
        authority_system = rep(NA_character_, length(taxa_raw)), 
        authority_id = rep(NA_character_, length(taxa_raw)),
        stringsAsFactors = FALSE
      )
      
      # Resolve and add to output
      
      taxa_resolved <- try(
        taxonomyCleanr::resolve_sci_taxa(
          data.sources = taxa.authority,
          x = taxonomyCleanr::trim_taxa(
            x = taxa_raw
          )
        ), 
        silent = TRUE
      )
      
      if (is.data.frame(taxa_resolved)){
        
        output_scientific$name_resolved <- taxa_resolved$taxa_clean
        
        output_scientific$authority_system <- taxa_resolved$authority
        
        output_scientific$authority_id <- taxa_resolved$authority_id
        
      } else if (class(taxa_resolved) == 'try-error'){
        
        warning('Taxonomic authorities are not available at this moment. Please try your call again later.')
        
      }
      
    }
    
    # Resolve common names ----------------------------------------------------
    
    if (taxa.name.type == 'common'){
      
      # Initialize output
      
      output_common <- data.frame(
        name = taxa_raw,
        name_type = rep('common', length(taxa_raw)),
        name_resolved = rep(NA_character_, length(taxa_raw)),
        authority_system = rep(NA_character_, length(taxa_raw)), 
        authority_id = rep(NA_character_, length(taxa_raw)),
        stringsAsFactors = FALSE
      )
      
      # Resolve and add to output
      
      if (is.data.frame(taxa_resolved)){
        
        taxa_resolved <- taxonomyCleanr::resolve_comm_taxa(
          data.sources = 3,
          x = taxonomyCleanr::trim_taxa(
            x = taxa_raw
          )
        )
        
        output_common$name_resolved <- taxa_resolved$taxa_clean
        
        output_common$authority_system <- taxa_resolved$authority
        
        output_common$authority_id <- taxa_resolved$authority_id
        
      } else if (class(taxa_resolved) == 'try-error'){
        
        warning('Taxonomic authorities are not available at this moment. Please try your call again later.')
        
      }
      
    }
    
    # Resolve scientific and common names -------------------------------------
    
    if (taxa.name.type == 'both'){
      
      # Initialize output
      
      output_scientific <- data.frame(
        name = taxa_raw,
        name_type = rep('scientific', length(taxa_raw)),
        name_resolved = rep(NA_character_, length(taxa_raw)),
        authority_system = rep(NA_character_, length(taxa_raw)), 
        authority_id = rep(NA_character_, length(taxa_raw)),
        stringsAsFactors = FALSE
      )
      
      output_common <- data.frame(
        name = taxa_raw,
        name_type = rep('common', length(taxa_raw)),
        name_resolved = rep(NA_character_, length(taxa_raw)),
        authority_system = rep(NA_character_, length(taxa_raw)), 
        authority_id = rep(NA_character_, length(taxa_raw)),
        stringsAsFactors = FALSE
      )
      
      # Resolve and add to output (scientific)
      
      taxa_resolved <- try(
        taxonomyCleanr::resolve_sci_taxa(
          data.sources = taxa.authority,
          x = taxonomyCleanr::trim_taxa(
            x = taxa_raw
          )
        ), 
        silent = TRUE
      )
      
      if (is.data.frame(taxa_resolved)){
        
        output_scientific$name_resolved <- taxa_resolved$taxa_clean
        
        output_scientific$authority_system <- taxa_resolved$authority
        
        output_scientific$authority_id <- taxa_resolved$authority_id
        
      } else if (class(taxa_resolved) == 'try-error'){
        
        warning('Taxonomic authorities are not available at this moment. Please try your call again later.')
        
      }
      
      # Resolve and add to output (common)
      
      if (is.data.frame(taxa_resolved)){
        
        taxa_resolved <- taxonomyCleanr::resolve_comm_taxa(
          data.sources = 3,
          x = taxonomyCleanr::trim_taxa(
            x = taxa_raw
          )
        )
        
        output_common$name_resolved <- taxa_resolved$taxa_clean
        
        output_common$authority_system <- taxa_resolved$authority
        
        output_common$authority_id <- taxa_resolved$authority_id
        
      } else if (class(taxa_resolved) == 'try-error'){
        
        warning('Taxonomic authorities are not available at this moment. Please try your call again later.')
        
      }
      
    }
    
    # Aggregate outputs -------------------------------------------------------
    
    if (exists('output_scientific') & exists('output_common')){
      
      output <- rbind(
        output_scientific,
        output_common
      )
      
    } else if (exists('output_scientific') & !exists('output_common')){
      
      output <- output_scientific
      
    } else if (!exists('output_scientific') & exists('output_common')){
      
      output <- output_common
      
    }
    
    # Write to file or add to x -----------------------------------------------
    
    if (isTRUE(write.file) & exists('data_read_2_x')){
      
      message('taxonomic_coverage.txt')
      
      suppressWarnings(
        utils::write.table(
          output,
          paste0(
            path,
            "/",
            "taxonomic_coverage.txt"
          ),
          sep = "\t",
          row.names = F,
          quote = F,
          fileEncoding = "UTF-8"
        )
      )
      
    } else if (!exists('data_read_2_x')){
      
      message('Adding taxonomic_coverage.txt to x')
      
      x$template$taxonomic_coverage.txt$content <- output
      
      return(x)
      
    }
    
  }
  
  message("Done.")
  
}
