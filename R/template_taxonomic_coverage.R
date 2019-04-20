#' Create taxonomic coverage template
#'
#' @description  
#'     Extract unique taxa and resolve to one or more taxonomic authorities. 
#'     \code{make_eml} will use this information to get the taxonomic hierarchy
#'     for each taxa and add to the EML. The metadata resulting from this 
#'     function allows searching on any rank of the taxa.
#'
#' @usage 
#'     template_taxonomic_coverage(
#'       path,
#'       data.path = path,
#'       taxa.table,
#'       taxa.col,
#'       taxa.name.type,
#'       taxa.authority,
#'       x = NULL,
#'       write.file = TRUE
#'     )
#'
#' @param path 
#'     (character) Path to where the template will be written.
#' @param data.path
#'     (character) Path to where \code{taxa.table} is stored.
#' @param taxa.table
#'     (character) Name of data table containing \code{taxa.col}.
#' @param taxa.col
#'     (character) Name of column in \code{taxa.table} containing taxa names.
#'     Names can be single words or species binomials.
#' @param taxa.name.type
#'     (character) Type of taxa names in \code{taxa.col} Can be: 
#'     \code{scientific}, \code{common}, or \code{both}.
#' @param taxa.authority
#'     (integer) An ordered numeric vector of ID's corresponding to data 
#'     sources (i.e. taxonomic authorities) you'd like to resolve taxa names
#'     to, in the order of decreasing preference. See the list of supported
#'     data sources with \code{view_taxa_authorities}. Columns 
#'     "resolve_sci_taxa", and "resolve_comm_taxa" list authorites supporting
#'     scientific and common searches, respectively.
#' @param x
#'     (named list) Alternative input/output to \code{EMLassemblyline} 
#'     functions. Use \code{make_arguments} to create \code{x}.
#' @param write.file
#'     (logical) Write "taxonomic_coverage.txt" to \code{path}.
#'
#' @return 
#'     \itemize{
#'         \item{\strong{taxonomic_coverage.txt} A tab delimited file written 
#'         to \code{path} containing authority system names and authority IDs
#'         for successfully resolved taxa, and "NA" otherwise.
#'         \item{If using \code{x}, then content of "taxonomic_coverage.txt is 
#'         added to \code{x} under "/x/templates".}
#'     }
#'     
#' @details 
#'     \code{template_taxonomic_coverage} searches the most preferred taxonomic
#'     authority for all unique taxa listed in \code{taxa.col} returning
#'     the authority name and corresponding taxa identifier for direct matches
#'     (no fuzzy searching), then the next most preferred taxonomic authority
#'     is search for taxa that have not yet been resolved. This process repeats
#'     for subsequently listed authorities. "NA" is returned when an authority 
#'     match is not made.
#'     
#'     When "taxonomic_coverage.txt" is passed to \code{make_eml}, the 
#'     authority information is used to get the hierarchical rank names of 
#'     resolved taxa and rendered into the "taxonomicCoverage" element of EML.
#'     
#'     Existing "taxonomic_coverage.txt" will not be overwritten by subsequent 
#'     calls to \code{template_taxonomic_coverage}.
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
  x = NULL, 
  write.file = TRUE
  ){
  
  message('Creating taxonomic coverage template.')

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
  
  # Check for existing content
  
  if (is.null(x)){
    if (isTRUE('taxonomic_coverage.txt' %in% list.files(path))){
      stop('taxonomic_coverage.txt already exists.')
    }
  } else if (!is.null(x)){
    if (!is.null(x$template$taxonomic_coverage.txt$content)){
      stop('taxonomic_coverage.txt already exists.')
    }
  }
  
  # Read data -----------------------------------------------------------------

  # Create x if it doesn't exist
  
  if (is.null(x)){
    
    # Validate file name
    
    data_file <- EDIutils::validate_file_names(
      path = data.path, 
      data.files = taxa.table
    )

    # Read templates and data.table into list

    x <- make_arguments(
      data.path = data.path,
      data.table = data_file
    )

    x <- x$x

    data_read_2_x <- TRUE

  }
  
  # Initialize output data frame ----------------------------------------------
  
  taxa_raw <- unique(
    x$data.table[[taxa.table]]$content[ , taxa.col]
  )

  output <- data.frame(
    taxa = taxa_raw,
    name_scientific = rep(NA_character_, length(taxa_raw)),
    authority_system_scientific = rep(NA_character_, length(taxa_raw)), 
    authority_id_scientific = rep(NA_character_, length(taxa_raw)), 
    name_common = rep(NA_character_, length(taxa_raw)),
    authority_system_common = rep(NA_character_, length(taxa_raw)),
    authority_id_common = rep(NA_character_, length(taxa_raw)), 
    stringsAsFactors = FALSE
  )

  # Resolve scientific names --------------------------------------------------
  
  if (taxa.name.type == 'scientific'){
    
    taxa_resolved <- taxonomyCleanr::resolve_sci_taxa(
      data.sources = taxa.authority,
      x = taxonomyCleanr::trim_taxa(
        x = taxa_raw
      )
    )

    output$authority_system_scientific <- taxa_resolved$authority
    
    output$authority_id_scientific <- taxa_resolved$authority_id

  }
  
  # Resolve common names ------------------------------------------------------
  
  if (taxa.name.type == 'common'){

    taxa_resolved <- taxonomyCleanr::resolve_comm_taxa(
      data.sources = 3,
      x = taxonomyCleanr::trim_taxa(
        x = taxa_raw
      )
    )
    
    output$authority_system_common <- taxa_resolved$authority
    
    output$authority_id_common <- taxa_resolved$authority_id
    
  }
  
  # Resolve scientific and common names ---------------------------------------
  
  if (taxa.name.type == 'both'){

    # Scientific
    
    taxa_resolved <- taxonomyCleanr::resolve_sci_taxa(
      data.sources = taxa.authority,
      x = taxonomyCleanr::trim_taxa(
        x = taxa_raw
      )
    )
    
    output$authority_system_scientific <- taxa_resolved$authority
    
    output$authority_id_scientific <- taxa_resolved$authority_id
    
    # Common
    
    taxa_resolved <- taxonomyCleanr::resolve_comm_taxa(
      data.sources = 3,
      x = taxonomyCleanr::trim_taxa(
        x = taxa_raw
      )
    )
    
    output$authority_system_common <- taxa_resolved$authority
    
    output$authority_id_common <- taxa_resolved$authority_id

  }
  
  # Write to file or add to x -------------------------------------------------
  
  if (isTRUE(write.file) & exists('data_read_2_x')){
    
    message('Writing taxonomic_coverage.txt to path')
    
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
  
  message("Done.")

}
