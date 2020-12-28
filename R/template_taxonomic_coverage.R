#' Describe biological organisms (taxa)
#'
#' @description Describes biological organisms (taxa) occuring in the data and helps resolve them to authority systems. If matches can be made, then the full taxonomic hierarchy of scientific and common names are automatically rendered in the final EML metadata. This enables future users to search on any taxonomic level of interest across data packages in data repositories.
#'
#' @param path (character) Path to the metadata template directory.
#' @param data.path (character) Path to the data directory.
#' @param taxa.table (character) Table with a column listing all unique taxa in the data.
#' @param taxa.col (character) Column name in \code{taxa.table} listing all unique taxa.
#' @param taxa.name.type (character) Type of name in \code{taxa.col}. Can be: \code{scientific} or \code{common}.
#' @param taxa.authority (integer) Taxonomic authorities as an ordered numeric vector of ID's decreasing in preference. Supported authorities: 3 = \href{https://www.itis.gov/}{ITIS}, 9 = \href{http://www.marinespecies.org/}{WORMS}, 11 = \href{https://www.gbif.org/}{GBIF}.
#' @param empty (logical) Whether to write an empty template file.
#' @param write.file (logical; optional) Whether to write the template file.
#'
#' @return 
#' \item{taxonomic_coverage}{Columns:
#'     \itemize{
#'     \item{taxa_raw: Taxon name as it occurs in the data. Can be single word or species binomial.}
#'     \item{name_type: Type of name. Can be "scientific" or "common".}
#'     \item{name_resolved: Taxon name as found in an authority system.}
#'     \item{authority_system: Authority system in which the taxa’s name was found. Supported authorities are: "\href{https://www.itis.gov/}{ITIS}", "\href{http://www.marinespecies.org/}{WORMS}", "or "\href{https://www.gbif.org/dataset/d7dddbf4-2cf0-4f39-9b2a-bb099caae36c}{GBIF}". For unsupported authorities, list the authority home page URL. For unresolved taxa use \code{NA}.}
#'     \item{authority_id: Taxon identifier in the authority system (e.g. 168469).}
#'     }
#' }
#'     
#' @details 
#'     \code{template_taxonomic_coverage()} searches the most preferred taxonomic authority for all unique taxa listed in \code{taxa.col} returning the authority name and corresponding taxa identifier for direct matches (no fuzzy searching), then the next most preferred taxonomic authority is search for taxa that have not yet been resolved. This process repeats for subsequently listed authorities. "NA" is returned when an authority match is not made.
#'     
#'     \code{make_eml()} expands taxa, resolved to supported authorities, into full taxonomic classification. Each level of classification is accompanied by an annotation (listing the \code{authority} and \code{authority.id}) and common names (only when \code{authority} is ITIS or WORMS). Taxa resolved to unsupported authorities, or not resolved at all, will be listed as is defined in the "name_resolved", "authority_system", and "authority_id" columns.
#'     
#'     Character encoding of metadata extracted directly from the tables are converted to UTF-8 via \code{enc2utf8()}.
#'
#' @examples 
#' \dontrun{
#' # Set working directory
#' setwd("/Users/me/Documents/data_packages/pkg_260")
#' 
#' # For a table containing taxa, try resolving scientific names to ITIS, and if no matches can be made, then try WORMS
#' template_taxonomic_coverage(
#'   path = "./metadata_templates",
#'   data.path = "./data_objects",
#'   taxa.table = "decomp.csv",
#'   taxa.col = "taxa",
#'   taxa.authority = c(3,9),
#'   taxa.name.type = "scientific")
#'   
#' # For manually resolving taxa to unsupported authorities, use an empty template
#' template_taxonomic_coverage(
#'   path = "./metadata_templates",
#'   empty = TRUE)
#'   
#' }
#'
#' @export
#'
template_taxonomic_coverage <- function(path, 
                                        data.path = path,
                                        taxa.table,
                                        taxa.col,
                                        taxa.name.type,
                                        taxa.authority,
                                        empty = FALSE,
                                        write.file = TRUE) {
  
  message('Templating taxonomic coverage ...')
  
  # TODO: Add optional "rank" field to the returned template for manually 
  # defining rank values of unsupported authorities. taxonomyCleanr >= 1.5 
  # supports this.
  
  # TODO: Allow users to input taxa as a vector of character strings like
  # taxonomyCleanr::resolve_sci_taxa().
  
  # Validate arguments --------------------------------------------------------
  
  validate_arguments(
    fun.name = 'template_taxonomic_coverage',
    fun.args = as.list(environment()))
  
  # Break if template exists --------------------------------------------------
  
  if (file.exists(paste0(path, '/taxonomic_coverage.txt'))) {
    message('taxonomic_coverage.txt already exists!')
    return(NULL)
  }
  
  # Return empty --------------------------------------------------------------
  
  if (empty) {
    
    message('taxonomic_coverage.txt')
    
    output <- data.frame(
      name = character(),
      name_type = character(),
      name_resolved = character(),
      authority_system = character(), 
      authority_id = character(),
      stringsAsFactors = FALSE)
    
    suppressWarnings(
      utils::write.table(
        output,
        paste0(path, "/", "taxonomic_coverage.txt"),
        sep = "\t",
        row.names = F,
        quote = F,
        fileEncoding = "UTF-8"))
    
    message("Done.")
    return(output)
    
  }
  
  # Read data -----------------------------------------------------------------
  
  x <- template_arguments(
    data.path = data.path,
    data.table = taxa.table)$x
  
  # Identify unique taxa of each taxa.table -----------------------------------
  
  taxa_raw <- trimws(
    unique(
      unlist(
        lapply(
          seq_along(x$data.table),
          function(i){
            unique(x$data.table[[taxa.table[i]]]$content[ , taxa.col[i]])
          }))))
  
  # Resolve scientific names --------------------------------------------------
  
  if (taxa.name.type == 'scientific') {
    
    # Initialize output
    
    output_scientific <- data.frame(
      name = taxa_raw,
      name_type = rep('scientific', length(taxa_raw)),
      name_resolved = rep(NA_character_, length(taxa_raw)),
      authority_system = rep(NA_character_, length(taxa_raw)), 
      authority_id = rep(NA_character_, length(taxa_raw)),
      stringsAsFactors = FALSE)
    
    # Resolve and add to output
    
    taxa_resolved <- try(
      taxonomyCleanr::resolve_sci_taxa(
        data.sources = taxa.authority,
        x = taxonomyCleanr::trim_taxa(
          x = taxa_raw)), 
      silent = TRUE)
    
    if (is.data.frame(taxa_resolved)) {
      output_scientific$name_resolved <- taxa_resolved$taxa_clean
      output_scientific$authority_system <- taxa_resolved$authority
      output_scientific$authority_id <- taxa_resolved$authority_id
    } else if (class(taxa_resolved) == 'try-error'){
      warning(
        paste0('Taxonomic authorities are not available at this moment. ',
               'Please try your call again later.'))
    }
    
  }
  
  # Resolve common names ------------------------------------------------------
  
  if (taxa.name.type == 'common') {
    
    # Initialize output
    
    output_common <- data.frame(
      name = taxa_raw,
      name_type = rep('common', length(taxa_raw)),
      name_resolved = rep(NA_character_, length(taxa_raw)),
      authority_system = rep(NA_character_, length(taxa_raw)), 
      authority_id = rep(NA_character_, length(taxa_raw)),
      stringsAsFactors = FALSE)
    
    # Resolve and add to output
    
    if (is.data.frame(taxa_resolved)){
      
      taxa_resolved <- taxonomyCleanr::resolve_comm_taxa(
        data.sources = 3,
        x = taxonomyCleanr::trim_taxa(
          x = taxa_raw))
      
      output_common$name_resolved <- taxa_resolved$taxa_clean
      output_common$authority_system <- taxa_resolved$authority
      output_common$authority_id <- taxa_resolved$authority_id
      
    } else if (class(taxa_resolved) == 'try-error') {
      warning(
        paste0('Taxonomic authorities are not available at this moment. ',
               'Please try your call again later.'))
    }
    
  }
  
  # Resolve scientific and common names ---------------------------------------
  
  if (taxa.name.type == 'both'){
    
    # Initialize output
    
    output_scientific <- data.frame(
      name = taxa_raw,
      name_type = rep('scientific', length(taxa_raw)),
      name_resolved = rep(NA_character_, length(taxa_raw)),
      authority_system = rep(NA_character_, length(taxa_raw)), 
      authority_id = rep(NA_character_, length(taxa_raw)),
      stringsAsFactors = FALSE)
    
    output_common <- data.frame(
      name = taxa_raw,
      name_type = rep('common', length(taxa_raw)),
      name_resolved = rep(NA_character_, length(taxa_raw)),
      authority_system = rep(NA_character_, length(taxa_raw)), 
      authority_id = rep(NA_character_, length(taxa_raw)),
      stringsAsFactors = FALSE)
    
    # Resolve and add to output (scientific)
    
    taxa_resolved <- try(
      taxonomyCleanr::resolve_sci_taxa(
        data.sources = taxa.authority,
        x = taxonomyCleanr::trim_taxa(
          x = taxa_raw)), 
      silent = TRUE)
    
    if (is.data.frame(taxa_resolved)){
      output_scientific$name_resolved <- taxa_resolved$taxa_clean
      output_scientific$authority_system <- taxa_resolved$authority
      output_scientific$authority_id <- taxa_resolved$authority_id
    } else if (class(taxa_resolved) == 'try-error') {
      warning(
        paste0('Taxonomic authorities are not available at this moment. ',
               'Please try your call again later.'))
    }
    
    # Resolve and add to output (common)
    
    if (is.data.frame(taxa_resolved)){
      
      taxa_resolved <- taxonomyCleanr::resolve_comm_taxa(
        data.sources = 3,
        x = taxonomyCleanr::trim_taxa(
          x = taxa_raw))
      
      output_common$name_resolved <- taxa_resolved$taxa_clean
      output_common$authority_system <- taxa_resolved$authority
      output_common$authority_id <- taxa_resolved$authority_id
      
    } else if (class(taxa_resolved) == 'try-error') {
      warning(
        paste0('Taxonomic authorities are not available at this moment. ',
               'Please try your call again later.'))
    }
    
  }
  
  # Aggregate outputs ---------------------------------------------------------
  
  if (exists('output_scientific') & exists('output_common')) {
    output <- rbind(output_scientific, output_common)
  } else if (exists('output_scientific') & !exists('output_common')){
    output <- output_scientific
  } else if (!exists('output_scientific') & exists('output_common')){
    output <- output_common
  }
  
  # Encode extracted metadata in UTF-8 ----------------------------------------
  
  output$name <- enc2utf8(output$name)
  
  # Write to file or add to x -------------------------------------------------
  
  if (write.file) {
    
    message('taxonomic_coverage.txt')
    
    suppressWarnings(
      utils::write.table(
        output,
        paste0(path, "/", "taxonomic_coverage.txt"),
        sep = "\t",
        row.names = F,
        quote = F,
        fileEncoding = "UTF-8"))
    
  }
  
  message("Done.")
  return(output)
  
}
