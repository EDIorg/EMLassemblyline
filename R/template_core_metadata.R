#' Describe core features of a data package
#'
#' @description Core information to all data packages (abstract, methods, keywords, personnel, license). Describes what the data are, why and how they were created, who was involved in their creation, and under what license the data are released.
#'
#' @param path 
#'     (character) Path to the metadata template directory.
#' @param license
#'     (character) License to publicly release the data package under. Use
#'     \href{https://creativecommons.org/publicdomain/zero/1.0/}{"CC0"} or 
#'     \href{https://creativecommons.org/licenses/by/4.0/}{"CCBY"}.
#' @param file.type
#'     (character; optional) File type for abstract, methods, and additional info. Can be: ".txt" (plain text), ".docx" (MS Word), or ".md" (markdown). The relative benefits: ".docx" supports basic formatting (super/sub scripts, italics, symbols, accented characters) but doesn't support bulleted lists and formatted equations. ".md" supports less formatting than ".docx" but is open source. ".txt" doesn't support any formatting but is a common file type.
#' @param write.file
#'     (logical; optional) Whether to write the template files.
#' @param x
#'     (named list; optional) Alternative input to 
#'     \code{template_core_metadatta()}. Use \code{template_arguments()} 
#'     to create \code{x}.
#'
#' @return 
#' \item{abstract}{Describes the salient features of a dataset in a concise summary much like an abstract does in a journal article. It should cover what the data are and why they were created.}
#' \item{methods}{Describes the data creation methods. Includes enough detail for future users to correctly use the data. Lists instrument descriptions, protocols, etc.}
#' \item{keywords}{Describes the data in a small set of terms. Keywords facilitate search and discovery on scientific terms, as well as names of research groups, field stations, and other organizations. Using a controlled vocabulary or thesaurus vastly improves discovery. We recommend using the \href{http://vocab.lternet.edu/vocab/vocab/index.php}{LTER Controlled Vocabulary} when possible. Columns:
#'     \itemize{
#'     \item{keyword: One keyword per line}
#'     \item{keywordThesaurus: URI of the vocabulary from which the keyword originates.}
#'     }
#' }
#' \item{personnel}{Describes the personnel and funding sources involved in the creation of the data. This facilitates attribution and reporting. Columns:
#'     \itemize{
#'     \item{givenName: First name}
#'     \item{middleInitial: Middle initial}
#'     \item{surName: Last name}
#'     \item{organizationName: Organization the person belongs to}
#'     \item{electronicMailAddress: Email address}
#'     \item{userId: Persons research identifier (e.g. \href{https://orcid.org/}{ORCID}). Links a persons research profile to a data publication.}
#'     \item{role: Role of the person with respect to the data. Persons serving more than one role are listed on separate lines (e.g. replicate the persons info on separate lines but change the role. Valid options: "creator" (Author of the data. Will appear in the data citation.), "PI" (Principal investigator the data were created under. Will appear with project level metadata.), "contact" (A point of contact for questions about the data. Can be an organization or position (e.g. data manager). To do this, enter the organization or position name under givenName and leave middleInitial and surName empty.), Other roles (e.g. "Field Technician") will be listed as associated parties to the data.}
#'     \item{projectTitle: Title of project the data were created under. If ancillary projects were involved, then add as new lines below the primary project with the PIs info replicated.}
#'     \item{fundingAgency: Agency the project was funded by.}
#'     \item{fundingNumber: Grant or award number.}
#'     }
#' }
#' \item{intellectual_rights}{Describes how the data may be used. Releasing without restriction (\href{https://creativecommons.org/publicdomain/zero/1.0/}{CC0}) or with minimal attribution (\href{https://creativecommons.org/licenses/by/4.0/}{CC BY}) maximizes value and future use.}
#' \item{additional_info}{Ancillary info not captured by any of the other templates.}
#' 
#' @examples
#' \dontrun{
#' # Set working directory
#' setwd("/Users/me/Documents/data_packages/pkg_260")
#' 
#' # For data licensed under CC0 and using .docx files for the abstract and methods
#' template_core_metadata(
#'   path = "./metadata_templates",
#'   license = "CC0",
#'   file.type = ".docx")
#' }
#'     
#' @export     
#'     

template_core_metadata <- function(
  path, 
  license, 
  file.type = '.txt', 
  write.file = TRUE,
  x = NULL){
  
  message('Templating core metadata ...')
  
  # Validate arguments --------------------------------------------------------
  
  # Validate path usage before passing arguments to validate_arguments()
  # When not using x, inputs are expected from path and data.path. When using x, 
  # only data.path is required.
  
  if (is.null(x) & missing(path)){
    stop('Input argument "path" is missing.')
  }
  
  # Pass remaining arguments to validate_arguments().
  
  validate_arguments(
    fun.name = 'template_core_metadata',
    fun.args = as.list(environment())
  )
  
  # Read metadata templates and data ------------------------------------------
  
  # If x doesn't exist ...
  
  if (is.null(x)){
      
    # Use NULL values
    
    x <- template_arguments()
    
    x <- x$x

    # Indicate files have been read
    
    data_read_2_x <- TRUE
    
  } else {
    
    write.file <- FALSE
    
  }
  
  # Import abstract -----------------------------------------------------------
  
  # If writing to file ...
  
  if (isTRUE(write.file)){

    # Write to path
    
    value <- file.copy(
      from = system.file(
        paste0(
          '/templates/abstract',
          file.type
        ),
        package = 'EMLassemblyline'
      ),
      to = paste0(
        path,
        paste0(
          '/abstract',
          file.type
        )
      )
    )
    
    # Send message
    
    if (isTRUE(value)){
      message(paste0('abstract', file.type))
    } else {
      message(paste0('abstract', file.type, ' already exists!'))
    }
    
    # If adding to x ...
    
  } else if (!exists('data_read_2_x')){
    
    if (is.null(x$template[["abstract"]]$content)) {
      x$template[["abstract"]]$content <- EML::set_TextType(
        file = system.file(
          paste0('/templates/abstract', file.type),
          package = 'EMLassemblyline'))
      message(paste0('abstract', file.type))
    } else {
      message(paste0('abstract', file.type, ' already exists!'))
    }
    
  }
  
  # Import additional_info ----------------------------------------------------
  
  # If writing to file ... 
  
  if (isTRUE(write.file)){
    
    # Write to path
    
    value <- file.copy(
      from = system.file(
        paste0(
          '/templates/additional_info',
          file.type
        ),
        package = 'EMLassemblyline'
      ),
      to = paste0(
        path,
        paste0(
          '/additional_info',
          file.type
        )
      )
    )
    
    # Send message
    
    if (isTRUE(value)){
      message(paste0('additional_info', file.type))
    } else {
      message(paste0('additional_info', file.type, ' already exists!'))
    }
    
    # If adding to x ...
    
  } else if (!exists('data_read_2_x')){
    
    if (is.null(x$template[["additional_info"]]$content) |
        (length(x$template[["additional_info"]]$content$para) == 0)) {
      x$template[["additional_info"]]$content <- EML::set_TextType(
        file = system.file(
          paste0('/templates/additional_info', file.type),
          package = 'EMLassemblyline'))
      message(paste0('additional_info', file.type))
    } else {
      message(paste0('additional_info', file.type, ' already exists!'))
    }
    
  }
  
  # Import intellectual_rights.txt --------------------------------------------
  
  # If license is CC0 ...
  
  if (tolower(license) == "cc0"){
    
    # If writing to file ...
    
    if (isTRUE(write.file)){
      
      # Write to path
      
      value <- file.copy(
        from = system.file(
          '/templates/intellectual_rights_cc0.txt',
          package = 'EMLassemblyline'
        ),
        to = paste0(
          path,
          "/intellectual_rights.txt"
        )
      )
      
      # Send message
      
      if (isTRUE(value)){
        message("intellectual_rights.txt")
      } else {
        message("intellectual_rights.txt already exists!")
      }
      
      # If adding x ...
      
    } else if (!exists('data_read_2_x')){
      
      if (is.null(x$template$intellectual_rights.txt$content)) {
        x$template$intellectual_rights.txt$content <- EML::set_TextType(
          file = system.file(
            '/templates/intellectual_rights_cc0.txt',
            package = 'EMLassemblyline'))
        message("intellectual_rights.txt")
      } else {
        message("intellectual_rights.txt already exists!")
      }
      
    }
    
    # If license is CCBY ...
    
  } else if (tolower(license) == "ccby"){
    
    # If writing to file ...
    
    if (isTRUE(write.file)){
      
      # Write to path
      
      value <- file.copy(
        from = system.file(
          '/templates/intellectual_rights_ccby4.0.txt',
          package = 'EMLassemblyline'
        ),
        to = paste0(
          path,
          "/intellectual_rights.txt"
        )
      )
      
      # Send message
      
      if (isTRUE(value)){
        message("intellectual_rights.txt")
      } else {
        message("intellectual_rights.txt already exists!")
      }
      
      # If adding to x ...
      
    } else if (!exists('data_read_2_x')){
      
      if (is.null(x$template$intellectual_rights.txt$content)) {
        x$template$intellectual_rights.txt$content <- EML::set_TextType(
          file = system.file(
            '/templates/intellectual_rights_ccby4.0.txt',
            package = 'EMLassemblyline'))
        message("intellectual_rights.txt")
      } else {
        message("intellectual_rights.txt already exists!")
      }
      
    }
    
  }
  
  # Import keywords.txt -------------------------------------------------------
  
  # If writing to file ...
  
  if (isTRUE(write.file)){
    
    # Write to path
    
    value <- file.copy(
      from = system.file(
        '/templates/keywords.txt',
        package = 'EMLassemblyline'
      ),
      to = paste0(
        path,
        "/keywords.txt"
      )
    )
    
    # Send message
    
    if (isTRUE(value)){
      message("keywords.txt.")
    } else {
      message("keywords.txt already exists!")
    }
    
    # If adding to x ...
    
  } else if (!exists('data_read_2_x')){
    
    if (is.null(x$template$keywords.txt$content)) {
      x$template$keywords.txt$content <- as.data.frame(
        data.table::fread(
          file = system.file(
            '/templates/keywords.txt',
            package = 'EMLassemblyline'
          ),
          colClasses = rep("character", 2),
          fill = TRUE,
          blank.lines.skip = TRUE))
      message("keywords.txt")
    } else {
      message("keywords.txt already exists!")
    }
    
  }
  
  # Import methods ------------------------------------------------------------
  
  # If writing to file ...
  
  if (isTRUE(write.file)){
    
    # Write to path
    
    value <- file.copy(
      from = system.file(
        paste0(
          '/templates/methods',
          file.type
        ),
        package = 'EMLassemblyline'
      ),
      to = paste0(
        path,
        paste0(
          '/methods',
          file.type
        )
      )
    )
    
    # Send message
    
    if (isTRUE(value)){
      message(paste0('methods', file.type))
    } else {
      message(paste0('methods', file.type, ' already exists!'))
    }
    
    # If adding to x ...
    
  } else if (!exists('data_read_2_x')){
    
    if (is.null(x$template[["methods"]]$content)) {
      x$template[["methods"]]$content <- EML::set_methods(
        methods_file = system.file(
          paste0('/templates/methods', file.type),
          package = 'EMLassemblyline'))
      message(paste0('methods', file.type))
    } else {
      message(paste0('methods', file.type, ' already exists!'))
    }
    
  }
  
  # Import personnel.txt ------------------------------------------------------
  
  # If writing to file ...
  
  if (isTRUE(write.file)){
    
    # Write to path
    
    value <- file.copy(
      from = system.file(
        '/templates/personnel.txt',
        package = 'EMLassemblyline'
      ),
      to = paste0(
        path,
        "/personnel.txt"
      )
    )
    
    # Send message
    
    if (isTRUE(value)){
      message("personnel.txt")
    } else {
      message("personnel.txt already exists!")
    }
    
    # If adding to x ...
    
  } else if (!exists('data_read_2_x')){
    
    if (is.null(x$template$personnel.txt$content)) {
      x$template$personnel.txt$content <- as.data.frame(
        data.table::fread(
          file = system.file(
            '/templates/personnel.txt',
            package = 'EMLassemblyline'
          ),
          fill = TRUE,
          blank.lines.skip = TRUE))
      message("personnel.txt")
    } else {
      message("personnel.txt already exists!")
    }
    
  }
  
  # Return --------------------------------------------------------------------
  
  if ((!isTRUE(write.file)) & is.null(x)){
    message('No templates were written to file (write.file = FALSE).')
  }
  
  message("Done.")
  
  if (!exists('data_read_2_x')){
    return(x)
  }
  
}


