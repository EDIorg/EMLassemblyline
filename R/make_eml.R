#' Make EML metadata
#'
#' @description  
#'     Render the contents of metadata templates into EML, validate, and write
#'     to file.
#'     
#' @usage 
#'     make_eml(
#'       path,
#'       data.path = path,
#'       eml.path = path, 
#'       dataset.title,
#'       temporal.coverage,
#'       geographic.description, 
#'       geographic.coordinates, 
#'       maintenance.description, 
#'       data.table = NULL, 
#'       data.table.name = data.table,
#'       data.table.description = NULL, 
#'       data.table.quote.character = NULL, 
#'       data.table.url = NULL,
#'       other.entity = NULL,
#'       other.entity.name = other.entity,
#'       other.entity.description = NULL,
#'       other.entity.url = NULL,
#'       provenance = NULL,
#'       user.id = NULL,
#'       user.domain = NULL,
#'       package.id = NULL,
#'       write.file = TRUE,
#'       return.obj = FALSE,
#'       x = NULL
#'     )
#'     
#' @param path 
#'     (character) Path to the metadata template directory.
#' @param data.path
#'     (character) Path to the data directory.
#' @param eml.path
#'     (character) Path to the EML directory, where EML files are written.
#' @param dataset.title
#'     (character) Title of the dataset.
#' @param temporal.coverage
#'     (character) Beginning and ending dates of the dataset in the format 
#'     "YYYY-MM-DD" (e.g. 
#'     \code{temporal.coverage = c('2012-05-01', '2014-11-30')}).
#' @param geographic.description
#'     (character) Description of datasets geographic extent. Don't use this 
#'     argument if geographic coverage is supplied by geographic_coverage.txt.
#' @param geographic.coordinates
#'     (character) Coordinates of datasets geographic extent. Coordinates are
#'     listed in this order: North, East, South, West (e.g. 
#'     \code{geographic.coordinates = c('28.38', '-119.95', '28.38', '-119.95')}).
#'     Longitudes west of the prime meridian and latitudes south of the equator 
#'     are negative. Don't use this argument if geographic coverage is supplied
#'     by geographic_coverage.txt.
#' @param maintenance.description
#'     (character) Data collection status ("ongoing" or "complete").
#' @param data.table
#'     (character; optional) Table file name. If more than one, then supply 
#'     as a vector of character strings (e.g. 
#'     \code{data.table = c("nitrogen.csv", "decomp.csv")}).
#' @param data.table.name
#'     (character; optional) A short descriptive name for the table. Defaults
#'     to \code{data.table}. If more than one, then supply as a vector of 
#'     character strings in the same order as listed in \code{data.table}.
#' @param data.table.description
#'     (character; optional) Table description. If more than one, then supply 
#'     as a vector of character strings in the same order as listed in 
#'     \code{data.table}.
#' @param data.table.quote.character
#'     (character; optional) Quote character used in \code{data.table}. If 
#'     more than one, then supply as a vector of character strings in the same 
#'     order as listed in \code{data.table}. If the quote character is a quotation, 
#'     then enter \code{"\\""}. If the quote character is an apostrophe, then 
#'     enter \code{"\\'"}.
#' @param data.table.url
#'     (character; optional) The publicly accessible URL from which 
#'     \code{data.table} can be downloaded. If more than one, then supply as 
#'     a vector of character strings in the same order as listed in 
#'     \code{data.table}.
#' @param other.entity
#'     (character; optional) Name of \code{other.entity}(s) in this 
#'     dataset. Use \code{other.entity} for all non-\code{data.table} files. 
#'     \code{other.entity}(s) should be stored at \code{data.path}. If more 
#'     than one, then supply as a vector of character strings (e.g. 
#'     \code{other.entity = c('ancillary_data.zip', 'quality_control.R')}).
#' @param other.entity.name
#'     (character; optional) A short descriptive name for the other.entity. 
#'     Defaults to \code{other.entity}. If more than one, then supply as a 
#'     vector of character strings in the same order as listed in 
#'     \code{other.entity}.
#' @param other.entity.description
#'     (character; optional) Description(s) of \code{other.entity}(s). If more 
#'     than one, then supply as a vector of descriptions in the same order as 
#'     listed in \code{other.entity}.
#' @param other.entity.url
#'     (character; optional) The publicly accessible URL from which 
#'     \code{other.entity} can be downloaded. If more than one, then supply as 
#'     a vector of character strings in the same order as listed in 
#'     \code{other.entity}.
#' @param provenance
#'     (character; optional) EDI Data Repository Data package ID(s) 
#'     corresponding to parent datasets from which this dataset was created 
#'     (e.g. \code{knb-lter-cap.46.3}).
#' @param user.id
#'     (character; optional) ID(s) of data repository user account(s). If more 
#'     than one, supply as a vector of character strings.
#' @param user.domain
#'     (character; optional) Domain of the \code{user.id}(s). Valid options 
#'     for EDI are "LTER" and "EDI". If more than one, supply as a vector of 
#'     character strings in the same order as corresponding \code{user.id}(s).
#' @param package.id
#'     (character; optional) Data Repository data package ID for this dataset. A 
#'     missing package ID defaults to "edi.101.1".
#' @param write.file
#'     (logical; optional) Whether to write the EML file.
#' @param return.obj
#'     (logical; optional) Whether to return the EML as an R object of class 
#'     \code{EML object}.
#' @param x
#'     (named list; optional) Alternative input to 
#'     \code{make_eml()}. Use \code{template_arguments()} 
#'     to create \code{x}.
#'     
#' @return 
#'     \itemize{
#'         \item{\strong{EML file} written to \code{eml.path}.}
#'         \item{\strong{EML object} when \code{return.obj = TRUE}.}
#'     }
#'     
#' @details 
#'     \code{make_eml()} reads the contents of metadata templates, 
#'     auto-extracts additional metadata from the data entities, appends value 
#'     added content (e.g. resolving keywords to controlled vocabularies), and 
#'     adds all the metadata content to locations in the EML schema according 
#'     with best practice recommendations of scientists, data managers, and 
#'     data repositories. The EML is then validated against the schema and 
#'     written to file.
#'
#' @examples 
#' # Initialize data package directory for make_eml()
#' file.copy(
#'   from = system.file('/examples/pkg_260', package = 'EMLassemblyline'),
#'   to = tempdir(),
#'   recursive = TRUE
#' )
#' 
#' # Set working directory
#' setwd(paste0(tempdir(), '/pkg_260'))
#' 
#' # Make EML (for data package with data tables)
#' 
#' make_eml(
#'   path = './metadata_templates',
#'   data.path = './data_objects',
#'   eml.path = './eml',
#'   dataset.title = 'Sphagnum and Vascular Plant Decomposition under Increasing Nitrogen Additions',
#'   temporal.coverage = c('2014-05-01', '2015-10-31'),
#'   maintenance.description = 'completed',
#'   data.table = c('decomp.csv', 'nitrogen.csv'),
#'   data.table.description = c('Decomposition data', 'Nitrogen data'),
#'   user.id = 'csmith',
#'   user.domain = 'EDI',
#'   package.id = 'edi.260.1'
#' )
#' 
#' # View EML directory contents (NOTE: edi.260.1 exists)
#' dir('./eml')
#' 
#' # Make EML (for data package with other entities)
#' 
#' make_eml(
#'   path = './metadata_templates',
#'   data.path = './data_objects',
#'   eml.path = './eml',
#'   dataset.title = 'Sphagnum and Vascular Plant Decomposition under Increasing Nitrogen Additions',
#'   temporal.coverage = c('2014-05-01', '2015-10-31'),
#'   maintenance.description = 'completed',
#'   other.entity = c('ancillary_data.zip', 'processing_and_analysis.R'),
#'   other.entity.description = c('Ancillary data', 'Data processing and analysis script'),
#'   user.id = 'csmith',
#'   user.domain = 'EDI',
#'   package.id = 'edi.260.2'
#' )
#' 
#' # View EML directory contents (NOTE: edi.260.2 exists)
#' dir('./eml')
#' 
#' # Make EML (for data package with data tables and other entities)
#' 
#' make_eml(
#'   path = './metadata_templates',
#'   data.path = './data_objects',
#'   eml.path = './eml',
#'   dataset.title = 'Sphagnum and Vascular Plant Decomposition under Increasing Nitrogen Additions',
#'   temporal.coverage = c('2014-05-01', '2015-10-31'),
#'   maintenance.description = 'completed',
#'   data.table = c('decomp.csv', 'nitrogen.csv'),
#'   data.table.description = c('Decomposition data', 'Nitrogen data'),
#'   other.entity = c('ancillary_data.zip', 'processing_and_analysis.R'),
#'   other.entity.description = c('Ancillary data', 'Data processing and analysis script'),
#'   user.id = 'csmith',
#'   user.domain = 'EDI',
#'   package.id = 'edi.260.3'
#' )
#' 
#' # View EML directory contents (NOTE: edi.260.3 exists)
#' dir('./eml')
#' 
#' # Clean up
#' unlink('.', recursive = TRUE)
#'
#' @export
#'

make_eml <- function(
  path,
  data.path = path,
  eml.path = path, 
  dataset.title,
  temporal.coverage,
  geographic.description, 
  geographic.coordinates, 
  maintenance.description, 
  data.table = NULL, 
  data.table.name = data.table,
  data.table.description = NULL, 
  data.table.quote.character = NULL, 
  data.table.url = NULL,
  other.entity = NULL,
  other.entity.name = other.entity,
  other.entity.description = NULL,
  other.entity.url = NULL,
  provenance = NULL,
  user.id = NULL,
  user.domain = NULL,
  package.id = NULL,
  write.file = TRUE,
  return.obj = FALSE,
  x = NULL,
  affiliation,
  data.files,
  data.files.description,
  data.files.quote.character,
  data.files.url,
  data.url = NULL,
  zip.dir,
  zip.dir.description
  ){
  
  # Validate arguments --------------------------------------------------------
  
  # Validate path usage before passing arguments to validate_arguments()
  # When not using x, inputs are expected from path, data.path, and 
  # eml.path. When using x, only data.path is required unless write.file = TRUE
  # in which case eml.path is required.
  
  if (is.null(x) & missing(path)){
    stop('Input argument "path" is missing.')
  } else if (!is.null(x) & missing(path)){
    path <- NULL
    if (missing(data.path)){
      stop('Input argument "data.path" is missing.')
    }
    if (isTRUE(write.file) & missing(eml.path)){
      stop('Input argument "write.file = TRUE" but "eml.path" is missing.')
    } else if (!isTRUE(write.file) & missing(eml.path)){
      eml.path <- NULL
    }
  }
  
  # Pass remaining arguments to validate_arguments().
  
  validate_arguments(
    fun.name = 'make_eml',
    fun.args = as.list(environment()))
  
  # Handle deprecated arguments
  
  if (!missing(affiliation)){
    warning(
      'Argument "affiliation" is deprecated; please use "user.domain" instead.',
      call. = FALSE)
    user.domain <- affiliation
  }
  
  if (!missing(data.files)){
    warning(
      'Argument "data.files" is deprecated; please use "data.table" instead.',
      call. = FALSE)
    data.table <- data.files
  }
  
  if (!missing(data.files.description)){
    warning(
      'Argument "data.files.description" is deprecated; please use "data.table.description" instead.',
      call. = FALSE)
    data.table.description <- data.files.description
  }
  
  if (!missing(data.files.quote.character)){
    warning(
      'Argument "data.files.quote.character" is deprecated; please use "data.table.quote.character" instead.',
      call. = FALSE)
    data.table.quote.character <- data.files.quote.character
  }
  
  if (!missing(data.files.url)){
    warning(
      'Argument "data.files.url" is deprecated; please use "data.url" instead.',
      call. = FALSE)
    data.url <- data.files.url
  }
  
  # FIXME: Do not remove until March 2021
  if (!missing(data.url)){
    warning(
      paste0("Argument 'data.url' is deprecated; please use 'data.table.url' ",
             "and 'other.entity.url' instead."),
      call. = FALSE)
  }
  
  if (!missing(zip.dir)){
    warning(
      'Argument "zip.dir" is deprecated; please use "other.entity" instead.',
      call. = FALSE)
    other.entity <- zip.dir
  }
  
  if (!missing(zip.dir.description)){
    warning(
      'Argument "zip.dir.description" is deprecated; please use "other.entity.description" instead.',
      call. = FALSE)
    other.entity.description <- zip.dir.description
  }
  
  # Read metadata templates and data ------------------------------------------
  
  if (is.null(x)) {
    
    if (is.null(data.table) & is.null(other.entity)) {      
      
      x <- template_arguments(
        path = path,
        data.path = data.path)$x
      
    } else if (!is.null(data.table) & is.null(other.entity)) {
      
      table_names <- suppressWarnings(
        EDIutils::validate_file_names(
          path = data.path, 
          data.files = data.table))
      x <- template_arguments(
        path = path,
        data.path = data.path,
        data.table = table_names)$x
      
    } else if (!is.null(data.table) & !is.null(other.entity)) {
      
      table_names <- suppressWarnings(
        EDIutils::validate_file_names(
          path = data.path, 
          data.files = data.table))
      x <- template_arguments(
        path = path,
        data.path = data.path,
        data.table = table_names,
        other.entity = other.entity)$x

    } else if (is.null(data.table) & !is.null(other.entity)) {
      
      x <- template_arguments(
        path = path,
        data.path = data.path,
        other.entity = other.entity)$x

    }
    
    data_read_2_x <- TRUE
    
  }

  # Validate metadata templates -----------------------------------------------
  
  x <- remove_empty_templates(x)
  validate_templates("make_eml", x)

  # Read personnel file
  
  personinfo <- x$template$personnel.txt$content
  personinfo$role <- tolower(personinfo$role)
  personinfo <- validate_personnel(x = personinfo)
  
  # Load helper funcitions ----------------------------------------------------
  
  # Load helper function to set personnel roles
  
  set_person <- function(info_row, person_role){
    
    if (person_role == "contact"){
      
      # If contact only has givenName then contact is an organization ...
      
      if ((personinfo[info_row, "givenName"] != "") & (personinfo[info_row, "middleInitial"] == "") & (personinfo[info_row, "surName"] == "")){
        
        contact <- list(
          organizationName = trimws(personinfo[info_row,"organizationName"]),
          positionName = stringr::str_to_title(trimws(personinfo[info_row,"givenName"])),
          electronicMailAddress = trimws(personinfo[info_row,"electronicMailAddress"])
        )

        if (nchar(trimws(personinfo[info_row,"userId"])) == 19){
          contact$userId <- list(
            directory = 'https://orcid.org',
            paste0("https://orcid.org/", trimws(personinfo[info_row,"userId"]))
          )
        }
        
      # ... otherwise contact is a person
        
      } else {
        
        contact <- list(
          individualName = list(
            givenName = list(
              trimws(personinfo[info_row,"givenName"]),
              trimws(personinfo[info_row,"middleInitial"])
            ),
            surName = trimws(personinfo[info_row,"surName"])
          ),
          organizationName = trimws(personinfo[info_row,"organizationName"]),
          electronicMailAddress = trimws(personinfo[info_row,"electronicMailAddress"])
        )

        if (nchar(trimws(personinfo[info_row,"userId"])) == 19){
          contact$userId <- list(
            directory = 'https://orcid.org',
            paste0("https://orcid.org/", trimws(personinfo[info_row,"userId"]))
          )
        }
        
        # FIXME Blank entries ('') result in closing tags when EML is written
        # to file. Need function to set all elements of value = '' to NULL.
        contact <- rapply(
          contact,
          function(x){
            if (x == ''){
              x <- NULL
            } else {
              x
            }
          },
          how = c('replace')
        )
        
      }

      contact
      
      # If person is a creator ...
      
    } else if (person_role == "creator"){
      
      creator <- list(
        individualName = list(
          givenName = list(
            trimws(personinfo[info_row,"givenName"]),
            trimws(personinfo[info_row,"middleInitial"])
          ),
          surName = trimws(personinfo[info_row,"surName"])
        ),
        organizationName = trimws(personinfo[info_row,"organizationName"]),
        electronicMailAddress = trimws(personinfo[info_row,"electronicMailAddress"])
      )

      if (nchar(trimws(personinfo[info_row,"userId"])) == 19){
        creator$userId <- list(
          directory = 'https://orcid.org',
          paste0("https://orcid.org/", trimws(personinfo[info_row,"userId"]))
        )
      }
      
      # FIXME Blank entries ('') result in closing tags when EML is written
      # to file. Need function to set all elements of value = '' to NULL.
      creator <- rapply(
        creator,
        function(x){
          if (x == ''){
            x <- NULL
          } else {
            x
          }
        },
        how = c('replace')
      )

      creator
      
      # ... otherwise if person is a Principal Investigator ...
      
    } else if (person_role == "pi"){
      
      rp_personnel <- list(
        individualName = list(
          givenName = list(
            trimws(personinfo[info_row,"givenName"]),
            trimws(personinfo[info_row,"middleInitial"])
          ),
          surName = trimws(personinfo[info_row,"surName"])
        ),
        organizationName = trimws(personinfo[info_row,"organizationName"]),
        electronicMailAddress = trimws(personinfo[info_row,"electronicMailAddress"]),
        role = 'Principal Investigator'
      )

      if (nchar(trimws(personinfo[info_row,"userId"])) == 19){
        rp_personnel$userId <- list(
          directory = 'https://orcid.org',
          paste0("https://orcid.org/", trimws(personinfo[info_row,"userId"]))
        )
      }
      
      # FIXME Blank entries ('') result in closing tags when EML is written
      # to file. Need function to set all elements of value = '' to NULL.
      rp_personnel <- rapply(
        rp_personnel,
        function(x){
          if (x == ''){
            x <- NULL
          } else {
            x
          }
        },
        how = c('replace')
      )
      
      rp_personnel
      
      # ... otherwise the person is an associatedParty ...
      
    } else {
      
      # If givenName, middleName, and surName are blank then the 
      # associatedParty is an organization ...
      
      if ((personinfo[info_row, "givenName"] == "") & (personinfo[info_row, "middleInitial"] == "") & (personinfo[info_row, "surName"] == "")){
        
        associated_party = list(
          organizationName = trimws(personinfo[info_row,"organizationName"]),
          electronicMailAddress = trimws(personinfo[info_row,"electronicMailAddress"]),
          role = stringr::str_to_title(trimws(personinfo[info_row,"role"]))
        )

        if (nchar(trimws(personinfo[info_row,"userId"])) == 19){
          associated_party$userId <- list(
            directory = 'https://orcid.org',
            paste0("https://orcid.org/", trimws(personinfo[info_row,"userId"]))
          )
        }

        associated_party
        
        # ... otherwise the associatedParty is a person.
        
      } else {

        associated_party <- list(
          individualName = list(
            givenName = list(
              trimws(personinfo[info_row,"givenName"]),
              trimws(personinfo[info_row,"middleInitial"])
            ),
            surName = trimws(personinfo[info_row,"surName"])
          ),
          organizationName = trimws(personinfo[info_row,"organizationName"]),
          electronicMailAddress = trimws(personinfo[info_row,"electronicMailAddress"]),
          role = stringr::str_to_title(trimws(personinfo[info_row,"role"]))
        )
        
        if (nchar(trimws(personinfo[info_row,"userId"])) == 19){
          associated_party$userId <- list(
            directory = 'https://orcid.org',
            paste0("https://orcid.org/", trimws(personinfo[info_row,"userId"]))
          )
        }
        
        # FIXME Blank entries ('') result in closing tags when EML is written
        # to file. Need function to set all elements of value = '' to NULL.
        associated_party <- rapply(
          associated_party,
          function(x){
            if (x == ''){
              x <- NULL
            } else {
              x
            }
          },
          how = c('replace')
        )

        associated_party
        
      }
      
    }
    
  }
  
  # Build nodes ---------------------------------------------------------------
  
  message("Making EML ...")
  message('Creating nodes ...')

  # Create EML
  
  message('<eml>')
  
  # Create <access> -----------------------------------------------------------
  
  message("  <access>")
  
  # Set default user.id and user.domain, if undefined
  
  if (is.null(user.id)){
    warning('No "user.id" was supplied. The default "someuserid" will be used.')
    user.id <- 'someuserid'
  } else if (is.null(user.domain)){
    warning('No "user.domain" was supplied. The default "user.domain" will be used.')
    user.domain <- 'someuserdomain'
  }
  
  # List principals and permissions
  
  list_of_allow_principals <- list()
  list_of_allow_permissions <- list()
  
  for (i in 1:length(user.id)){
    if (user.domain[i] == 'LTER'){
      list_of_allow_principals[[i]] <- paste0(
        'uid=',
        user.id[i],
        ',o=',
        user.domain[i],
        ',dc=ecoinformatics,dc=org'
      )
    } else if (user.domain[i] == 'EDI'){
      list_of_allow_principals[[i]] <- paste0(
        'uid=',
        user.id[i],
        ',o=',
        user.domain[i],
        ',dc=edirepository,dc=org'
      )
    }
    list_of_allow_permissions[[i]] <- c(
      "all",
      "read")
  }
  
  list_of_allow_principals[[(length(list_of_allow_principals)+1)]] <- c("public")
  list_of_allow_permissions[[(length(list_of_allow_permissions)+1)]] <- c("read")

  # Create <access>

  access <- list(
    scope = 'document',
    order = 'allowFirst',
    authSystem = 'https://pasta.edirepository.org/authentication',
    allow = list(
    )
  )
  
  for (i in 1:length(list_of_allow_principals)){
    access$allow[[i]] <- list(
      principal = list_of_allow_principals[[i]][1],
      permission = list_of_allow_permissions[[i]][1]
    )
  }

  # Create <dataset> ----------------------------------------------------------
  
  message("  <dataset>")

  dataset <- list()
  
  # Create <title> ------------------------------------------------------------
  
  message('    <title>')
  
  dataset$title <- dataset.title
  
  # Create <creator> ----------------------------------------------------------

  useI <- which(personinfo$role == "creator")

  creator <- list()
  for (j in 1:length(useI)){
    message('    <creator>')
    creator[[j]] <- set_person(
      info_row = useI[j],
      person_role = "creator"
    )
  }

  dataset$creator <- creator

  # Create <associatedParty> --------------------------------------------------
  
  useI <- which(
    personinfo$role != "pi" &
    personinfo$role != "creator" &
    personinfo$role != "contact"
  )
  
  if (length(useI) != 0){
    associated_party_list <- list()
    for (j in 1:length(useI)){
      message("    <associatedParty>")
      associated_party_list[[j]] <- suppressWarnings(
        set_person(
          info_row = useI[j],
          person_role = ""
        )
      )
    }
    dataset$associatedParty <- associated_party_list
  }
  
  # Create <pubDate> ----------------------------------------------------------
  
  message("    <pubDate>")

  dataset$pubDate <- format(Sys.time(), "%Y-%m-%d")

  # Create <abstract> ---------------------------------------------------------
  
  if (any(stringr::str_detect(names(x$template), 'abstract'))) {
    message("    <abstract>")
    dataset$abstract <- x$template[[
      names(x$template)[stringr::str_detect(names(x$template), 'abstract')]
      ]]$content
  }
  
  # Create <keywordSet> -------------------------------------------------------
  
  if (!is.null(x$template$keywords.txt)){
    
    message("    <keywordSet>")
    keywords <- x$template$keywords.txt$content
    
    # Remove blank keyword entries
    
    use_i <- keywords$keyword == ""
    if (sum(use_i) > 0){
      keywords <- keywords[!use_i, ]
    } 
    
    # Try resolving keywords without a listed thesaurus to the LTER Controlled 
    # Vocabulary
    
    if (sum(keywords$keywordThesaurus == '') > 0){
      
      unresolved_terms <- keywords[keywords$keywordThesaurus == '', 'keyword']
      
      results <- try(
        EDIutils::vocab_resolve_terms(
          x = unresolved_terms,
          cv = 'lter'
        ),
        silent = T
      )
      
      if (is.data.frame(results)){
        results <- results[results$controlled_vocabulary != '', ]
        use_i <- match(results$term, keywords$keyword)
        keywords[use_i, 'keywordThesaurus'] <- results[ , 'controlled_vocabulary']
      }
      
    }
    
    # Build keywordSet
    
    keywordSet <- list()
    uni_keywordThesaurus <- unique(keywords$keywordThesaurus)
    for (i in 1:length(uni_keywordThesaurus)){
      
      keyword <- list()
      use_i <- uni_keywordThesaurus[i] == keywords[["keywordThesaurus"]]
      kws <- keywords$keyword[use_i]
      for (k in 1:length(kws)){
        message('    <keyword>')
        keyword[[k]] <- kws[k]
      }
      
      keywordSet[[i]] <- list(
        keyword = keyword,
        keywordThesaurus = uni_keywordThesaurus[i]
      )
      
      if (keywordSet[[i]]$keywordThesaurus == ''){
        keywordSet[[i]]$keywordThesaurus <- NULL
      } else {
        message('    <keywordThesaurus>')
      }
      
    }
    dataset$keywordSet <- keywordSet
  }
  
  

  # Create <additionalInfo> ---------------------------------------------------
  
  if (any(stringr::str_detect(names(x$template), 'additional_info'))){
    message("   <additionalInfo>")
    dataset$additionalInfo <- x$template[[
      names(x$template)[stringr::str_detect(names(x$template), 'additional_info')]]]$content
  }

  # Create <intellectualRights> -----------------------------------------------

  if (!is.null(stringr::str_detect(names(x$template), 'intellectual_rights'))) {
    message("    <intellectualRights>")
    dataset$intellectualRights <- x$template$intellectual_rights.txt$content
  }

  # Create <coverage> ---------------------------------------------------------
  
  message('    <coverage>')
  
  dataset$coverage <- list()
  
  # Create <geographicCoverage> -----------------------------------------------
  
  if (missing(geographic.coordinates) & 
      !any(stringr::str_detect(names(x$template), "geographic_coverage.txt|bounding_boxes.txt"))) {
    stop(
      paste0("No geographic coverage found. Please add using the ",
        "'geographic.coordinates' and 'geographic.description' arguments to ",
        "make_eml(), or the bounding_boxes.txt or ",
        "geographic_coverage.txt templates."), 
      call. = F)
  }
  
  geographicCoverage <- list()
  
  # Add coverage defined in arguments of make_eml
  
  if (!missing(geographic.coordinates) & !missing(geographic.description)){
    
    message("      <geographicCoverage>")
    
    geographicCoverage[[(length(geographicCoverage)+1)]] <- list(
      geographicDescription = geographic.description,
      boundingCoordinates = list(
        westBoundingCoordinate = as.character(geographic.coordinates[4]),
        eastBoundingCoordinate = as.character(geographic.coordinates[2]),
        northBoundingCoordinate = as.character(geographic.coordinates[1]),
        southBoundingCoordinate = as.character(geographic.coordinates[3])
      )
    )
    
  }
  
  # Add coverage defined in bounding_boxes.txt ...
  
  if (!is.null(x$template$bounding_boxes.txt)){
    
    bounding_boxes <- x$template$bounding_boxes.txt$content
    
    if (is.data.frame(bounding_boxes)){
      
      warning(
        'Template "bounding_boxes.txt" is deprecated; please use "geographic_coverage.txt" instead.',
        call. = FALSE)
      
      if (nrow(bounding_boxes) != 0){
        
        bounding_boxes_2 <- lapply(bounding_boxes, as.character)
        
        for (i in 1:length(bounding_boxes_2$geographicDescription)){
          
          message("      <geographicCoverage>")
          
          geographicCoverage[[(length(geographicCoverage)+1)]] <- list(
            geographicDescription = bounding_boxes_2$geographicDescription[i],
            boundingCoordinates = list(
              westBoundingCoordinate = as.character(bounding_boxes_2$westBoundingCoordinate[i]),
              eastBoundingCoordinate = as.character(bounding_boxes_2$eastBoundingCoordinate[i]),
              northBoundingCoordinate = as.character(bounding_boxes_2$northBoundingCoordinate[i]),
              southBoundingCoordinate = as.character(bounding_boxes_2$southBoundingCoordinate[i])
            )
          )
          
        }
        
      }
      
    }

  }
  
  # If geographic_coverage.txt exists ...
  
  if (!is.null(x$template$geographic_coverage.txt)){
    
    # Add coverage defined in new version of geographic_coverage.txt ...
    
    bounding_boxes <- x$template$geographic_coverage.txt$content
    
    if (is.data.frame(bounding_boxes)){
      
      if (all(colnames(bounding_boxes) %in% 
              c('northBoundingCoordinate', 'southBoundingCoordinate', 'eastBoundingCoordinate', 
                'westBoundingCoordinate', 'geographicDescription'))){
        
        if (nrow(bounding_boxes) != 0){
          
          bounding_boxes_2 <- lapply(bounding_boxes, as.character)
          
          for (i in 1:length(bounding_boxes_2$geographicDescription)){
            
            message("      <geographicCoverage>")
            
            geographicCoverage[[(length(geographicCoverage)+1)]] <- list(
              geographicDescription = bounding_boxes_2$geographicDescription[i],
              boundingCoordinates = list(
                westBoundingCoordinate = as.character(bounding_boxes_2$westBoundingCoordinate[i]),
                eastBoundingCoordinate = as.character(bounding_boxes_2$eastBoundingCoordinate[i]),
                northBoundingCoordinate = as.character(bounding_boxes_2$northBoundingCoordinate[i]),
                southBoundingCoordinate = as.character(bounding_boxes_2$southBoundingCoordinate[i])
              )
            )
            
          }
          
        }
        
        # Add coverage defined in old version of geographic_coverage.txt ...
        
      } else if (all(colnames(bounding_boxes) %in% 
                     c('site', 'latitude', 'longitude'))){
        
        if (nrow(bounding_boxes) != 0){
          
          bounding_boxes_2 <- lapply(bounding_boxes, as.character)
          
          
          for (i in 1:length(bounding_boxes_2$site)){
            
            message("      <geographicCoverage>")
            
            geographicCoverage[[(length(geographicCoverage)+1)]] <- list(
              geographicDescription = bounding_boxes_2$site[i],
              boundingCoordinates = list(
                westBoundingCoordinate = as.character(bounding_boxes_2$longitude[i]),
                eastBoundingCoordinate = as.character(bounding_boxes_2$longitude[i]),
                northBoundingCoordinate = as.character(bounding_boxes_2$latitude[i]),
                southBoundingCoordinate = as.character(bounding_boxes_2$latitude[i])
              )
            )

          }
          
        }
        
      }

    }
    
  }
  
  # Add to dataset
  
  dataset$coverage$geographicCoverage <- geographicCoverage

  # Create <temporalCoverage> -------------------------------------------------
  
  message("      <temporalCoverage>")

  dataset$coverage$temporalCoverage <- list(
    rangeOfDates = list(
      beginDate = list(
        calendarDate = temporal.coverage[1]
      ),
      endDate = list(
        calendarDate = temporal.coverage[2]
      )
    )
  )
  
  # Create <taxonomicCoverage> ------------------------------------------------
  
  if ('taxonomicCoverage.xml' %in% names(x$template)){

    if (is.list(x$template$taxonomicCoverage.xml$content)){

      message("      <taxonomicCoverage>")

      dataset$coverage$taxonomicCoverage <- x$template$taxonomicCoverage.xml$content

    }

  }
  
  if ('taxonomic_coverage.txt' %in% names(x$template)){

    if (is.data.frame(x$template$taxonomic_coverage.txt$content)){

      if (sum(is.na(x$template$taxonomic_coverage.txt$content$authority_id)) != nrow(x$template$taxonomic_coverage.txt$content)){

        message('      <taxonomicCoverage>')

        tc <- try(
          suppressMessages(
            taxonomyCleanr::make_taxonomicCoverage(
              taxa.clean = x$template$taxonomic_coverage.txt$content$name_resolved,
              authority = x$template$taxonomic_coverage.txt$content$authority_system,
              authority.id = x$template$taxonomic_coverage.txt$content$authority_id,
              write.file = FALSE
            )
          ),
          silent = T
        )

        if (class(tc)[1] == 'list'){

          dataset$coverage$taxonomicCoverage <- tc

        }

      }

    }

  }

  # Create <maintenance> ------------------------------------------------------
  
  message("    <maintenance>")
  
  dataset$maintenance$description <- maintenance.description

  # Create <contact> ----------------------------------------------------------

  useI <- which(personinfo$role == "contact")

  contact_list <- list()
  for (j in 1:length(useI)){
    message("    <contact>")
    contact_list[[j]] <- set_person(info_row = useI[j],
                                    person_role = "contact")
  }

  dataset$contact <- contact_list

  # Create <methods> ----------------------------------------------------------
  
  if (any(stringr::str_detect(names(x$template), 'methods'))) {
    message("    <methods>")
    dataset$methods <- x$template[[
      names(x$template)[stringr::str_detect(names(x$template), 'methods')]]]$content
  }

  # Create <methodStep> (provenance) ------------------------------------------
  
  if (!is.null(provenance)){
    environment <- 'production'
    for (p in 1:length(provenance)){
      message('      <methodStep> (provenance metadata)')
      prov_pkg_id <- stringr::str_replace_all(provenance[p], '\\.', '/')
      r <- httr::GET(url = paste0(EDIutils::url_env(environment), 
                                  '.lternet.edu/package/provenance/eml/', 
                                  prov_pkg_id))
      if (r$status_code == 200){
        prov_metadata <- httr::content(r, encoding = 'UTF-8')
        # Remove IDs from creator and contact
        xml2::xml_set_attr(
          xml2::xml_find_all(prov_metadata, './/dataSource/creator'),
          'id', NULL
        )
        xml2::xml_set_attr(
          xml2::xml_find_all(prov_metadata, './/dataSource/contact'),
          'id', NULL
        )
        
        # Write to data.path
        lib_path <- system.file('/examples/templates/abstract.txt', package = 'EMLassemblyline')
        lib_path <- substr(lib_path, 1, nchar(lib_path) - 32)
        if (!stringr::str_detect(stringr::str_replace_all(data.path, '\\\\', '/'), lib_path)){
          xml2::write_xml(
            prov_metadata,
            paste0(data.path,
                   '/provenance_metadata.xml')
          )
        }
        
        # Read provenance file and add to L0 EML

        prov_metadata <- EML::read_eml(paste0(data.path, '/provenance_metadata.xml'))
        methods_step <- dataset@methods@methodStep
        methods_step[[length(methods_step)+1]] <- prov_metadata
        dataset@methods@methodStep <- methods_step
        
        # Delete provenance_metadata.xml (a temporary file)
        lib_path <- system.file('/examples/templates/abstract.txt', package = 'EMLassemblyline')
        lib_path <- substr(lib_path, 1, nchar(lib_path) - 32)
        if (!stringr::str_detect(stringr::str_replace_all(data.path, '\\\\', '/'), lib_path)){
          file.remove(
            paste0(data.path, '/provenance_metadata.xml')
          )
        }
      } else {
        message('Unable to get provenance metadata.')
      }
    }
  }
  
  # Create <project> ----------------------------------------------------------
  
  useI <- which(personinfo$role == "pi")
  
  if (!identical(useI, integer(0))){
    
    message("    <project>")
    
    pi_list <- list()
    pi_list[[1]] <- suppressWarnings(set_person(info_row = useI[1],
                                                person_role = "pi"))
    
    # If no projectTitle ...
    if (personinfo$projectTitle[useI[1]] == ""){
      # If no fundingAgency ...
      if (personinfo$fundingAgency[useI[1]] == ""){
        # If no fundingNumber ...
        if (personinfo$fundingNumber[useI[1]] == ""){
          project <- list(
            title = "No project title to report",
            personnel = pi_list,
            funding = "No funding to report"
          )
        # ... if fundingNumber is present ...
        } else if (personinfo$fundingNumber[useI[1]] != ""){
          project <- list(
            title = "No project title to report",
            personnel = pi_list,
            funding = personinfo$fundingNumber[useI[1]]
          )
        }
      # ... if fundingAgency is present ...
      } else if (personinfo$fundingAgency[useI[1]] != ""){
        # ... if fundingNumber is missing ...
        if (personinfo$fundingNumber[useI[1]] == ""){
          project <- list(
            title = "No project title to report",
            personnel = pi_list,
            funding = personinfo$fundingAgency[useI[1]]
          )
        # ... if fundingNumber is present ...
        } else if (personinfo$fundingNumber[useI[1]] != ""){
          project <- list(
            title = "No project title to report",
            personnel = pi_list,
            funding = paste0(personinfo$fundingAgency[useI[1]],
                             ": ",
                             personinfo$fundingNumber[useI[1]])
          )
        }
      }
    # ... if projectTitle is present ...
    } else if (personinfo$projectTitle[useI[1]] != ""){
      # ... if fundingAgency is present ...
      if (personinfo$fundingAgency[useI[1]] == ""){
        # ... if fundingNumber is present ...
        if (personinfo$fundingNumber[useI[1]] == ""){
          project <- list(
            title = personinfo$projectTitle[useI[1]],
            personnel = pi_list,
            funding = "No funding to report"
          )
        # ... if fundingNumber is missing ...
        } else if (personinfo$fundingNumber[useI[1]] != ""){
          project <- list(
            title = personinfo$projectTitle[useI[1]],
            personnel = pi_list,
            funding = personinfo$fundingNumber[useI[1]]
          )
        }
      # ... if fundingAgency is missing ...
      } else if (personinfo$fundingAgency[useI[1]] != ""){
        # ... if fundingNumber is present ...
        if (personinfo$fundingNumber[useI[1]] == ""){
          project <- list(
            title = personinfo$projectTitle[useI[1]],
            personnel = pi_list,
            funding = personinfo$fundingAgency[useI[1]]
          )
        # ... if fundingNumber is missing ...
        } else if (personinfo$fundingNumber[useI[1]] != ""){
          project <- list(
            title = personinfo$projectTitle[useI[1]],
            personnel = pi_list,
            funding = paste0(personinfo$fundingAgency[useI[1]],
                             ": ",
                             personinfo$fundingNumber[useI[1]])
          )
        }
      }
    }
    dataset$project <- project
    
    # Related project
    
    if (length(useI) > 1){
      relatedProject_list <- list()
      pi_list <- list()
      for (i in 1:(length(useI)-1)){
        message('      <relatedProject>')
        pi_list[[1]] <- suppressWarnings(set_person(info_row = useI[i+1],
                                                    person_role = "pi"))
        # If projectTitle is absent ...
        if (personinfo$projectTitle[useI[i+1]] == ""){
          # If fundingAgency is absent ...
          if (personinfo$fundingAgency[useI[i+1]] == ""){
            # If fundingNumber is absent ...
            if (personinfo$fundingNumber[useI[i+1]] == ""){
              relatedProject_list[[i]] <- list(
                title = "No project title to report",
                personnel = pi_list,
                funding = "No funding to report"
              )
            # ... if fundingNumber is missing ...  
            } else if (personinfo$fundingNumber[useI[i+1]] != ""){
              relatedProject_list[[i]] <- list(
                title = "No project title to report",
                personnel = pi_list,
                funding = personinfo$fundingNumber[useI[i+1]]
              )
            }
          # ... if fundingAgency is present ...
          } else if (personinfo$fundingAgency[useI[i+1]] != ""){
            # ... if fundingNumber is missing ...
            if (personinfo$fundingNumber[useI[i+1]] == ""){
              relatedProject_list[[i]] <- list(
                title = "No project title to report",
                personnel = pi_list,
                funding = personinfo$fundingAgency[useI[i+1]]
              )
            # ... if fundingNumber is present ...
            } else if (personinfo$fundingNumber[useI[i+1]] != ""){
              relatedProject_list[[i]] <- list(
                title = "No project title to report",
                personnel = pi_list,
                funding = paste0(personinfo$fundingAgency[useI[i+1]],
                                 ": ",
                                 personinfo$fundingNumber[useI[i+1]])
              )
            }
          }
        # ... if projectTitle is present ...
        } else if (personinfo$projectTitle[useI[i+1]] != ""){
          # ... if fundingAgency is present ...
          if (personinfo$fundingAgency[useI[i+1]] != ""){
            # ... if fundingNumber is present ...
            if (personinfo$fundingNumber[useI[i+1]] != ""){
              relatedProject_list[[i]] <- list(
                title = personinfo$projectTitle[useI[i+1]],
                personnel = pi_list,
                funding = paste0(personinfo$fundingAgency[useI[i+1]],
                                 ": ",
                                 personinfo$fundingNumber[useI[i+1]])
              )
            # ... if fundingNumber is missing ...
            } else if (personinfo$fundingNumber[useI[i+1]] == ""){
              relatedProject_list[[i]] <- list(
                title = personinfo$projectTitle[useI[i+1]],
                personnel = pi_list,
                funding = personinfo$fundingAgency[useI[i+1]]
              )
            }
          # ... if fundingAgency is missing ...
          } else if (personinfo$fundingAgency[useI[i+1]] == ""){
            # ... if fundingNumber is missing ...
            if (personinfo$fundingNumber[useI[i+1]] == ""){
              relatedProject_list[[i]] <- list(
                title = personinfo$projectTitle[useI[i+1]],
                personnel = pi_list,
                funding = "No funding to report"
              )
            # ... if fundingNumber is present ...
            } else if (personinfo$fundingNumber[useI[i+1]] != ""){
              relatedProject_list[[i]] <- list(
                title = personinfo$projectTitle[useI[i+1]],
                personnel = pi_list,
                funding = paste0(personinfo$fundingAgency[useI[i+1]],
                                 ": ",
                                 personinfo$fundingNumber[useI[i+1]])
              )
            }
          }
        }
      }
      dataset$project$relatedProject <- relatedProject_list
    }
    
  }
  
  # Create <dataTable> --------------------------------------------------------
  
  # Compile attributes
  
  if (!is.null(data.table)){
    attributes_in <- compile_attributes(x = x)
  }
  
  # Create dataTable
  
  data_tables_stored <- list()

  if (!is.null(x$data.table)){
    for (i in 1:length(names(x$data.table))){
      
      message(
        paste0(
          '    <dataTable> (',
          names(x$data.table)[i],
          ')'
        )
      )
      
      attributes <- attributes_in[[1]][[i]]
      
      df_table <- x$data.table[[i]]$content
      
      # Get catvars
      
      fname_table_catvars <- paste0(
        'catvars_',
        stringr::str_remove(
          string = names(x$data.table)[i],
          pattern = '\\.[:alpha:]*$'
        ),
        '.txt'
      )
      
      if (fname_table_catvars %in% names(x$template)){
        
        catvars <- x$template[[fname_table_catvars]]$content
        
        # If content is present
        
        if (dim(catvars)[1] > 0){
          
          for (j in 1:dim(catvars)[2]){
            catvars[ ,j] <- as.character(catvars[ ,j])
          }
          
          non_blank_rows <- nrow(catvars) - sum(catvars$attributeName == "")
          catvars <- catvars[1:non_blank_rows, 1:3]
          
          # Clean extraneous white spaces from catvars tables
          
          if (dim(catvars)[1] != 0){
            for (j in 1:ncol(catvars)){
              if (class(catvars[ ,j]) == "character" ||
                  (class(catvars[ ,j]) == "factor")){
                catvars[ ,j] <- trimws(catvars[ ,j])
              }
            }
          }
          
        }
        
        # Clean extraneous white spaces from attributes
        
        for (j in 1:ncol(attributes)){
          if (class(attributes[ ,j]) == "character" ||
              (class(attributes[ ,j]) == "factor")){
            attributes[ ,j] <- trimws(attributes[ ,j])
          }
        }
        
        # Create the attributeList element
        
        attributeList <- suppressWarnings(
          EML::set_attributes(
            attributes[ , c(
              'attributeName', 
              'formatString',
              'unit',
              'numberType',
              'definition',
              'attributeDefinition',
              'minimum',
              'maximum',
              'missingValueCode',
              'missingValueCodeExplanation')],
            factors = catvars,
            col_classes = attributes[ ,"columnClasses"]
          )
        )
        
      } else {
        
        # Clean extraneous white spaces from attributes
        
        for (j in 1:ncol(attributes)){
          if (class(attributes[ ,j]) == "character" ||
              (class(attributes[ ,j]) == "categorical")){
            attributes[ ,j] <- trimws(attributes[ ,j])
          }
        }

        # Create the attributeList element
        
        attributeList <- suppressWarnings(
          EML::set_attributes(
            attributes[ , c(
              'attributeName', 
              'formatString',
              'unit',
              'numberType',
              'definition',
              'attributeDefinition',
              'minimum',
              'maximum',
              'missingValueCode',
              'missingValueCodeExplanation')],
            col_classes = attributes[ ,"columnClasses"]
          )
        )
        
      }

      # Set physical

      physical <- suppressMessages(
        EML::set_physical(
          paste0(data.path, '/', names(x$data.table)[i]),
          numHeaderLines = "1",
          recordDelimiter = EDIutils::get_eol(
            path = data.path,
            file.name = names(x$data.table)[i],
            os = EDIutils::detect_os()
          ),
          attributeOrientation = "column",
          url = 'placeholder'
        )
      )

      if (!is.null(data.table.quote.character)){
        physical$dataFormat$textFormat$simpleDelimited$quoteCharacter <- data.table.quote.character[i]
      }

      # FIXME: data.url is deprecated. Remove support for this argument after (11 March 2021)
      if (!is.null(data.url)){
        physical$distribution$online$url[[1]] <- paste0(
          data.url, "/", names(x$data.table)[i])
      } else if (!is.null(data.table.url)) {
        physical$distribution$online$url <- data.table.url[i]
      } else {
        physical$distribution <- list()
      }

      physical$dataFormat$textFormat$simpleDelimited$fieldDelimiter <- EDIutils::detect_delimeter(
        path = data.path,
        data.files = data.table[i],
        os = EDIutils::detect_os()
      )
      
       # Pull together information for the data table
      
      data_table <- list(
        entityName = data.table.name[i],
        entityDescription = data.table.description[i],
        physical = physical,
        attributeList = attributeList,
        numberOfRecords = as.character(nrow(df_table))
      )
      
      # FIXME: EML v2.0.0 handles absense of missingValue codes differently
      # than EML v 1.0.3. This fixes the issue here, though it may be better
      # to implement the fix in EML v2.0.0.
      for (j in seq_along(data_table$attributeList$attribute)){
        if (data_table$attributeList$attribute[[j]]$missingValueCode$code == ''){
          data_table$attributeList$attribute[[j]]$missingValueCode$code <- NA_character_
          data_table$attributeList$attribute[[j]]$missingValueCode$codeExplanation <- NA_character_
        }
      }
      
      data_tables_stored[[i]] <- data_table
      
    }

    # Are custom units present in these tables?
    
    if ('custom_units.txt' %in% names(x$template)){
      
      custom_units_df <- x$template$custom_units.txt$content
      
      if (is.data.frame(custom_units_df)){
        
        if (nrow(custom_units_df) < 1){
          custom_units <- "no"
        } else {
          custom_units <- "yes"
        }
        
        # Clean white spaces from custom_units and units_types
        
        if (custom_units == "yes"){
          
          message("    <additionalMetadata>")
          
          for (j in 1:ncol(custom_units_df)){
            if (class(custom_units_df[ ,j]) == "character" ||
                (class(custom_units_df[ ,j]) == "factor")){
              custom_units_df[ ,j] <- trimws(custom_units_df[ ,j])
            }
          }
          
          unitsList <- EML::set_unitList(custom_units_df)
        }

      } else {
      
      custom_units <- "no"
      
      }
      
    }
    
    
    # Compile data tables
    
    dataset$dataTable <- data_tables_stored
    
  }
  
  # Create <otherEntity> ------------------------------------------------------
  
  if (!is.null(x$other.entity)){
    
    if (length(names(x$other.entity)) > 0){
      
      list_of_other_entity <- list()
      
      for (i in 1:length(other.entity)){
        
        message(
          paste0(
            '    <otherEntity> (', 
            names(x$other.entity)[i],
            ')'
          )
        )
        
        # Create new other entity element
        
        otherEntity <- list()
        
        # Add entityName
        
        otherEntity$entityName <- list(other.entity.name[i])

        # Add entityDescription
        
        otherEntity$entityDescription <- other.entity.description[i]
        
        # Add physical
        
        physical <- suppressMessages(
          EML::set_physical(
            paste0(data.path, '/', names(x$other.entity)[i])
          )
        )
        
        physical$dataFormat$textFormat <- NULL

        physical$dataFormat$externallyDefinedFormat$formatName <- 'unknown'

        # FIXME: data.url is deprecated. Remove support for this argument after (11 March 2021)
        if (!is.null(data.url)){
          physical$distribution$online$url[[1]] <- paste0(
            data.url, "/", names(x$other.entity)[i])
        } else if (!is.null(other.entity.url)) {
          physical$distribution$online$url <- other.entity.url[i]
        }else {
          physical$distribution <- list()
        }

        otherEntity$physical <- physical

        # Add entityType

        otherEntity$entityType <- 'unknown'
        
        # Add otherEntity to list
        
        list_of_other_entity[[i]] <- otherEntity
        
      }
      
      dataset$otherEntity <- list_of_other_entity
      
    }
    
    
  }
  
  # Create <additionalMetadata> -----------------------------------------------
  
  if (exists('custom_units')){
    if (custom_units != 'no'){
      if (!exists("unitsList")) {
        unitsList <- EML::set_unitList(x$template$custom_units.txt$content)
      }
      message('  <additionalMetadata>')
      additionalMetadata <- list()
      additionalMetadata[[(length(additionalMetadata)+1)]] <- list(metadata = list(unitList = unitsList))
    }
  }

  # Compile nodes -------------------------------------------------------------
  
  # Set default package.id
  
  if (is.null(package.id)){
    package.id <- 'edi.101.1'
  }

  # Compile nodes
  
  if (exists('custom_units')){
    
    if (custom_units == "yes"){
      
      eml <- list(
        schemaLocation = "eml://ecoinformatics.org/eml-2.1.1  https://nis.lternet.edu/schemas/EML/eml-2.1.1/eml.xsd",
        packageId = package.id,
        system = "edi",
        access = access,
        dataset = dataset,
        additionalMetadata = additionalMetadata
      )

    } else {
      
      eml <- list(
        schemaLocation = "eml://ecoinformatics.org/eml-2.1.1  https://nis.lternet.edu/schemas/EML/eml-2.1.1/eml.xsd",
        packageId = package.id,
        system = "edi",
        access = access,
        dataset = dataset
      )
      
    }
    
  } else {
    
    eml <- list(
      schemaLocation = "eml://ecoinformatics.org/eml-2.1.1  https://nis.lternet.edu/schemas/EML/eml-2.1.1/eml.xsd",
      packageId = package.id,
      system = "edi",
      access = access,
      dataset = dataset
    )
    
  }
  
  message('</eml>')
  
  # Write EML -----------------------------------------------------------------
  
  if (isTRUE(write.file)){
    
    message(paste0('Writing EML (', package.id, '.xml)'))
    emld::eml_version("eml-2.1.1")
    EML::write_eml(
      eml, 
      paste0(eml.path, "/", package.id, ".xml")
    )
    
  }
  
  # Validate EML --------------------------------------------------------------
  
  message("Validating EML")

  validation_result <- EML::eml_validate(eml)

  if (validation_result == "TRUE"){

    message("EML passed validation!")

  } else {

    message("EML validaton failed. See warnings for details.")

  }

  message("Done.")

  if (isTRUE(return.obj)){
    eml
  }
  
}

# Compile attributes ----------------------------------------------------------

# This is a helper function for make_eml.R. 
# It compiles attributes, retrieves minimum and maximum values for numeric data
# and reformats the attributes table.


compile_attributes <- function(x){
  
  data.table <- names(x$data.table)
  
  # Get names of data files with associated attribute files
  
  # files <- names(x$template)
  # use_i <- stringr::str_detect(string = files,
  #                     pattern = "^attributes")
  # attribute_files <- files[use_i]
  # fname_table_attributes <- attribute_files
  # # table_names_base <- stringr::str_sub(string = attribute_files,
  # #                             start = 12,
  # #                             end = nchar(attribute_files)-4)
  # # use_i <- stringr::str_detect(string = names(x$data.table),
  # #                     pattern = stringr::str_c("^", table_names_base, collapse = "|"))
  # # table_names <- names(x$data.table)
  # 
  # # Synchronize ordering of data files and attribute files
  # 
  fname_table_attributes <- paste0(
    'attributes_',
    stringr::str_remove(
      string = data.table,
      pattern = '\\.[:alpha:]*$'
    ),
    '.txt'
  )
  
  # FIXME: Clean up attributes and catvars.
  # - Ignore units when not numeric
  # - Ignore date time format strings when not classified as date
  # - Ignore blank (i.e. "") categorical codes (implement this in the metadata 
  # quality check functions to be developed? See GitHub issue #46)
  
  
  # Loop through data tables --------------------------------------------------
  
  attributes_stored <- list()
  
  for (i in 1:length(data.table)){

    df_table <- x$data.table[[data.table[i]]]$content
    
    df_attributes <- x$template[[fname_table_attributes[i]]]$content

    # Convert user inputs to consistent case
    
    df_attributes$class <- tolower(df_attributes$class)
    
    # Validate attributes -----------------------------------------------------
    
    df_attributes$unit[is.na(df_attributes$unit)] <- ""
    df_attributes$dateTimeFormatString[is.na(df_attributes$dateTimeFormatString)] <- ""
    
    # Modify attributes -------------------------------------------------------
    
    # Modify attributes: remove units class != numeric
    
    use_i <- df_attributes$class != "numeric"
    use_i2 <- df_attributes$unit == ""
    use_i3 <- use_i2 != use_i
    
    if (sum(use_i3) > 0){
      df_attributes$unit[use_i3] <- ""
    }

    # Initialize outgoing attribute table 
    
    rows <- ncol(df_table)
    attributes <- data.frame(attributeName = character(rows),
                             formatString = character(rows),
                             unit = character(rows),
                             numberType = character(rows),
                             definition = character(rows),
                             attributeDefinition = character(rows),
                             columnClasses = character(rows),
                             minimum = character(rows),
                             maximum = character(rows),
                             missingValueCode = character(rows),
                             missingValueCodeExplanation = character(rows),
                             stringsAsFactors = FALSE)
    
    attributes$attributeName <- df_attributes$attributeName
    
    # Set attribute definition (i.e. "attributeDefinition")
    
    attributes$attributeDefinition <- df_attributes$attributeDefinition
    
    # Set attribute class
    
    attributes$columnClasses <- df_attributes$class
    
    # Set attribute units
    
    attributes$unit <- df_attributes$unit
    
    # Set attribute date time format string
    
    attributes$formatString <- df_attributes$dateTimeFormatString
    
    # Set attribute missing value code
    
    attributes$missingValueCode <- df_attributes$missingValueCode
    
    use_i <- is.na(attributes$missingValueCode)
    attributes$missingValueCode[use_i] <- "NA"
    
    # Set attribute missing value code explanation
    
    attributes$missingValueCodeExplanation <- df_attributes$missingValueCodeExplanation
    
    # Remove missing value codes, set attribute number type, then minimumm and 
    # maximum values. Throw an error if non-numeric values are found.
    
    is_numeric <- which(attributes$columnClasses == "numeric")
    attributes$minimum <- as.numeric(attributes$minimum)
    attributes$maximum <- as.numeric(attributes$maximum)
    
    if (!identical(is_numeric, integer(0))){
      
      for (j in 1:length(is_numeric)){
        
        raw <- df_table[ ,is_numeric[j]]
        
        
        if (attributes$missingValueCode[is_numeric[j]] != ""){
          useI <- raw == attributes$missingValueCode[is_numeric[j]]
          raw <- as.numeric(raw[!useI])
        }
        
        rounded <- floor(raw)
        if (length(raw) - sum(raw == rounded, na.rm = T) > 0){
          attributes$numberType[is_numeric[j]] <- "real"
        } else if (min(raw, na.rm = T) > 0){
          attributes$numberType[is_numeric[j]] <- "natural"
        } else if (min(raw, na.rm = T) < 0){
          attributes$numberType[is_numeric[j]] <- "integer"
        } else {
          attributes$numberType[is_numeric[j]] <- "whole"
        }
        
        # If all numeric values are NA then set min and max to NA
        
        if (length(raw) == sum(is.na(raw))){
          attributes$minimum[is_numeric[j]] <- NA
          attributes$maximum[is_numeric[j]] <- NA
        } else {
          attributes$minimum[is_numeric[j]] <- round(min(raw,
                                                         na.rm = TRUE),
                                                     digits = 2)
          attributes$maximum[is_numeric[j]] <- round(max(raw,
                                                         na.rm = TRUE),
                                                     digits = 2)
        }
        
      }
      
    }
    
    
    
    is_character <- which(attributes$columnClasses == "character") 
    is_catvar <- which(attributes$columnClasses == "categorical")
    is_date <- which(attributes$columnClasses == "date")
    use_i <- c(is_character, is_catvar)
    
    attributes$numberType[use_i] <- "character"
    
    attributes$columnClasses[is_catvar] <- "factor"
    
    attributes$columnClasses[is_date] <- "Date"
    
    # Set attribute definition (i.e. "definition")
    
    use_i <- c(is_character, is_catvar)
    
    if (length(use_i) > 0){
      attributes$definition[use_i] <- attributes$attributeDefinition[use_i]
    }
    
    attributes_stored[[i]] <- attributes
    
  }
  
  list("attributes" = attributes_stored)
  
}

