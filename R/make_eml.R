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
#'   recursive = TRUE)
#' 
#' # Set working directory
#' setwd(paste0(tempdir(), '/pkg_260'))
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
#'   package.id = 'edi.260.3')
#' 
#' # View EML directory contents
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
  ) {
  
  # Parameterize --------------------------------------------------------------
  
  # Get attributes of template files
  
  attr_tmp <- read_template_attributes()
  
  # Validate arguments --------------------------------------------------------
  
  # Validate path usage before passing arguments to validate_arguments().
  # When not using x, inputs are expected from path, data.path, and 
  # eml.path. When using x, only data.path is required unless write.file = TRUE
  # in which case eml.path is required.
  
  if (is.null(x) & missing(path)) {
    stop("Input argument 'path' is missing.")
  } else if (!is.null(x) & missing(path)) {
    path <- NULL
    if (missing(data.path)) {
      stop("Input argument 'data.path' is missing.")
    }
    if (isTRUE(write.file) & missing(eml.path)) {
      stop("Input argument 'write.file = TRUE' but 'eml.path' is missing.")
    } else if (!isTRUE(write.file) & missing(eml.path)) {
      eml.path <- NULL
    }
  }
  
  # Pass remaining arguments to validate_arguments()
  
  validate_arguments(
    fun.name = "make_eml",
    fun.args = as.list(environment()))
  
  # Handle deprecated arguments
  
  # FIXME: Remove May 2020
  if (!missing(affiliation)) {
    warning(
      "Argument 'affiliation' is deprecated; please use 'user.domain' instead.",
      call. = F)
    user.domain <- affiliation
  }
  # FIXME: Remove May 2020
  if (!missing(data.files)){
    warning(
      "Argument 'data.files' is deprecated; please use 'data.table' instead.",
      call. = F)
    data.table <- data.files
  }
  # FIXME: Remove May 2020
  if (!missing(data.files.description)){
    warning(
      "Argument 'data.files.description' is deprecated; please use 'data.table.description' instead.",
      call. = F)
    data.table.description <- data.files.description
  }
  # FIXME: Remove May 2020
  if (!missing(data.files.quote.character)){
    warning(
      "Argument 'data.files.quote.character' is deprecated; please use 'data.table.quote.character' instead.",
      call. = F)
    data.table.quote.character <- data.files.quote.character
  }
  # FIXME: Remove May 2020
  if (!missing(data.files.url)){
    warning(
      "Argument 'data.files.url' is deprecated; please use 'data.url' instead.",
      call. = F)
    data.url <- data.files.url
  }
  # FIXME: Do not remove until March 2021
  if (!missing(data.url)){
    warning(
      paste0("Argument 'data.url' is deprecated; please use 'data.table.url' ",
             "and 'other.entity.url' instead."),
      call. = F)
  }
  # FIXME: Remove May 2020
  if (!missing(zip.dir)){
    warning(
      "Argument 'zip.dir' is deprecated; please use 'other.entity' instead.",
      call. = F)
    other.entity <- zip.dir
  }
  # FIXME: Remove May 2020
  if (!missing(zip.dir.description)){
    warning(
      "Argument 'zip.dir.description' is deprecated; please use 'other.entity.description' instead.",
      call. = F)
    other.entity.description <- zip.dir.description
  }
  
  # Read templates and data ---------------------------------------------------
  # The reading function (template_arguments()) ignores empty templates located
  # at path, thereby simplifying logic required to populate EML nodes below.
  
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
  
  # Clean templates of extraneous NA values -----------------------------------
  # Users often add NAs to templates where EMLassemblyline expects "". This 
  # removes NAs from where they shouldn't be and replaces them with "". NOTE:
  # Any value listed in the missingValueCode field is interpreted "as is" when
  # attributes.txt templates are input as files. In contrast, when inputs are 
  # supplied by the argument "x" and when is.na() returns TRUE for values in
  # the missingValueCode field, these NAs are converted to "".
  
  for (k in names(x$template)) {
    if (is.data.frame(x$template[[k]]$content)) {
      for (m in names(x$template[[k]]$content)) {
        if (m == "missingValueCode") {
          if (!exists("data_read_2_x")) {
            x$template[[k]]$content[[m]][
              is.na(x$template[[k]]$content[[m]])] <- ""
          }
        } else {
          x$template[[k]]$content[[m]][
            is.na(x$template[[k]]$content[[m]])] <- ""
        }
      }
    }
  }

  # Validate templates --------------------------------------------------------
  
  x <- remove_empty_templates(x)
  validate_templates("make_eml", x)

  # Modify templates ----------------------------------------------------------
  # Modification of some template content helps with downstream processes.
  
  # catvars.txt:
  # - Remove incomplete cases
  # - Remove white space
  # FIXME: Ignore blank (i.e. "") categorical codes (implement this in 
  # the metadata quality check functions to be developed? See GitHub 
  # issue #46)
  
  use_i <- stringr::str_detect(
    names(x$template), 
    attr_tmp$regexpr[attr_tmp$template_name == "catvars"])
  if (any(use_i)) {
    for (i in which(use_i)) {
      use_i <- (x$template[[i]]$content$attributeName == "") |
        (x$template[[i]]$content$code == "") |
        (x$template[[i]]$content$definition == "")
      x$template[[i]]$content <- x$template[[i]]$content[!use_i, ]
      x$template[[i]]$content <- as.data.frame(
        lapply(
          x$template[[i]]$content,
          trimws), 
        stringsAsFactors = F)
    }
  }
  
  # custom_units.txt:
  # - Remove white space
  
  if (!is.null(x$template$custom_units.txt)) {
    x$template$custom_units.txt$content <- as.data.frame(
      lapply(
        x$template$custom_units.txt$content,
        trimws), 
      stringsAsFactors = F)
  }
  
  # keywords.txt: 
  # - Remove blank keywords to reduce errors when matching to controlled 
  # vocabularies
  
  x$template$keywords.txt$content <- x$template$keywords.txt$content[
    x$template$keywords.txt$content$keyword != "", ]
  
  # personnel.txt:
  # - Make all roles lowercase for string matching and remove mistankenly 
  # entered white spaces
  
  x$template$personnel.txt$content$role <- tolower(
    x$template$personnel.txt$content$role)
  x$template$personnel.txt$content <- as.data.frame(
    lapply(
      x$template$personnel.txt$content,
      trimws), 
    stringsAsFactors = F)
  
  # personnel.txt: 
  # - Set default project title and funding elements when none are provided 
  # - Combine projectTitle and fundingNumber into new "funding" field
  
  x$template$personnel.txt$content$projectTitle[
    x$template$personnel.txt$content$projectTitle == ""] <- 
    "No project title to report"
  x$template$personnel.txt$content$funding <- trimws(
    paste(
      x$template$personnel.txt$content$fundingAgency,
      x$template$personnel.txt$content$fundingNumber))
  x$template$personnel.txt$content$funding[
    x$template$personnel.txt$content$funding == ""] <- "No funding to report"
  
  # table_attributes.txt:
  # - Convert contents in the class field to a consistent case
  # - Set units in non-numeric classes to ""
  # - Ignore dateTimeFormatString when not classified as date
  # - Remove white space
  
  use_i <- stringr::str_detect(
    names(x$template), 
    attr_tmp$regexpr[attr_tmp$template_name == "attributes"])
  if (any(use_i)) {
    for (i in which(use_i)) {
      x$template[[i]]$content$class <- tolower(x$template[[i]]$content$class)
      x$template[[i]]$content$unit[
        x$template[[i]]$content$class != "numeric"] <- ""
      x$template[[i]]$content$dateTimeFormatString[
        x$template[[i]]$content$class != "date"] <- ""
      x$template[[i]]$content <- as.data.frame(
        lapply(
          x$template[[i]]$content,
          trimws), 
        stringsAsFactors = F)
    }
  }
  
  # Load helper funcitions ----------------------------------------------------
  
  # A function to set personnel roles: contact, creator, Principal 
  # investigator, associated party (other)
  
  set_person <- function(info_row, person_role) {
    
    if (person_role == "contact") {
      
      # If the contact only has givenName then the contact is an organization, 
      # otherwise it is a person.
      if ((x$template$personnel.txt$content[info_row, "givenName"] != "") & 
          (x$template$personnel.txt$content[info_row, "middleInitial"] == "") & 
          (x$template$personnel.txt$content[info_row, "surName"] == "")) {
        contact <- list(
          organizationName = x$template$personnel.txt$content[info_row,"organizationName"],
          positionName = stringr::str_to_title(x$template$personnel.txt$content[info_row,"givenName"]),
          electronicMailAddress = x$template$personnel.txt$content[info_row,"electronicMailAddress"])
        if (nchar(x$template$personnel.txt$content[info_row,"userId"]) == 19) {
          contact$userId <- list(
            directory = "https://orcid.org",
            paste0(
              "https://orcid.org/", 
              x$template$personnel.txt$content[info_row,"userId"]))
        }
      } else {
        contact <- list(
          individualName = list(
            givenName = list(
              x$template$personnel.txt$content[info_row,"givenName"],
              x$template$personnel.txt$content[info_row,"middleInitial"]),
            surName = x$template$personnel.txt$content[info_row,"surName"]),
          organizationName = x$template$personnel.txt$content[info_row,"organizationName"],
          electronicMailAddress = x$template$personnel.txt$content[info_row,"electronicMailAddress"])
        if (nchar(x$template$personnel.txt$content[info_row,"userId"]) == 19){
          contact$userId <- list(
            directory = 'https://orcid.org',
            paste0(
              "https://orcid.org/", 
              x$template$personnel.txt$content[info_row,"userId"]))
        }
        # FIXME Blank entries ('') result in closing tags when EML is written
        # to file. Need function to set all elements of value = '' to NULL.
        contact <- rapply(
          contact,
          function(x){
            if (x == ""){
              x <- NULL
            } else {
              x
            }
          },
          how = c("replace"))
      }
      contact
      
    } else if (person_role == "creator") {
      
      creator <- list(
        individualName = list(
          givenName = list(
            x$template$personnel.txt$content[info_row,"givenName"],
            x$template$personnel.txt$content[info_row,"middleInitial"]),
          surName = x$template$personnel.txt$content[info_row,"surName"]),
        organizationName = x$template$personnel.txt$content[info_row,"organizationName"],
        electronicMailAddress = x$template$personnel.txt$content[info_row,"electronicMailAddress"])
      if (nchar(x$template$personnel.txt$content[info_row,"userId"]) == 19){
        creator$userId <- list(
          directory = 'https://orcid.org',
          paste0(
            "https://orcid.org/", 
            x$template$personnel.txt$content[info_row,"userId"]))
      }
      # FIXME Blank entries ('') result in closing tags when EML is written
      # to file. Need function to set all elements of value = '' to NULL.
      creator <- rapply(
        creator,
        function(x){
          if (x == ""){
            x <- NULL
          } else {
            x
          }
        },
        how = c("replace"))
      creator
      
    } else if (person_role == "pi") {
      
      rp_personnel <- list(
        individualName = list(
          givenName = list(
            x$template$personnel.txt$content[info_row,"givenName"],
            x$template$personnel.txt$content[info_row,"middleInitial"]),
          surName = x$template$personnel.txt$content[info_row,"surName"]),
        organizationName = x$template$personnel.txt$content[info_row,"organizationName"],
        electronicMailAddress = x$template$personnel.txt$content[info_row,"electronicMailAddress"],
        role = 'Principal Investigator')
      if (nchar(x$template$personnel.txt$content[info_row,"userId"]) == 19){
        rp_personnel$userId <- list(
          directory = 'https://orcid.org',
          paste0(
            "https://orcid.org/", 
            x$template$personnel.txt$content[info_row,"userId"]))
      }
      # FIXME Blank entries ('') result in closing tags when EML is written
      # to file. Need function to set all elements of value = '' to NULL.
      rp_personnel <- rapply(
        rp_personnel,
        function(x){
          if (x == ""){
            x <- NULL
          } else {
            x
          }
        },
        how = c("replace"))
      rp_personnel
      
    } else {
      
      # If givenName, middleName, and surName are blank then the 
      # associatedParty is an organization, otherwise the associatedParty 
      # is a person
      if ((x$template$personnel.txt$content[info_row, "givenName"] == "") & 
          (x$template$personnel.txt$content[info_row, "middleInitial"] == "") & 
          (x$template$personnel.txt$content[info_row, "surName"] == "")) {
        associated_party = list(
          organizationName = x$template$personnel.txt$content[info_row,"organizationName"],
          electronicMailAddress = x$template$personnel.txt$content[info_row,"electronicMailAddress"],
          role = stringr::str_to_title(x$template$personnel.txt$content[info_row,"role"]))
        if (nchar(x$template$personnel.txt$content[info_row,"userId"]) == 19){
          associated_party$userId <- list(
            directory = 'https://orcid.org',
            paste0(
              "https://orcid.org/", 
              x$template$personnel.txt$content[info_row,"userId"]))
        }
        associated_party
      } else {
        associated_party <- list(
          individualName = list(
            givenName = list(
              x$template$personnel.txt$content[info_row,"givenName"],
              x$template$personnel.txt$content[info_row,"middleInitial"]
            ),
            surName = x$template$personnel.txt$content[info_row,"surName"]
          ),
          organizationName = x$template$personnel.txt$content[info_row,"organizationName"],
          electronicMailAddress = x$template$personnel.txt$content[info_row,"electronicMailAddress"],
          role = stringr::str_to_title(x$template$personnel.txt$content[info_row,"role"])
        )
        
        if (nchar(x$template$personnel.txt$content[info_row,"userId"]) == 19){
          associated_party$userId <- list(
            directory = 'https://orcid.org',
            paste0(
              "https://orcid.org/", 
              x$template$personnel.txt$content[info_row,"userId"]))
        }
        # FIXME Blank entries ('') result in closing tags when EML is written
        # to file. Need function to set all elements of value = '' to NULL.
        associated_party <- rapply(
          associated_party,
          function(x){
            if (x == ""){
              x <- NULL
            } else {
              x
            }
          },
          how = c("replace"))
        associated_party
      }
      
    }
    
  }
  
  # Create <eml> --------------------------------------------------------------
  
  # FIXME: Support other system values
  
  message("Making EML ...")
  message("<eml>")
  
  if (is.null(package.id)){
    package.id <- 'edi.101.1'
  }
  
  eml <- list(
    schemaLocation = "eml://ecoinformatics.org/eml-2.2.0  https://nis.lternet.edu/schemas/EML/eml-2.2.0/xsd/eml.xsd",
    packageId = package.id,
    system = "edi")
  
  # Create <access> -----------------------------------------------------------
  
  # FIXME: Support other user.id, user.domain, authSystem,
  
  message("  <access>")
  
  # Set default user.id and user.domain
  
  if (is.null(user.id)) {
    warning(
      "No 'user.id' was supplied. The default 'someuserid' will be used.",
      call. = F)
    user.id <- "someuserid"
  } else if (is.null(user.domain)) {
    warning(
      "No 'user.domain' was supplied. The default 'user.domain' will be used.",
      call. = F)
    user.domain <- "someuserdomain"
  }
  
  # Initialize the <access> node
  
  eml$access <- list(
    scope = "document",
    order = "allowFirst",
    authSystem = "https://pasta.edirepository.org/authentication",
    allow = list())
  
  # Set permissions to "all" for the EML creator/manager and "read" for 
  # everyone else
  
  r <- lapply(
    seq_along(user.id),
    function(k) {
      if (user.domain[k] == "LTER") {
        principal <- paste0(
          "uid=", user.id[k], ",o=", user.domain[k], ",dc=ecoinformatics,dc=org")
      } else if (user.domain[k] == "EDI") {
        principal <- paste0(
          "uid=", user.id[k], ",o=", user.domain[k], ",dc=edirepository,dc=org")
      } else {
        principal <- user.id
      }
      list(principal = principal, permission = "all")
    }
  )
  r[[length(r)+1]] <- list(principal = "public", permission = "read")
  eml$access$allow <- r

  # Create <dataset> ----------------------------------------------------------
  # Initialize the dataset list to which sub-nodes will be added
  
  message("  <dataset>")
  dataset <- list()
  
  # Create <title> ------------------------------------------------------------
  
  message("    <title>")
  eml$dataset$title <- dataset.title
  
  # Create <creator> ----------------------------------------------------------

  eml$dataset$creator <- lapply(
    which(x$template$personnel.txt$content$role == "creator"),
    function(k) {
      message("    <creator>")
      set_person(info_row = k, person_role = "creator")
    })

  # Create <associatedParty> --------------------------------------------------
  
  eml$dataset$associatedParty <- lapply(
    which(
      stringr::str_detect(
        x$template$personnel.txt$content$role,
        "[^pi|^creator|^contact]")),
    function(k) {
      message("    <associatedParty>")
      set_person(info_row = k, person_role = "")
    })
  
  # Create <pubDate> ----------------------------------------------------------
  
  message("    <pubDate>")
  eml$dataset$pubDate <- format(Sys.time(), "%Y-%m-%d")

  # Create <abstract> ---------------------------------------------------------
  
  if (any(stringr::str_detect(names(x$template), 'abstract'))) {
    message("    <abstract>")
    eml$dataset$abstract <- x$template[[
      names(x$template)[stringr::str_detect(names(x$template), 'abstract')]
      ]]$content
  }
  
  # Create <keywordSet> -------------------------------------------------------
  # Try resolving keywords without a listed thesaurus to the LTER Controlled 
  # Vocabulary, then create a separate keywordSet for each thesaurus + keyword
  # group.
  
  if (!is.null(x$template$keywords.txt)) {
    
    use_i <- x$template$keywords.txt$content$keywordThesaurus == ""
    if (any(use_i)) {
      r <- try(
        EDIutils::vocab_resolve_terms(
          x = x$template$keywords.txt$content$keyword[use_i],
          cv = "lter"),
        silent = T)
      if (is.data.frame(r)) {
        x$template$keywords.txt$content[
          match(
            r[r$controlled_vocabulary != "", ]$term, 
            x$template$keywords.txt$content$keyword), ] <- 
          r[r$controlled_vocabulary != "", ]
      }
    }
    
    eml$dataset$keywordSet <- lapply(
      unique(x$template$keywords.txt$content$keywordThesaurus),
      function(k) {
        message("    <keywordSet>")
        if (k == "") {
          list(
            keyword = as.list(
              x$template$keywords.txt$content$keyword[
                x$template$keywords.txt$content$keywordThesaurus == k]))
        } else {
          list(
            keyword = as.list(
              x$template$keywords.txt$content$keyword[
                x$template$keywords.txt$content$keywordThesaurus == k]),
            keywordThesaurus = unique(
              x$template$keywords.txt$content$keywordThesaurus[
                x$template$keywords.txt$content$keywordThesaurus == k]))
        }
      })
    
  }

  # Create <additionalInfo> ---------------------------------------------------
  
  if (any(stringr::str_detect(names(x$template), "additional_info"))) {
    message("   <additionalInfo>")
    eml$dataset$additionalInfo <- x$template[[
      names(x$template)[
        stringr::str_detect(names(x$template), "additional_info")]]]$content
  }

  # Create <intellectualRights> -----------------------------------------------

  if (!is.null(stringr::str_detect(names(x$template), "intellectual_rights"))) {
    message("    <intellectualRights>")
    eml$dataset$intellectualRights <- x$template$intellectual_rights.txt$content
  }

  # Create <coverage> ---------------------------------------------------------
  
  message('    <coverage>')
  
  eml$dataset$coverage <- list()
  
  # Create <geographicCoverage> -----------------------------------------------
  
  # Check for multiple geographic coverage inputs.
  # FIXME: On May 1, 2020 remove support for bounding_boxes.txt
  
  if (missing(geographic.coordinates) & 
      !any(stringr::str_detect(
        names(x$template), 
        "geographic_coverage.txt|bounding_boxes.txt"))) {
    stop(
      paste0("No geographic coverage found. Please add using the ",
        "'geographic.coordinates' and 'geographic.description' arguments to ",
        "make_eml(), or the bounding_boxes.txt or ",
        "geographic_coverage.txt templates."), 
      call. = F)
  }
  
  # Combine multiple sources of geographic coverage and remove duplicate entries
  
  o <- unique.data.frame(
    rbind(
      data.frame(
        geographicDescription = character(0),
        northBoundingCoordinate = character(0),
        southBoundingCoordinate = character(0),
        eastBoundingCoordinate = character(0),
        westBoundingCoordinate = character(0),
        stringsAsFactors = F),
      if (!missing(geographic.description) & !missing(geographic.coordinates)) {
        data.frame(
          geographicDescription = as.character(geographic.description),
          northBoundingCoordinate = as.character(geographic.coordinates[1]),
          southBoundingCoordinate = as.character(geographic.coordinates[3]),
          eastBoundingCoordinate = as.character(geographic.coordinates[2]),
          westBoundingCoordinate = as.character(geographic.coordinates[4]),
          stringsAsFactors = F)
      },
      data.frame(
        geographicDescription = x$template$bounding_boxes.txt$content$geographicDescription,
        northBoundingCoordinate = x$template$bounding_boxes.txt$content$northBoundingCoordinate,
        southBoundingCoordinate = x$template$bounding_boxes.txt$content$southBoundingCoordinate,
        eastBoundingCoordinate = x$template$bounding_boxes.txt$content$eastBoundingCoordinate,
        westBoundingCoordinate = x$template$bounding_boxes.txt$content$westBoundingCoordinate,
        stringsAsFactors = F),
      data.frame(
        geographicDescription = x$template$geographic_coverage.txt$content$geographicDescription,
        northBoundingCoordinate = x$template$geographic_coverage.txt$content$northBoundingCoordinate,
        southBoundingCoordinate = x$template$geographic_coverage.txt$content$southBoundingCoordinate,
        eastBoundingCoordinate = x$template$geographic_coverage.txt$content$eastBoundingCoordinate,
        westBoundingCoordinate = x$template$geographic_coverage.txt$content$westBoundingCoordinate,
        stringsAsFactors = F)))
  
  # Create the geographicCoverage node
  
  eml$dataset$coverage$geographicCoverage <- lapply(
    seq_len(nrow(o)),
    function(k) {
      message('        <geographicCoverage>')
      list(
        geographicDescription = o$geographicDescription[k],
        boundingCoordinates = list(
          westBoundingCoordinate = o$westBoundingCoordinate[k],
          eastBoundingCoordinate = o$eastBoundingCoordinate[k],
          northBoundingCoordinate = o$northBoundingCoordinate[k],
          southBoundingCoordinate = o$southBoundingCoordinate[k]))
    }
  )

  # Create <temporalCoverage> -------------------------------------------------
  
  message("      <temporalCoverage>")
  eml$dataset$coverage$temporalCoverage <- list(
    rangeOfDates = list(
      beginDate = list(calendarDate = temporal.coverage[1]),
      endDate = list(calendarDate = temporal.coverage[2])))
  
  # Create <taxonomicCoverage> ------------------------------------------------
  # Two sources of taxonomic coverage are supported: 
  #
  # 1.) The taxonomicCoverage EML node as an .xml file, which is read and 
  # inserted into the emld list object that make_eml() creates.
  #
  # 2.) The taxonomic_coverage.txt template listing taxa and authorities. 
  # Attempts are made to get the full hierarchy of taxonomic rank values for 
  # each taxa and render to EML.
  # FIXME: Ensure this second option includes unresolvable taxa within the EML.
  # FIXME: Create methods for adding taxonomic authorities. Only ITIS is 
  # currently supported.
  # FIXME: Allow taxonomic hierarchies to be supplied as a table (i.e. align
  # taxonomic_coverage.txt with the taxonomicCoverage option of 
  # EML::set_coverage()).

  if (!is.null(x$template$taxonomicCoverage.xml)) {
    message("      <taxonomicCoverage>")
    eml$dataset$coverage$taxonomicCoverage <- 
      x$template$taxonomicCoverage.xml$content
  } else if (!is.null(x$template$taxonomic_coverage.txt)) {
    message("      <taxonomicCoverage>")
    tc <- try(
      suppressMessages(
        taxonomyCleanr::make_taxonomicCoverage(
          taxa.clean = x$template$taxonomic_coverage.txt$content$name_resolved,
          authority = x$template$taxonomic_coverage.txt$content$authority_system,
          authority.id = x$template$taxonomic_coverage.txt$content$authority_id,
          write.file = F)),
      silent = T)
    if (class(tc) != "try-error") {
      eml$dataset$coverage$taxonomicCoverage <- tc
    }
  }

  # Create <maintenance> ------------------------------------------------------
  
  message("    <maintenance>")
  eml$dataset$maintenance$description <- maintenance.description

  # Create <contact> ----------------------------------------------------------

  eml$dataset$contact <- lapply(
    which(x$template$personnel.txt$content$role == "contact"),
    function(k) {
      message("    <contact>")
      set_person(info_row = k, person_role = "contact")
    })

  # Create <methods> ----------------------------------------------------------
  
  if (any(stringr::str_detect(names(x$template), "methods"))) {
    message("    <methods>")
    eml$dataset$methods$methodStep <- list(
      x$template[[
        names(x$template)[stringr::str_detect(names(x$template), "methods")]
        ]]$content$methodStep)
  }

  # Create <methodStep> (provenance) ------------------------------------------
  # Get provenance metadata for a data package in the EDI data repository.
  # FIXME: Support inputs from the provenance.txt metadata template
  # FIXME: Support provenance metadata models used by other EML based 
  # repositories
  
  if (!is.null(provenance)) {
    o <- lapply(
      provenance,
      function(k) {
        message("      <methodStep> (provenance metadata)")
        r <- httr::GET(
          paste0(
            EDIutils::url_env("production"), 
            ".lternet.edu/package/provenance/eml/", 
            stringr::str_replace_all(k, '\\.', '/')))
        if (r$status_code == 200) {
          prov <- httr::content(r, encoding = 'UTF-8')
          # Remove IDs from creator and contact to preempt ID + reference 
          # errors
          xml2::xml_set_attr(
            xml2::xml_find_all(prov, './/dataSource/creator'),
            'id', NULL)
          xml2::xml_set_attr(
            xml2::xml_find_all(prov, './/dataSource/contact'),
            'id', NULL)
          # Write .xml to tempdir() and read back in as an emld list object
          # to be added to the dataset emld list under construction here
          xml2::write_xml(prov, paste0(tempdir(), "/provenance_metadata.xml"))
          prov <- EML::read_eml(paste0(tempdir(), "/provenance_metadata.xml"))
          prov$`@context` <- NULL
          prov$`@type` <- NULL
          eml$dataset$methods$methodStep[[
            length(eml$dataset$methods$methodStep)+1]] <<- prov
          suppressMessages(file.remove(paste0(tempdir(), "/provenance_metadata.xml")))
        } else {
          message("Unable to get provenance metadata.")
        }
      })
  }
  
  # Create <project> ----------------------------------------------------------
  # The project metadata corresponding to the first "pi" listed in 
  # personnel.txt will become the primary project. Project metadata listed 
  # under supbsequent "pi" will become related projects in the order they are 
  # listed.
  
  use_i <- x$template$personnel.txt$content$role == "pi"
  if (any(use_i)){
    lapply(
      which(use_i),
      function(k) {
        if (k == min(which(use_i))) {
          message("    <project>")
          eml$dataset$project <<- list(
            title = x$template$personnel.txt$content$projectTitle[k],
            personnel = set_person(info_row = k, person_role = "pi"),
            funding = x$template$personnel.txt$content$funding[k])
        } else {
          message("      <relatedProject>")
          eml$dataset$project$relatedProject[[
            length(eml$dataset$project$relatedProject)+1]] <<- list(
              title = x$template$personnel.txt$content$projectTitle[k],
              personnel = set_person(info_row = k, person_role = "pi"),
              funding = x$template$personnel.txt$content$funding[k])
        }
      })
  }
  
  # Create <dataTable> --------------------------------------------------------
  
  if (!is.null(x$data.table)) {
    
    eml$dataset$dataTable <- lapply(
      names(x$data.table),
      function(k) {
        message(paste0("    <dataTable> (", k, ")"))
        
        # Get corresponding table_attributes.txt
        
        tbl_attr <- x$template[[
          paste0("attributes_", tools::file_path_sans_ext(k), ".txt")]]$content
        
        # Add attributes.txt contents to the data frame input expected by 
        # EML::set_attributes().
        
        attributes <- data.frame(
          attributeName = tbl_attr$attributeName,
          formatString = tbl_attr$dateTimeFormatString,
          unit = tbl_attr$unit,
          numberType = "",
          definition = "",
          attributeDefinition = tbl_attr$attributeDefinition,
          columnClasses = tbl_attr$class,
          minimum = NA,
          maximum = NA,
          missingValueCode = tbl_attr$missingValueCode,
          missingValueCodeExplanation = tbl_attr$missingValueCodeExplanation,
          stringsAsFactors = F)
        
        # Update missingValueCode - NA must be "NA" otherwise it will not be 
        # listed in the EML
        
        attributes$missingValueCode[is.na(attributes$missingValueCode)] <- "NA"
        
        # Update non-numeric attributes - categorical and character classes must 
        # have a numberType of "character" and their attributeDefinition must be 
        # listed under "defintion. date and categorical classes must be listed as 
        # "Date" and "factor" respectively
        
        use_i <- (attributes$columnClasses == "categorical") | 
          (attributes$columnClasses == "character")
        attributes$numberType[use_i] <- "character"
        attributes$definition[use_i] <- attributes$attributeDefinition[use_i]
        attributes$columnClasses[
          attributes$columnClasses == "date"] <- "Date"
        attributes$columnClasses[
          attributes$columnClasses == "categorical"] <- "factor"
        
        # Update numeric attributes - Remove NA and missingValueCode for 
        # calculations. Get minimum and maximum values. Infer numberType
        
        for (i in which(tbl_attr$class == "numeric")) {
          a <- x$data.table[[k]]$content[[tbl_attr$attributeName[i]]][
            !is.na(x$data.table[[k]]$content[[tbl_attr$attributeName[i]]])]
          a <- a[a != tbl_attr$missingValueCode[i]]
          if (all(is.na(a))) {
            attributes$minimum[i] <- NA
            attributes$maximum[i] <- NA
          } else {
            attributes$minimum[i] <- as.numeric(min(a, na.rm = T))
            attributes$maximum[i] <- as.numeric(max(a, na.rm = T))
          }
          is_integer <-function(x, tol = .Machine$double.eps^0.5) {
            abs(x - round(x)) < tol
          }
          if (any(!is_integer(a))) {
            attributes$numberType[i] <- "real"
          } else if (!any(a < 1)) {
            attributes$numberType[i] <- "natural"
          } else if (any(a < 0)) {
            attributes$numberType[i] <- "integer"
          } else {
            attributes$numberType[i] <- "whole"
          }
          # FIXME: Calculate precision for numeric attributes. Alternatively
          # this should be added to the attributes template so precision can
          # be manually defined by the metadata creator.
        }
        
        # Create attributeList
        
        if (paste0("catvars_", tools::file_path_sans_ext(k), ".txt") %in% 
            names(x$template)) {
          attributeList <- suppressWarnings(
            EML::set_attributes(
              attributes[ , c(
                "attributeName", 
                "formatString",
                "unit",
                "numberType",
                "definition",
                "attributeDefinition",
                "minimum",
                "maximum",
                "missingValueCode",
                "missingValueCodeExplanation")],
              factors = x$template[[
                paste0(
                  "catvars_", 
                  tools::file_path_sans_ext(k), 
                  ".txt")]]$content,
              col_classes = attributes$columnClasses))
        } else {
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
              col_classes = attributes$columnClasses))
        }
        
        # Set physical
        # FIXME: Auto-detect numHeaderLines
        # FIXME: Move EDIutils::get_eol() to EMLassemblyline?
        
        physical <- suppressMessages(
          EML::set_physical(
            paste0(data.path, "/", k),
            numHeaderLines = "1",
            recordDelimiter = EDIutils::get_eol(
              path = data.path,
              file.name = k,
              os = EDIutils::detect_os()),
            attributeOrientation = "column",
            url = "placeholder"))
        
        if (!is.null(data.table.quote.character)) {
          physical$dataFormat$textFormat$simpleDelimited$quoteCharacter <- 
            data.table.quote.character[which(k == names(x$data.table))]
        }
        
        # FIXME: data.url is deprecated. Remove support for this argument after (11 March 2021)
        if (!is.null(data.url)) {
          physical$distribution$online$url[[1]] <- paste0(data.url, "/", k)
        } else if (!is.null(data.table.url)) {
          physical$distribution$online$url <- data.table.url[
            which(k == names(x$data.table))]
        } else {
          physical$distribution <- list()
        }
        
        physical$dataFormat$textFormat$simpleDelimited$fieldDelimiter <- 
          EDIutils::detect_delimeter(
            path = data.path,
            data.files = k,
            os = EDIutils::detect_os())
        
        # Create dataTable
        
        data_table <- list(
          entityName = data.table.name[which(k == names(x$data.table))],
          entityDescription = data.table.description[which(k == names(x$data.table))],
          physical = physical,
          attributeList = attributeList,
          numberOfRecords = as.character(nrow(x$data.table[[k]]$content)))
        
        # FIXME: EML v2.0.0 handles absense of missingValue codes differently
        # than EML v 1.0.3. This fixes the issue here, though it may be better
        # to implement the fix in EML v2.0.0.
        for (j in seq_along(data_table$attributeList$attribute)){
          if (data_table$attributeList$attribute[[j]]$missingValueCode$code == ''){
            data_table$attributeList$attribute[[j]]$missingValueCode$code <- NA_character_
            data_table$attributeList$attribute[[j]]$missingValueCode$codeExplanation <- NA_character_
          }
        }
        
        data_table

      }
    )
  }
  
  # Create <otherEntity> ------------------------------------------------------
  # FIXME: Need methods for inferring entity type, format, and other metadata
  
  if (!is.null(x$other.entity)) {
    eml$dataset$otherEntity <- lapply(
      names(x$other.entity),
      function(k) {
        message(paste0("    <otherEntity> (", k, ")"))
        # Set physical
        physical <- suppressMessages(
          EML::set_physical(
            paste0(data.path, "/", k)))
        physical$dataFormat$textFormat <- NULL
        physical$dataFormat$externallyDefinedFormat$formatName <- "unknown"
        # FIXME: data.url is deprecated. Remove support for this argument after (11 March 2021)
        if (!is.null(data.url)) {
          physical$distribution$online$url[[1]] <- paste0(data.url, "/", k)
        } else if (!is.null(other.entity.url)) {
          physical$distribution$online$url <- other.entity.url[
            which(k == names(x$other.entity))]
        }else {
          physical$distribution <- list()
        }
        # Create otherEntity
        list(
          entityName = other.entity.name[
            which(k == names(x$other.entity))],
          entityDescription = other.entity.description[
            which(k == names(x$other.entity))],
          physical = physical,
          entityType = "unknown")
      })
  }
  
  # Create <additionalMetadata> -----------------------------------------------
  
  if (!is.null(x$template$custom_units.txt)) {
    message("  <additionalMetadata>")
    eml$additionalMetadata[[
      length(eml$dataset$additionalMetadata)+1]]$metadata$unitList <- EML::set_unitList(
        x$template$custom_units.txt$content)
  }
  
  # Write EML -----------------------------------------------------------------
  
  message("</eml>")
  if (isTRUE(write.file)) {
    message(paste0("Writing EML (", package.id, ".xml)"))
    emld::eml_version("eml-2.2.0")
    EML::write_eml(eml, paste0(eml.path, "/", package.id, ".xml"))
  }
  
  # Validate EML --------------------------------------------------------------
  
  message("Validating EML")
  r <- EML::eml_validate(eml)
  if (isTRUE(r)) {
    message("  Validation passed :)")
  } else {
    message("  Validation failed :(")
  }
  message("Done.")
  
  if (isTRUE(return.obj)) {
    eml
  }
  
}
