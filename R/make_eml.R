#' Make EML
#'
#' @description  
#'     Translate user supplied metadata templates into the EML, validate the 
#'     EML, and write to file.
#'     
#' @usage 
#'     make_eml(
#'     path,
#'     data.path = path,
#'     eml.path = path, 
#'     dataset.title, 
#'     data.files, 
#'     data.files.description, 
#'     data.files.quote.character, 
#'     zip.dir,
#'     zip.dir.description,
#'     data.files.url, 
#'     temporal.coverage, 
#'     geographic.description, 
#'     geographic.coordinates, 
#'     maintenance.description, 
#'     user.id, 
#'     affiliation,
#'     environment,
#'     package.id,
#'     provenance)
#'     
#' @param path 
#'     (character) A path to the directory containing the completed metadata 
#'     templates.
#' @param data.path
#'     (character) A path to the directory containing the data entities 
#'     described by the metadata templates.
#' @param eml.path
#'     (character) A path to the directory where the EML will be writtn.
#' @param dataset.title
#'     (character) The title for your data package. Be descriptive
#'     (more than 5 words). We recommend the following format: Project name: 
#'     Broad description: Time span (e.g. "GLEON: Long term lake chloride 
#'     concentrations from North America and Europe: 1940-2016").
#' @param data.files
#'     (character) A vector of character strings specifying the names of your 
#'     data files (e.g. data.files = c("lake_chloride_concentrations.csv", 
#'     "lake_characteristics.csv")).
#' @param data.files.description
#'     (character) A vector of character strings briefly describing the data 
#'     files listed in the data.files argument and in the same order as listed 
#'     in the data.files argument 
#'     (e.g. data.files.description = c("Chloride concentration data.", 
#'     "Climate, road density, and impervious surface data."))
#' @param data.files.quote.character
#'     (character) A vector of character strings defining the quote characters 
#'     used in your data files and in the same order as listed in the data.files 
#'     argument. This argument is required only if your data contain quotations.  
#'     If the quote character is a quotation, then enter "\\"". If the quote 
#'     character is an apostrophe, then enter "\\'". Example: 
#'     data.files.quote.character = c("\\"", "\\"").
#' @param data.files.url
#'     (character) The URL of where your data tables are 
#'     stored on a publicly accessible server (i.e. does not require user ID 
#'     or password). This argument is required only if your data are accessible 
#'     from a publicly accesible URL. The EDI data repository software, PASTA+, 
#'     will use this to upload your data into the repository. If you will be 
#'     manually uploading your data tables, then don't use this argument.
#'     Example: data.files.url = "https://lter.limnology.wisc.edu/sites/default/files/data/gleon_chloride".
#' @param zip.dir
#'     (character) A vector of character strings listing the .zip directories of your 
#'     dataset.
#' @param zip.dir.description
#'     (character) A vector of character strings briefly describing the contents of 
#'     any .zip directory present in the working directory.
#' @param temporal.coverage
#'     (character) A vector of character strings specifying the beginning and ending dates 
#'     of your dataset. Use the format YYYY-MM-DD.
#' @param geographic.coordinates
#'     (character) A vector of character strings specifying the spatial bounding
#'     coordinates of your dataset in decimal degrees. This argument is not 
#'     required if you are supplying bounding coordinates in the 
#'     bounding_boxes.txt template file. The list must follow
#'     this order: North, East, South, West. Longitudes West of the 
#'     prime meridian and latitudes South of the equator are prefixed with a
#'     minus sign (i.e. dash -). If you don't have an area, but rather a point,
#'     Repeat the latitude value for North and South, and repeat the longitude
#'     value for East and West (e.g. geographic.coordinates = c('28.38',
#'     '-119.95', '28.38', '-119.95)).
#' @param geographic.description
#'     (character) A description of the geographic coverage of your dataset. 
#'     Don't use this argument if you are supplying geographic.coordinates in 
#'     the bounding_boxes.txt template file. Example: "North America and Europe".
#' @param maintenance.description
#'     (character) A description of whether data collection for this dataset 
#'     is "ongoing" or "completed".
#' @param user.id
#'     (character) A vector of character strings, specifying your user 
#'     ID for the EDI data repository. The user.id controls editing access to 
#'     your data package. If you do not have one, contact EDI 
#'     (info@@environmentaldatainitiative.org) to obtain one. In the meantime 
#'     do not use this argument when running `make_eml`.
#' @param affiliation
#'     (character) A vector of character strings, specifying the 
#'     affiliation of your user ID. In a list, the associations must follow the 
#'     same order of the corresponding values listed under user.id. This is the 
#'     affiliation used when logging in to the EDI Data Portal and can be: 
#'     "LTER" or "EDI". If you don't have a user.id then do not use this 
#'     argument when running `make_eml`.
#' @param environment
#'     (character) Data repository environment to create the package in.
#'     Can be: 'development', 'staging', 'production'.
#' @param package.id
#'     (character) The ID of your data package. 
#'     A missing package ID defaults to \emph{edi.101.1}. A package ID must
#'     contain the scope, package number, and revision number 
#'     (e.g. 'edi.101.1').
#' @param provenance
#'     (character) A vector of EDI package.ids corresponding to parent data 
#'     packages from which this data package was created. A package ID must
#'     contain the scope, package number, and revision number 
#'     (e.g. 'knb-lter-cap.46.3').
#'     
#' @return 
#'     Validation results printed to the RStudio \emph{Console}.
#'     
#'     An EML file written to the dataset working directory titled 
#'     \emph{packageID.xml}.
#'     
#' @details 
#'     If validation fails, open the EML document in a .xml editor to identify 
#'     the source of error. Often the error is small and quickly resolved with 
#'     the aid of an editors schema validator.
#'
#' @export
#'


make_eml <- function(path, data.path = path, eml.path = path, dataset.title, data.files, data.files.description, 
                     data.files.quote.character, data.files.url, zip.dir, zip.dir.description,
                     temporal.coverage, geographic.coordinates, geographic.description, maintenance.description, user.id, 
                     affiliation, environment = 'production', package.id, provenance) {

  # Check arguments and parameterize ------------------------------------------
  
  if (missing(path)){
    stop('Input argument "path" is missing! Specify path to your dataset working directory.')
  }
  if (missing(dataset.title)){
    stop('Input argument "dataset.title" is missing! Add a title for your dataset.')
  }
  # if (missing(data.files)){
  #   stop('Input argument "data.files" is missing! List the names of your datasets data files.')
  # }
  if (!missing(data.files)){
    if (missing(data.files.description)){
      stop('Input argument "data.files.description" is missing! Please describe your data files.')
    }
  }
  if (missing(temporal.coverage)){
    stop('Input argument "temporal.coverage" is missing! Add the temporal coverage of your dataset.')
  }
  if (missing(geographic.coordinates) & !file.exists(paste0(path, '/', 'bounding_boxes.txt'))){
    stop('Input argument "geographic.coordinates" is missing and the "bounding_boxes.txt" template is missing! Add geographic bounding coordinates for your dataset.')
  }
  if (missing(maintenance.description)){
    stop('Input argument "maintenance.description" is missing! Indicate whether data collection is "ongoing" or "completed" for your dataset.')
  }
  if (!missing(user.id) & missing(affiliation)){
    stop('Input argument "affiliation" is missing! Add one.')
  }
  if (!missing(affiliation) & missing(user.id)){
    stop('Input argument "user.id" is missing! Add one.')
  }
  
  # Validate paths
  
  validate_path(path)
  if (!missing(data.path)){
    validate_path(data.path)  
  }
  if (!missing(eml.path)){
    validate_path(eml.path)  
  }
  
  # Validate data file names
  
  if (!missing(data.files)){
    table_names <- validate_file_names(path = data.path, data.files = data.files) 
  }
  
  # Validate fields of data.files
  
  if (!missing(data.files)){
    EDIutils::validate_fields(data.path, data.files = table_names)
  }
  
  # Detect operating system
  
  os <- detect_os()
  
  # Validate and set package ID
  
  if (!missing(package.id)){
    if (!isTRUE(stringr::str_detect(package.id, '[:alpha:]\\.[:digit:]+\\.[:digit:]'))){
      stop('Input argument "package.id" appears to be malformed. A package ID must consist of a scope, identifier, and revision (e.g. "edi.100.4").')
    }
    data_package_id <- package.id
  } else {
    data_package_id <- "edi.101.1"
  }
  
  # Validate data.files.description
  
  if (!missing(data.files)){
    if (length(data.files.description) != length(data.files)){
      stop('The number of descriptions listed in the argument "data.files.description" does not match the number of files listed in the argument "data.files". These must match.')
    }
  }
  
  # Validate data.files.quote.character
  
  if (!missing(data.files)){
    if (!missing(data.files.quote.character)){
      if (length(data.files.quote.character) != length(data.files)){
        stop('The number of quote characters listed in the argument "data.files.quote.character" does not match the number of files listed in the argument "data.files". These must match.')
      }
    }
  }
  
  # Validate temporal.coverage
  
  if (length(temporal.coverage) != 2){
    stop('The argument "temporal.coverage" requires both a begin date and end date. Please fix this.')
  }
  if (length(temporal.coverage) != 2){
    stop('The argument "temporal.coverage" requires both a begin date and end date. Please fix this.')
  }
  
  # Validate zip.dir and zip.dir.description
  
  if ((!missing(zip.dir)) & (missing(zip.dir.description))){
    stop('The argument "zip.dir.description" is missing and "zip.dir" is present. Add a description for your zip directory.')
  }
  if ((!missing(zip.dir.description)) & (missing(zip.dir))){
    stop('The argument "zip.dir" is missing and "zip.dir.description" is present. Add the zip directories you are describing.')
  }
  if ((!missing(zip.dir.description)) & (!missing(zip.dir))){
    if ((length(zip.dir)) != (length(zip.dir.description))){
      stop('The number of zip.dir and zip.dir.descriptions must match!')
    }
  }
  
  # Validate user.id and association
  if (!missing(user.id) & !missing(affiliation)){
    if (length(user.id) != length(affiliation)){
      stop('The number of values listed in arguments "user.id" and "affiliation" do not match. Each user.id must have a corresponding affiliation')
    }
    if (sum(sum(affiliation == 'LTER'), sum(affiliation == 'EDI')) != length(affiliation)){
      stop('Input argument "affiliation" is not "EDI" or "LTER"! Only "EDI" and "LTER" are acceptable values.')
    }
  }
  
  
  # Compile attributes --------------------------------------------------------
  
  if (!missing(data.files)){
    attributes_in <- compile_attributes(path, data.path, data.files)
  }
  
  # Set file names

  fname_abstract <- paste(path, "/abstract.txt", sep = "")
  
  fname_additional_info <- paste(path, "/additional_info.txt", sep = "")

  fname_keywords <- paste(path, "/keywords.txt", sep = "")
  
  fname_personnel <- paste(path, "/personnel.txt", sep = "")
  
  fname_intellectual_rights <- paste(path, "/intellectual_rights.txt", sep = "")

  fname_methods <- paste(path, "/methods.txt", sep = "")
  
  fname_custom_units <- paste(path, "/custom_units.txt", sep = "")
  
  files <- list.files(path)
  
  fname_table_catvars <- c()
  
  if (exists('table_names')){
    for (i in 1:length(table_names)){
      fname_table_catvars[i] <- paste("catvars_",
                                      substr(table_names[i], 1, nchar(table_names[i]) - 4),
                                      ".txt",
                                      sep = "")
    }
  }
  
  # Initialize data entity storage (tables)

  data_tables_stored <- list()

  # Load helper function to set personnel roles

  set_person <- function(info_row, person_role){

    if (person_role == "contact"){

      if ((personinfo[info_row, "givenName"] != "") & (personinfo[info_row, "middleInitial"] == "") & (personinfo[info_row, "surName"] == "")){
        
        contact <- new(
          "contact",
          organizationName = trimws(personinfo[info_row,"organizationName"]),
          positionName = str_to_title(trimws(personinfo[info_row,"givenName"])),
          electronicMailAddress = trimws(personinfo[info_row,"electronicMailAddress"]))
        
        if (nchar(trimws(personinfo[info_row,"userId"])) == 19){
          userId <- new("userId")
          userId@directory <- new("xml_attribute", "https://orcid.org")
          hold <- trimws(personinfo[info_row,"userId"])
          hold <- paste("https://orcid.org/", hold, sep = "")
          userId@.Data <- hold
          contact@userId <- new("ListOfuserId", c(userId))
        }
        
      } else {
        
        individualName <- new(
          "individualName",
          givenName = c(trimws(personinfo[info_row,"givenName"]),
                        trimws(personinfo[info_row,"middleInitial"])),
          surName = trimws(personinfo[info_row,"surName"]))
        
        contact <- new(
          "contact",
          individualName = individualName,
          organizationName = trimws(personinfo[info_row,"organizationName"]),
          electronicMailAddress = trimws(personinfo[info_row,"electronicMailAddress"]))
        
        if (nchar(trimws(personinfo[info_row,"userId"])) == 19){
          userId <- new("userId")
          userId@directory <- new("xml_attribute", "https://orcid.org")
          hold <- trimws(personinfo[info_row,"userId"])
          hold <- paste("https://orcid.org/", hold, sep = "")
          userId@.Data <- hold
          contact@userId <- new("ListOfuserId", c(userId))
        }
        
      }

      contact

    } else if (person_role == "creator"){

      individualName <- new(
        "individualName",
        givenName = c(trimws(personinfo[info_row,"givenName"]),
                      trimws(personinfo[info_row,"middleInitial"])),
        surName = trimws(personinfo[info_row,"surName"]))

      creator <- new(
        "creator",
        individualName = individualName,
        organizationName = trimws(personinfo[info_row,"organizationName"]),
        electronicMailAddress = trimws(personinfo[info_row,"electronicMailAddress"]))

      if (nchar(trimws(personinfo[info_row,"userId"])) == 19){
        userId <- new("userId")
        userId@directory <- new("xml_attribute", "https://orcid.org")
        hold <- trimws(personinfo[info_row,"userId"])
        hold <- paste("https://orcid.org/", hold, sep = "")
        userId@.Data <- hold
        creator@userId <- new("ListOfuserId", c(userId))
      }
      creator

    } else if (person_role == "pi"){

      individualName <- new(
        "individualName",
        givenName = c(trimws(personinfo[info_row,"givenName"]),
                      trimws(personinfo[info_row,"middleInitial"])),
        surName = trimws(personinfo[info_row,"surName"]))

      principal_investigator <- as.person(
        paste(
          trimws(personinfo[info_row, "givenName"]),
          " ",
          trimws(personinfo[info_row, "surName"]),
          " <",
          trimws(personinfo[info_row, "electronicMailAddress"]),
          ">",
          sep = ""))

      rp_personnel <- as(principal_investigator, "personnel")

      if (nchar(trimws(personinfo[info_row,"userId"])) == 19){
        rp_userId <- new("userId")
        rp_userId@directory <- new("xml_attribute", "https://orcid.org")
        hold <- trimws(personinfo[info_row,"userId"])
        hold <- paste("https://orcid.org/", hold, sep = "")
        rp_userId@.Data <- hold
        rp_personnel@userId <- new("ListOfuserId", c(rp_userId))
      }
      role <- new("role", "Principal Investigator")
      rp_personnel@role <- new("ListOfrole", c(role))
      rp_personnel
      
    } else {
      
      if ((personinfo[info_row, "givenName"] == "") & (personinfo[info_row, "middleInitial"] == "") & (personinfo[info_row, "surName"] == "")){
        
        associated_party <- new(
          "associatedParty",
          organizationName = trimws(personinfo[info_row,"organizationName"]),
          electronicMailAddress = trimws(personinfo[info_row,"electronicMailAddress"]))
        
        if (nchar(trimws(personinfo[info_row,"userId"])) == 19){
          userId <- new("userId")
          userId@directory <- new("xml_attribute", "https://orcid.org")
          hold <- trimws(personinfo[info_row,"userId"])
          hold <- paste("https://orcid.org/", hold, sep = "")
          userId@.Data <- hold
          associated_party@userId <- new("ListOfuserId", c(userId))
        }
        role <- new("role", str_to_title(trimws(personinfo[info_row,"role"])))
        associated_party@role <- new("role", c(role))
        associated_party
        
      } else {
        
        individualName <- new(
          "individualName",
          givenName = c(trimws(personinfo[info_row,"givenName"]),
                        trimws(personinfo[info_row,"middleInitial"])),
          surName = trimws(personinfo[info_row,"surName"]))
        
        associated_party <- new(
          "associatedParty",
          individualName = individualName,
          organizationName = trimws(personinfo[info_row,"organizationName"]),
          electronicMailAddress = trimws(personinfo[info_row,"electronicMailAddress"]))
        
        if (nchar(trimws(personinfo[info_row,"userId"])) == 19){
          userId <- new("userId")
          userId@directory <- new("xml_attribute", "https://orcid.org")
          hold <- trimws(personinfo[info_row,"userId"])
          hold <- paste("https://orcid.org/", hold, sep = "")
          userId@.Data <- hold
          associated_party@userId <- new("ListOfuserId", c(userId))
        }
        role <- new("role", str_to_title(trimws(personinfo[info_row,"role"])))
        associated_party@role <- new("role", c(role))
        associated_party
        
      }

    }

  }

  # Read personnel file

  message("Reading personnel file.")

  personinfo <- read.table(
    fname_personnel,
    header = T,
    sep="\t",
    quote="\"",
    as.is=TRUE,
    comment.char = "",
    fill = T,
    colClasses = rep("character", 10))
  personinfo <- personinfo[ ,1:10]
  colnames(personinfo) <- c("givenName",
                            "middleInitial",
                            "surName",
                            "organizationName",
                            "electronicMailAddress",
                            "userId",
                            "role",
                            "projectTitle",
                            "fundingAgency",
                            "fundingNumber")
  
  personinfo$role <- tolower(personinfo$role)
  
  # Validate personnel.txt ----------------------------------------------------
  
  personinfo <- validate_personnel(x = personinfo)
  
  
  # Build modules--------------------------------------------------------------

  message("Building EML ...")
  
  # Create EML
  
  # Build eml-access module ---------------------------------------------------
  
  message("<access>")
  
  if (!missing(user.id) & !missing(affiliation)){
    list_of_allow_principals <- list()
    list_of_allow_permissions <- list()
    for (i in 1:length(user.id)){
      if (affiliation[i] == 'LTER'){
        list_of_allow_principals[[i]] <- c(
          paste0(
            'uid=',
            user.id[i],
            ',o=',
            affiliation[i],
            ',dc=ecoinformatics,dc=org'
          ),
          "public"
        )
      } else if (affiliation[i] == 'EDI'){
        list_of_allow_principals[[i]] <- c(
          paste0(
            'uid=',
            user.id[i],
            ',o=',
            affiliation[i],
            ',dc=edirepository,dc=org'
          ),
          "public"
        )
      }
      list_of_allow_permissions[[i]] <- c(
        "all",
        "read")
    }
  } else {
    list_of_allow_principals <- list()
    list_of_allow_permissions <- list()
  }
  
  list_of_allow_principals[[(length(list_of_allow_principals)+1)]] <- c("public")
  list_of_allow_permissions[[(length(list_of_allow_permissions)+1)]] <- c("read")
  
  access_order <- "allowFirst"
  
  access_scope <- "document"
  
  access <- new("access",
                scope = access_scope,
                order = access_order,
                authSystem = "https://pasta.edirepository.org/authentication")
  
  allow <- list()
  for (i in 1:length(list_of_allow_principals)){
    allow[[i]] <- new("allow",
                      principal = list_of_allow_principals[[i]][1],
                      permission = list_of_allow_permissions[[i]][1])
  }
  
  access@allow <- new("ListOfallow",
                      c(allow))

  # Build dataset module
  
  message("<dataset>")

  dataset <- new("dataset",
                 title = dataset.title)

  # Add creators
  
  message("<creators>")

  useI <- which(personinfo$role == "creator")

  creator_list <- list()
  for (j in 1:length(useI)){
    creator_list[[j]] <- set_person(info_row = useI[j],
                                    person_role = "creator")
  }

  dataset@creator <- as(creator_list, "ListOfcreator")

  # Add publicaton date
  
  message("<publicationDate>")

  dataset@pubDate <- as(format(Sys.time(), "%Y-%m-%d"), "pubDate")

  # Add abstract
  
  message("<abstract>")
  
  if (!file.exists(fname_abstract)){
    stop("abstract.txt doesn't exist!")
  }

  dataset@abstract <- as(set_TextType(fname_abstract), "abstract")

  # Add keywords --------------------------------------------------------------
  
  message("<keywordSet>")
  
  if (!isTRUE(file.exists(fname_keywords))){
    stop('keywords.txt does not exist! Run import_templates.txt to regenerate this template.')
  }

  keywords <- suppressWarnings(read.table(
    paste(substr(fname_keywords, 1, nchar(fname_keywords) - 4),
          ".txt",
          sep = ""),
    header = T,
    sep="\t",
    quote="\"",
    as.is=TRUE,
    comment.char = "",
    fill = T,
    colClasses = rep("character", 2)))
   keywords <- keywords[ ,1:2]
  colnames(keywords) <- c("keyword", 
                          "keywordThesaurus")
  
  # Edit keywords: Remove blank keyword entries
  use_i <- keywords$keyword == ""
  keywords <- keywords[!use_i, c('keyword', 'keywordThesaurus')]
  
  # Try resolving keywords without a listed thesaurus to the LTER Controlled 
  # Vocabulary
  
  use_i <- keywords$keywordThesaurus == ''
  
  if (sum(use_i) > 0){
    
    unresolved_terms <- keywords$keyword[use_i]

    results <- try(
      resolve_terms(
        x = 'peat',
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
  
  list_keywordSet <- list()

  uni_keywordThesaurus <- unique(keywords$keywordThesaurus)
  for (i in 1:length(uni_keywordThesaurus)){
    keywordSet <- list()
    use_i <- uni_keywordThesaurus[i] == keywords[["keywordThesaurus"]]
    kws <- keywords$keyword[use_i]
    for (k in 1:length(kws)){
      keywordSet[[k]] <- as(kws[k], "keyword")
    }
    list_keywordSet[[i]] <- new("keywordSet",
                           keywordSet,
                           keywordThesaurus = uni_keywordThesaurus[i])
  }
  dataset@keywordSet <- as(list_keywordSet, "ListOfkeywordSet")

  # Add intellectual rights

  message("<intellectualRights>")
  
  if (!file.exists(fname_intellectual_rights)){
    stop("intellectual_rights.txt doesn't exist!")
  }
  
  dataset@intellectualRights <- as(
    set_TextType(fname_intellectual_rights),
    "intellectualRights")

  # Add coverage --------------------------------------------------------------
  
  message("<temporalCoverage>")
  
  dataset@coverage <- set_coverage(
    begin = temporal.coverage[1],
    end = temporal.coverage[2]
  )
  
  message("<geographicCoverage>")
  
  list_of_coverage <- list()
  
  # Add coverage defined in arguments of make_eml
  
  if (!missing(geographic.coordinates) & !missing(geographic.description)){
    geographic_description <- new("geographicDescription", geographic.description)
    bounding_coordinates <- new("boundingCoordinates",
                                westBoundingCoordinate = as.character(geographic.coordinates[4]),
                                eastBoundingCoordinate = as.character(geographic.coordinates[2]),
                                northBoundingCoordinate = as.character(geographic.coordinates[1]),
                                southBoundingCoordinate = as.character(geographic.coordinates[3]))
    geographic_coverage <- new("geographicCoverage",
                               geographicDescription = geographic_description,
                               boundingCoordinates = bounding_coordinates)
    list_of_coverage[[(length(list_of_coverage)+1)]] <- geographic_coverage
  }
  
  # Add coverage defined in bounding_boxes.txt
  
  if (file.exists(paste0(path, '/', 'bounding_boxes.txt'))){
    
    bounding_boxes <- suppressMessages(
      as.data.frame(
        utils::read.table(paste0(path, '/', 'bounding_boxes.txt'), header = T, sep = '\t', quote = "\"", as.is = T, comment.char = '')
      )
    )
    
    if (nrow(bounding_boxes) != 0){
      
      bounding_boxes <- lapply(bounding_boxes, as.character)
      
      for (i in 1:length(bounding_boxes$geographicDescription)){
        
        geographic_description <- new("geographicDescription", bounding_boxes$geographicDescription[i])
        bounding_coordinates <- new("boundingCoordinates",
                                    westBoundingCoordinate = bounding_boxes$westBoundingCoordinate[i],
                                    eastBoundingCoordinate = bounding_boxes$eastBoundingCoordinate[i],
                                    northBoundingCoordinate = bounding_boxes$northBoundingCoordinate[i],
                                    southBoundingCoordinate = bounding_boxes$southBoundingCoordinate[i])
        geographic_coverage <- new("geographicCoverage",
                                   geographicDescription = geographic_description,
                                   boundingCoordinates = bounding_coordinates)
        list_of_coverage[[(length(list_of_coverage)+1)]] <- geographic_coverage
        
      }
      
      dataset@coverage@geographicCoverage <- as(list_of_coverage, "ListOfgeographicCoverage")
      
    } 
    
  }
  
  dataset@coverage@geographicCoverage <- as(list_of_coverage, "ListOfgeographicCoverage")
  
  
  if (file.exists(paste(path, "/", "taxonomicCoverage.xml", sep = ""))){
    message("<taxonomicCoverage>")
    taxonomic_coverage <- read_eml(paste(path, "/", "taxonomicCoverage.xml", sep = ""))
    dataset@coverage@taxonomicCoverage <- as(list(taxonomic_coverage), "ListOftaxonomicCoverage")
  }

  # Add maintenance -----------------------------------------------------------
  
  message("<maintenance>")
  
  maintenance <- new("maintenance")
  maintenance@description <- as(maintenance.description, "description")
  dataset@maintenance <- maintenance
  
  # Add contacts
  
  message("<contacts>")

  useI <- which(personinfo$role == "contact")

  contact_list <- list()
  for (j in 1:length(useI)){
    contact_list[[j]] <- set_person(info_row = useI[j],
                                    person_role = "contact")
  }

  dataset@contact <- as(contact_list, "ListOfcontact")

  # Add methods
  
  message("<methods>")
  
  if (!file.exists(fname_methods)){
    stop("methods.txt doesn't exist!")
  }

  dataset@methods <- set_methods(fname_methods)
  
  if (file.exists(paste(path, "/", "geographic_coverage.txt", sep = ""))){
    
    message("<geographicDescription>")
    
    df_geographic_coverage <- read.table(paste(path,
                                               "/",
                                               "geographic_coverage.txt",
                                               sep = ""),
                                         header = T,
                                         sep="\t",
                                         quote="\"",
                                         as.is=TRUE,
                                         comment.char = "",
                                         fill = T,
                                         na.strings = "NA",
                                         colClasses = c("numeric","numeric","character"),
                                         fileEncoding = "UTF-8")
    df_geographic_coverage <- df_geographic_coverage[ ,1:3]
    colnames(df_geographic_coverage) <- c("latitude",
                                          "longitude",
                                          "site")
    
    df_geographic_coverage$latitude <- as.character(df_geographic_coverage$latitude)
    
    df_geographic_coverage$longitude <- as.character(df_geographic_coverage$longitude)
    
    df_geographic_coverage$site <- as.character(df_geographic_coverage$site)
    
    list_of_coverage <- list()
    
    for (i in 1:dim(df_geographic_coverage)[1]){
      
      coverage <- new("coverage")
      
      geographic_description <- new("geographicDescription", df_geographic_coverage$site[i])
      
      bounding_coordinates <- new("boundingCoordinates",
                                  westBoundingCoordinate = df_geographic_coverage$longitude[i],
                                  eastBoundingCoordinate = df_geographic_coverage$longitude[i],
                                  northBoundingCoordinate = df_geographic_coverage$latitude[i],
                                  southBoundingCoordinate = df_geographic_coverage$latitude[i])
      
      geographic_coverage <- new("geographicCoverage",
                                 geographicDescription = geographic_description,
                                 boundingCoordinates = bounding_coordinates)
      
      coverage@geographicCoverage <- as(list(geographic_coverage), "ListOfgeographicCoverage")
      
      list_of_coverage[[i]] <- coverage
      
    }

    sampling <- new("sampling")
    
    sampling@studyExtent@coverage <- as(list_of_coverage, "ListOfcoverage")
    
    sampling@samplingDescription <- as("Geographic coordinates of sampling sites", "samplingDescription")
    
    dataset@methods@sampling <- as(list(sampling), "ListOfsampling")

  }

  # Add provenance metadata -----------------------------------------------
  
  if (!missing(provenance)){
    message('<provenance metadata>')
    for (p in 1:length(provenance)){
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
        xml2::write_xml(
          prov_metadata,
          paste0(eml.path,
                 '/provenance_metadata.xml')
        )
        # Read provenance file and add to L0 EML
        prov_metadata <- read_eml(paste0(eml.path, '/provenance_metadata.xml'))
        methods_step <- dataset@methods@methodStep
        methods_step[[length(methods_step)+1]] <- prov_metadata
        dataset@methods@methodStep <- methods_step
        # Delete provenance_metadata.xml (a temporary file)
        file.remove(
          paste0(eml.path, '/provenance_metadata.xml')
        )
      } else {
        message('Unable to get provenance metadata.')
      }
    }
  }
  
  # Add project and funding
  
  useI <- which(personinfo$role == "pi")
  
  if (!identical(useI, integer(0))){
    
    message("<project>")
    
    pi_list <- list()
    pi_list[[1]] <- suppressWarnings(set_person(info_row = useI[1],
                                                person_role = "pi"))
    
    if (personinfo$projectTitle[useI[1]] == ""){
      if (personinfo$fundingAgency[useI[1]] == ""){
        if (personinfo$fundingNumber[useI[1]] == ""){
          project <- new("project",
                         title = "No project title to report",
                         personnel = pi_list,
                         funding = "No funding to report")
        } else if (personinfo$fundingNumber[useI[1]] != ""){
          project <- new("project",
                         title = "No project title to report",
                         personnel = pi_list,
                         funding = personinfo$fundingNumber[useI[1]])
        }
      } else if (personinfo$fundingAgency[useI[1]] != ""){
        if (personinfo$fundingNumber[useI[1]] == ""){
          project <- new("project",
                         title = "No project title to report",
                         personnel = pi_list,
                         funding = personinfo$fundingAgency[useI[1]])
        } else if (personinfo$fundingNumber[useI[1]] != ""){
          project <- new("project",
                         title = "No project title to report",
                         personnel = pi_list,
                         funding = paste0(personinfo$fundingAgency[useI[1]],
                                          ": ",
                                          personinfo$fundingNumber[useI[1]]))
        }
      }
    } else if (personinfo$projectTitle[useI[1]] != ""){
      if (personinfo$fundingAgency[useI[1]] == ""){
        if (personinfo$fundingNumber[useI[1]] == ""){
          project <- new("project",
                         title = personinfo$projectTitle[useI[1]],
                         personnel = pi_list,
                         funding = "No funding to report")
        } else if (personinfo$fundingNumber[useI[1]] != ""){
          project <- new("project",
                         title = personinfo$projectTitle[useI[1]],
                         personnel = pi_list,
                         funding = personinfo$fundingNumber[useI[1]])
        }
      } else if (personinfo$fundingAgency[useI[1]] != ""){
        if (personinfo$fundingNumber[useI[1]] == ""){
          project <- new("project",
                         title = personinfo$projectTitle[useI[1]],
                         personnel = pi_list,
                         funding = personinfo$fundingAgency[useI[1]])
        } else if (personinfo$fundingNumber[useI[1]] != ""){
          project <- new("project",
                         title = personinfo$projectTitle[useI[1]],
                         personnel = pi_list,
                         funding = paste0(personinfo$fundingAgency[useI[1]],
                                          ": ",
                                          personinfo$fundingNumber[useI[1]]))
        }
      }
    }
    dataset@project <- project
    
    
    if (length(useI) > 1){
      relatedProject_list <- list()
      pi_list <- list()
      for (i in 1:(length(useI)-1)){
        pi_list[[1]] <- suppressWarnings(set_person(info_row = useI[i+1],
                                                    person_role = "pi"))
        
        if (personinfo$projectTitle[useI[i+1]] == ""){
          if (personinfo$fundingAgency[useI[i+1]] == ""){
            if (personinfo$fundingNumber[useI[i+1]] == ""){
              relatedProject_list[[i]] <- new("relatedProject",
                                              title = "No project title to report",
                                              personnel = pi_list,
                                              funding = "No funding to report")
            } else if (personinfo$fundingNumber[useI[i+1]] != ""){
              relatedProject_list[[i]] <- new("relatedProject",
                                              title = "No project title to report",
                                              personnel = pi_list,
                                              funding = personinfo$fundingNumber[useI[i+1]])
            }
          } else if (personinfo$fundingAgency[useI[i+1]] != ""){
            if (personinfo$fundingNumber[useI[i+1]] == ""){
              relatedProject_list[[i]] <- new("relatedProject",
                                              title = "No project title to report",
                                              personnel = pi_list,
                                              funding = personinfo$fundingAgency[useI[i+1]])
            } else if (personinfo$fundingNumber[useI[i+1]] != ""){
              relatedProject_list[[i]] <- new("relatedProject",
                                              title = "No project title to report",
                                              personnel = pi_list,
                                              funding = paste0(personinfo$fundingAgency[useI[i+1]],
                                                               ": ",
                                                               personinfo$fundingNumber[useI[i+1]]))
            }
          }
        } else if (personinfo$projectTitle[useI[i+1]] != ""){
          if (personinfo$fundingAgency[useI[i+1]] == ""){
            if (personinfo$fundingNumber[useI[i+1]] == ""){
              relatedProject_list[[i]] <- new("relatedProject",
                                              title = personinfo$projectTitle[useI[i+1]],
                                              personnel = pi_list,
                                              funding = "No funding to report")
            } else if (personinfo$fundingNumber[useI[i+1]] != ""){
              relatedProject_list[[i]] <- new("relatedProject",
                                              title = personinfo$projectTitle[useI[i+1]],
                                              personnel = pi_list,
                                              funding = personinfo$fundingNumber[useI[i+1]])
            }
          } else if (personinfo$fundingAgency[useI[i+1]] != ""){
            if (personinfo$fundingNumber[useI[i+1]] == ""){
              relatedProject_list[[i]] <- new("relatedProject",
                                              title = personinfo$projectTitle[useI[i+1]],
                                              personnel = pi_list,
                                              funding = personinfo$fundingAgency[useI[i+1]])
            } else if (personinfo$fundingNumber[useI[i+1]] != ""){
              relatedProject_list[[i]] <- new("relatedProject",
                                              title = personinfo$projectTitle[useI[i+1]],
                                              personnel = pi_list,
                                              funding = paste0(personinfo$fundingAgency[useI[i+1]],
                                                               ": ",
                                                               personinfo$fundingNumber[useI[i+1]]))
            }
          }
        }
      }
      dataset@project@relatedProject <- as(relatedProject_list, "ListOfrelatedProject")
    }
    
  }
  
  # Add associated parties
  
  message("<associatedParty>")
  
  useI <- which(personinfo$role != "pi" &
                  personinfo$role != "creator" &
                  personinfo$role != "contact")
  
  if (length(useI) != 0){
    associated_party_list <- list()
    for (j in 1:length(useI)){
      associated_party_list[[j]] <- suppressWarnings(set_person(info_row = useI[j],
                                               person_role = ""))
    }
    dataset@associatedParty <- as(associated_party_list, "ListOfassociatedParty")
  }
  
  # Add additional information
  
  message("<additionalInfo>")
  
  if (file.exists(fname_additional_info)){
    
    additional_info <- as(set_TextType(fname_additional_info), "additionalInfo")
    
    dataset@additionalInfo <- as(list(additional_info), "ListOfadditionalInfo")

  }
  

  # Loop through tables -------------------------------------------------------

  if (!missing(data.files)){
    for (i in 1:length(table_names)){
      
      message(paste(
        "<dataTable> ...",
        table_names[i]))
      
      attributes <- attributes_in[[1]][[i]]
      
      # Read data table
      
      file_path <- paste(data.path,
                         "/",
                         table_names[i],
                         sep = "")
      
      delim_guess <- EDIutils::detect_delimeter(data.path, data.files = table_names[i], os)
      
      # if (delim_guess == ','){
      #   df_table <- suppressMessages(
      #     read_csv(
      #       file = file_path
      #     )
      #   )
      # } else if (delim_guess == '\t'){
      #   df_table <- suppressMessages(
      #     read_tsv(
      #       file = file_path
      #     )
      #   )
      # }
      # 
      # df_table <- as.data.frame(df_table)
      
      df_table <- read.table(file_path,
                             header = TRUE,
                             sep = delim_guess,
                             quote = "\"",
                             as.is = TRUE,
                             comment.char = "")
      
      # Read catvars file
      
      if (!is.na(match(fname_table_catvars[i], list.files(path)))){
        
        validate_fields(
          path = path,
          data.files = fname_table_catvars[i]
        )
        
        catvars <- read.table(
          paste(
            path,
            "/",
            fname_table_catvars[i], sep = ""),
          header = T,
          sep="\t",
          quote="\"",
          as.is=TRUE,
          comment.char = "",
          fill = T,
          colClasses = rep("character", 3))
        catvars <- catvars[ ,1:3]
        colnames(catvars) <- c("attributeName",
                               "code",
                               "definition")
        
        # Validate catvars: Definitions for each code
        
        use_i <- sum(catvars$attributeName != "") ==  sum(catvars$definition != "")
        
        if (!isTRUE(use_i)){
          hold <- catvars$code[catvars$definition == ""]
          stop(paste(fname_table_catvars[i], 
                     " contains codes without definition. Please add definitions to these codes: ",
                     paste(hold, collapse = ", ")))
        }
        
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
        
        # Get the column classes into a vector
        
        col_classes <- attributes[ ,"columnClasses"]
        
        # Create the attributeList element
        
        attributeList <- suppressWarnings(set_attributes(attributes,
                                                         factors = catvars,
                                                         col_classes = col_classes))
        
      } else {
        
        # Clean extraneous white spaces from attributes
        
        for (j in 1:ncol(attributes)){
          if (class(attributes[ ,j]) == "character" ||
              (class(attributes[ ,j]) == "categorical")){
            attributes[ ,j] <- trimws(attributes[ ,j])
          }
        }
        
        # Get the column classes into a vector
        
        col_classes <- attributes[ ,"columnClasses"]
        
        # Create the attributeList element
        
        attributeList <- suppressWarnings(set_attributes(attributes,
                                                         col_classes = col_classes))
        
      }
      
      # Set physical
      
      eol <- detect_eol(
        path = data.path,
        file.name = table_names[i],
        os = os
      )
      
      if (!missing("data.files.quote.character")){
        
        physical_temp <- set_physical(
          paste0(
            data.path,
            '/',
            table_names[i]
          )
        )
        
        physical <- set_physical(table_names[i],
                                 numHeaderLines = "1",
                                 recordDelimiter = eol,
                                 attributeOrientation = "column",
                                 fieldDelimiter = unlist(eml_get(physical_temp, 'fieldDelimiter')),
                                 quoteCharacter = data.files.quote.character[i])
        
      }
      
      if (missing('data.files.quote.character')) {
        
        physical_temp <- set_physical(
          paste0(
            data.path,
            '/',
            table_names[i]
          )
        )
        
        physical <- set_physical(table_names[i],
                                 numHeaderLines = "1",
                                 recordDelimiter = eol,
                                 attributeOrientation = "column",
                                 fieldDelimiter = unlist(eml_get(physical_temp, 'fieldDelimiter')))
        
      }
      
      
      
      physical@size <- new("size",
                           unit = "byte",
                           as.character(
                             file.size(
                               paste(data.path,
                                     "/",
                                     table_names[i],
                                     sep = ""))))
      
      if (!missing(data.files.url)){
        data_table_url <- paste(data.files.url,
                                "/",
                                table_names[i],
                                sep = "")
        distribution <- new("distribution",
                            online = new("online",
                                         url = data_table_url))
        physical@distribution <- new("ListOfdistribution",
                                     c(distribution))
      }
      
      
      if (os == "mac"){
        
        command_certutil <- paste("md5 ",
                                  "\"",
                                  data.path,
                                  "/",
                                  table_names[i],
                                  "\"",
                                  sep = "")
        
        certutil_output <- system(command_certutil, intern = T)
        
        checksum_md5 <- gsub(".*= ", "", certutil_output)
        
        authentication <- new("authentication",
                              method = "MD5",
                              checksum_md5)
        
        physical@authentication <- as(list(authentication),
                                      "ListOfauthentication")
        
      } else if (os == "win"){
        
        command_certutil <- paste("CertUtil -hashfile ",
                                  "\"",
                                  data.path,
                                  "\\",
                                  table_names[i],
                                  "\"",
                                  " MD5",
                                  sep = "")
        
        certutil_output <- system(command_certutil, intern = T)
        
        checksum_md5 <- gsub(" ", "", certutil_output[2])
        
        authentication <- new("authentication",
                              method = "MD5",
                              checksum_md5)
        
        physical@authentication <- as(list(authentication),
                                      "ListOfauthentication")
        
      } else if (os == "lin"){
        
        command_certutil <- paste0("md5sum ",
                                   "\"",
                                   data.path,
                                   "/",
                                   table_names[i],
                                   "\"")
        
        certutil_output <- system(command_certutil, intern = T)
        
        checksum_md5 <- strsplit(certutil_output, split = " ")[[1]][1]
        
        authentication <- new("authentication",
                              method = "MD5",
                              checksum_md5)
        
        physical@authentication <- as(list(authentication),
                                      "ListOfauthentication")
        
        
        
      }
      
      # Get number of records
      
      number_of_records <- as.character(dim(df_table)[1])
      
      # Pull together information for the data table
      
      data_table <- new("dataTable",
                        entityName = data.files.description[i],
                        entityDescription = data.files.description[i],
                        physical = physical,
                        attributeList = attributeList,
                        numberOfRecords = number_of_records)
      
      data_tables_stored[[i]] <- data_table
      
    }
    
    # Compile datatables --------------------------------------------------------
    
    # Are custom units present in these tables?
    
    if (file.exists(fname_custom_units)){
      
      custom_units_df <- read.table(
        fname_custom_units,
        header = T,
        sep="\t",
        quote="\"",
        as.is=TRUE,
        comment.char = "",
        fill = T,
        colClasses = rep("character", 5))
      custom_units_df <- custom_units_df[ ,1:5]
      colnames(custom_units_df) <- c("id",
                                     "unitType",
                                     "parentSI",
                                     "multiplierToSI",
                                     "description")
      
      if (nrow(custom_units_df) < 1){
        custom_units <- "no"
      } else {
        custom_units <- "yes"
      }
      
      # Clean white spaces from custom_units and units_types
      
      if (custom_units == "yes"){
        
        message("<additionalMetadata>")
        
        for (j in 1:ncol(custom_units_df)){
          if (class(custom_units_df[ ,j]) == "character" ||
              (class(custom_units_df[ ,j]) == "factor")){
            custom_units_df[ ,j] <- trimws(custom_units_df[ ,j])
          }
        }
        
        unitsList <- set_unitList(custom_units_df)
      }
      
      
    } else {
      
      custom_units <- "no"
      
    }
    
    
    # Compile data tables
    
    dataset@dataTable <- new("ListOfdataTable",
                             data_tables_stored)
    
  }
  

  

  # Add otherEntity -----------------------------------------------------------
  
  if (!missing(zip.dir)){
    
    if (length(zip.dir) > 0){

      list_of_other_entity <- list()
      
      for (i in 1:length(zip.dir)){
        
        message(paste0("<otherEntity> ... ", zip.dir[i]))
        
        # Create new other entity element
        
        other_entity <- new("otherEntity")
        
        # Add code file names
        
        other_entity@entityName <- zip.dir.description[i]
        
        # Add description
        
        description <- zip.dir.description[i]
        
        other_entity@entityDescription <- description
        
        #  Build physical
        
        physical <- new("physical",
                        objectName = zip.dir[i])
        
        format_name <- "zip directory"
        entity_type <- "zip directory"
        physical@dataFormat@externallyDefinedFormat@formatName <- format_name
        
        physical@size <- new("size", unit = "bytes", as(as.character(file.size(paste(data.path, "/", zip.dir[i], sep = ""))), "size"))
        
        if (os == "mac"){
          
          command_certutil <- paste("md5 ",
                                    "\"",
                                    data.path,
                                    "/",
                                    zip.dir[i],
                                    "\"",
                                    sep = "")
          
          certutil_output <- system(command_certutil, intern = T)
          
          checksum_md5 <- gsub(".*= ", "", certutil_output)
          
          authentication <- new("authentication",
                                method = "MD5",
                                checksum_md5)
          
          physical@authentication <- as(list(authentication),
                                        "ListOfauthentication")
          
        } else if (os == "win"){
          
          command_certutil <- paste("CertUtil -hashfile ",
                                    "\"",
                                    data.path,
                                    "\\",
                                    zip.dir[i],
                                    "\"",
                                    " MD5",
                                    sep = "")
          
          certutil_output <- system(command_certutil, intern = T)
          
          checksum_md5 <- gsub(" ", "", certutil_output[2])
          
          authentication <- new("authentication",
                                method = "MD5",
                                checksum_md5)
          
          physical@authentication <- as(list(authentication),
                                        "ListOfauthentication")
          
        } else if (os == "lin"){
          
          command_certutil <- paste0("md5sum ",
                                     "\"",
                                     data.path,
                                     "/",
                                     zip.dir[i],
                                     "\"")
          
          certutil_output <- system(command_certutil, intern = T)
          
          checksum_md5 <- strsplit(certutil_output, split = " ")[[1]][1]
          
          authentication <- new("authentication",
                                method = "MD5",
                                checksum_md5)
          
          physical@authentication <- as(list(authentication),
                                        "ListOfauthentication")
          
          
          
        }
        
        other_entity@physical <- as(c(physical), "ListOfphysical")
        
        # Add entity type
        
        other_entity@entityType <- entity_type
        
        # Add other entity to list
        
        list_of_other_entity[[i]] <- other_entity
        
      }
      
      dataset@otherEntity <- new("ListOfotherEntity",
                                 list_of_other_entity)
      
    }
    
    
  }

  
  # Build EML -----------------------------------------------------------------
  
  message("Compiling EML.")
  
  if (exists('custom_units')){
    if (custom_units == "yes"){
      eml <- new("eml",
                 schemaLocation = "eml://ecoinformatics.org/eml-2.1.1  http://nis.lternet.edu/schemas/EML/eml-2.1.1/eml.xsd",
                 packageId = data_package_id,
                 system = "edi",
                 access = access,
                 dataset = dataset,
                 additionalMetadata = as(unitsList, "additionalMetadata"))
    } else {
      eml <- new("eml",
                 schemaLocation = "eml://ecoinformatics.org/eml-2.1.1  http://nis.lternet.edu/schemas/EML/eml-2.1.1/eml.xsd",
                 packageId = data_package_id,
                 system = "edi",
                 access = access,
                 dataset = dataset)
    }
  } else {
    eml <- new("eml",
               schemaLocation = "eml://ecoinformatics.org/eml-2.1.1  http://nis.lternet.edu/schemas/EML/eml-2.1.1/eml.xsd",
               packageId = data_package_id,
               system = "edi",
               access = access,
               dataset = dataset)
  }

  # Write EML

  message("Writing EML.")
  
  write_eml(eml, paste(eml.path, "/", data_package_id, ".xml", sep = ""))
  
  # Validate EML
  
  message("Validating EML.")
  
  validation_result <- eml_validate(eml)
  
  if (validation_result == "TRUE"){
    
    message("EML passed validation!")
    
  } else {
    
    message("EML validaton failed. See warnings for details.")
    
  }
  
  message("Done.")
  
}



