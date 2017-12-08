#' Make EML
#'
#' @description  
#'     Translate user supplied metadata templates into the EML, validate the 
#'     EML, and write to file.
#'     
#' @usage 
#'     make_eml(path = "", 
#'     dataset.title = "", 
#'     data.files = c("df.1", "df.2", ...), 
#'     data.files.description = c("df1.desc", "df.2.desc", ...), 
#'     data.files.quote.character = c("df.1.quote.char", "df.2.quote.char", ...), 
#'     data.files.url = "", 
#'     temporal.coverage = c("begin_date", "end_date"), 
#'     geographic.description = "", 
#'     geographic.coordinates = c("North", "East", "South", "West"), 
#'     maintenance.description = "", 
#'     user.id = "", 
#'     package.id = "")
#'     
#' @param path 
#'     A path to the dataset working directory containing the completed 
#'     metadata templates, \emph{attributes_datatablename.txt}, 
#'     \emph{catvars_datatablename.txt} (if categorical variables 
#'     are present), and \emph{geographic_coverage.txt} (if reporting detailed 
#'     geographic coverage).
#' @param dataset.title
#'     A character string specifying the title for your dataset. Be descriptive
#'     (more than 5 words). We recommend the following format: Project name: 
#'     Broad description: Time span (e.g. "GLEON: Long term lake chloride 
#'     concentrations from North America and Europe: 1940-2016").
#' @param data.files
#'     A list of character strings specifying the names of the data files
#'     of your dataset (e.g. data.files = c("lake_chloride_concentrations.csv", 
#'     "lake_characteristics.csv"))
#' @param data.files.description
#'     A list of character strings briefly describing the data files listed in the 
#'     data.files argument and in the same order as listed in the data.files
#'     argument (e.g. data.files.description = c("Chloride concentration data.", 
#'     "Climate, road density, and impervious surface data."))
#' @param data.files.quote.character
#'     A list of character strings defining the quote characters used in your 
#'     data files and in the same order as listed in the data.files argument.  
#'     If the quote character is a quotation, then enter "\\"". If the quote 
#'     character is an apostrophe, then enter "\\'". If there is no quote 
#'     character then don't use this argument when running \code{make_eml}.
#'     Example: data.files.quote.character = c("\\"", "\\"").
#' @param data.files.url
#'     A character string specifying the URL of where your data tables are 
#'     stored on a publicly accessible server (i.e. does not require user ID 
#'     or password). The EDI data repository software, PASTA+, will use this to
#'     upload your data into the repository. If you will be manually uploading 
#'     your data tables, then don't use this argument when running 
#'     \code{make_eml}. Example: data.files.url = "https://lter.limnology.wisc.edu/sites/default/files/data/gleon_chloride".
#' @param temporal.coverage
#'     A list of character strings specifying the beginning and ending dates 
#'     of your dataset. use the format YYYY-MM-DD.
#' @param geographic.description
#'     A character string describing the geographic coverage of your dataset 
#'     (e.g. "North America and Europe").
#' @param geographic.coordinates
#'     A list of character strings specifying the spatial bounding 
#'     coordinates of your dataset in decimal degrees. The list must follow 
#'     this order: "North", "East", "South", "West". Longitudes west of the 
#'     prime meridian and latitudes south of the equator are prefixed with a 
#'     minus sign (i.e. dash -). If you don't have an area, but rather a point,
#'     Repeat the latitude value for North and South, and repeat the longitude
#'     value for East and West (e.g. geographic.coordinates = c("28.38", 
#'     "-119.95", "28.38", "-119.95")).
#' @param maintenance.description
#'     A character string specifying whether data collection for this dataset 
#'     is "ongoing" or "completed".
#' @param user.id
#'     A character string specifying your user ID for the EDI data repository.
#'     If you do not have one, contact EDI (info@environmentaldatainitiative.org)
#'     to obtain one. Alternatively, do not use this argument when running
#'     \code{make_eml}.
#' @param package.id
#'     A character string specifying the package ID for your data package. If 
#'     you do not have a package ID, then do not use this argument when running 
#'     \code{make_eml}. A missing package ID defaults to \emph{edi.101.1}.
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


make_eml <- function(path, dataset.title, data.files, data.files.description, 
                     data.files.quote.character, data.files.url,
                     temporal.coverage, geographic.description, 
                     geographic.coordinates, maintenance.description, user.id, 
                     package.id) {

  # Check arguments and parameterize ------------------------------------------
  
  if (missing(path)){
    stop('Input argument "path" is missing! Specify path to your dataset working directory.')
  }
  if (missing(dataset.title)){
    stop('Input argument "dataset.title" is missing! Add a title for your dataset.')
  }
  if (missing(data.files)){
    stop('Input argument "data.files" is missing! List the names of your datasets data files.')
  }
  if (missing(data.files.description)){
    stop('Input argument "data.files.description" is missing! Please describe your data files.')
  }
  if (missing(temporal.coverage)){
    stop('Input argument "temporal.coverage" is missing! Add the temporal coverage of your dataset.')
  }
  if (missing(geographic.description)){
    stop('Input argument "geographic.description" is missing! Add this description for your dataset.')
  }
  if (missing(geographic.coordinates)){
    stop('Input argument "geographic.coordinates" is missing! Add add geographic coordinates for your dataset.')
  }
  if (missing(maintenance.description)){
    stop('Input argument "maintenance.description" is missing! Indicate whether data collection is "ongoing" or "completed" for your dataset.')
  }
  
  # Validate path
  
  validate_path(path)
  
  # Validate data file names
  
  table_names <- validate_file_names(path, data.files)
  
  # Validate fields of data.files
  
  validate_fields(path, data.files = table_names)
  
  # Detect operating system
  
  os <- detect_os()
  
  # Set package ID
  
  if (!missing(package.id)){
    data_package_id <- package.id
  } else {
    data_package_id <- "edi.101.1"
  }
  
  # Validate data.files.description
  
  if (length(data.files.description) != length(data.files)){
    stop('The number of descriptions listed in the argument "data.files.description" does not match the number of files listed in the argument "data.files". These must match.')
  }
  
  # Validate data.files.quote.character
  
  if (!missing(data.files.quote.character)){
    if (length(data.files.quote.character) != length(data.files)){
      stop('The number of quote characters listed in the argument "data.files.quote.character" does not match the number of files listed in the argument "data.files". These must match.')
    }
  }
  
  # Validate temporal.coverage
  
  if (length(temporal.coverage) != 2){
    stop('The argument "temporal.coverage" requires both a begin date and end date. Please fix this.')
  }
  
  # Validate geographic.coordinates
  
  if (length(geographic.coordinates) != 4){
    stop('The argument "geographic.coordinates" requires North, East, South, and West bounding coordinates. If reporting a point and not an area, replicate the respective latitude and longitude coordinates.")')
  }
  
  # Compile attributes --------------------------------------------------------
  
  attributes_in <- compile_attributes(path, data.files)
  
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
  for (i in 1:length(table_names)){
    fname_table_catvars[i] <- paste("catvars_",
      substr(table_names[i], 1, nchar(table_names[i]) - 4),
      ".txt",
      sep = "")
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

  # Read personnel file

  message("Reading personnel file.")
  
  personinfo <- read.table(
    fname_personnel,
    header=TRUE,
    sep="\t",
    quote="\"",
    as.is=TRUE,
    comment.char = "",
    colClasses = rep("character", 8))

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
  
  # Validate personnel: required roles are present
  
  if (sum(personinfo$role %in% "creator") == 0){
    stop("Your dataset is missing a creator. Add one to personnel.txt.")
  }
  if (sum(personinfo$role %in% "contact") == 0){
    stop("Your dataset is missing a contact. Add one to personnel.txt.")
  }
  if (sum(personinfo$role %in% "pi") == 0){
    stop("Your dataset is missing a principal investigator. Add one to personnel.txt.")
  }
  
  # Validate personnel: fundingNumber is present if fundingAgency is present
  
  use_i <- personinfo$fundingAgency != ""
  use_i2 <- personinfo$fundingNumber != ""
  use_i3 <- use_i != use_i2
  
  if (sum(use_i3 > 0)){
    rown <- seq(length(use_i3))[use_i3] + 1
    stop(paste("Row number(s): ",
               paste(rown, collapse = ", "),
               " of personnel.txt are missing fundingNumber or fundingAgency. If one is listed, the other musth be listed as well. Please add these."))
  }
  
  # Validate personnel: project info is associated with first listed PI
  
  use_i <- personinfo$role == "pi"
  pis <- personinfo[use_i, ]
  pi_proj <- pis[ , c("projectTitle", "fundingAgency", "fundingNumber")]
  
  if ((sum(pi_proj == "") > 0) & (sum(pi_proj[1, ] == "") == 3)){
    stop("The first Principal Investigator listed in personnel.txt is missing a projectTitle, fundingAgency, or fundingNumber. The first listed PI represents the major project and requires this. Please add one.")
  }
  
  # Build modules--------------------------------------------------------------

  message("Building EML ...")
  
  # Create EML
  
  # Build eml-access module ---------------------------------------------------
  
  message("<access>")
  
  if (!missing(user.id)){
    allow_principals <- c(paste("uid=",
                                user.id,
                                ",o=LTER,dc=ecoinformatics,dc=org",
                                sep = ""),
                          "public")
    
    allow_permissions <- c("all",
                           "read")
  } else if (exists("user_id")){
    allow_principals <- c(paste("uid=",
                                user_id,
                                ",o=LTER,dc=ecoinformatics,dc=org",
                                sep = ""),
                          "public")
    
    allow_permissions <- c("all",
                           "read")
  } else {
    allow_principals <- c("public")
    allow_permissions <- c("read")
  }

  access_order <- "allowFirst"
  
  access_scope <- "document"
  
  access <- new("access",
                scope = access_scope,
                order = access_order,
                authSystem = "https://pasta.edirepository.org/authentication")

  allow <- list()
  for (i in 1:length(allow_principals)){
    allow[[i]] <- new("allow",
                      principal = allow_principals[i],
                      permission = allow_permissions[i])
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

  keywords <- read.table(
    paste(substr(fname_keywords, 1, nchar(fname_keywords) - 4),
          ".txt",
          sep = ""),
    header = TRUE,
    sep = "\t",
    as.is = TRUE,
    na.strings = "NA",
    colClasses = "character")
  
  colnames(keywords) <- c("keyword", "keywordThesaurus")
  
  # Edit keywords: Remove blank keyword entries
  
  use_i <- keywords$keyword == ""
  if (sum(use_i) > 0){
    keywords <- keywords[!use_i, ]
  }
  
  # 
  
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
  
  # Set temporal and geographic coverage
  
  message("<temporalCoverage>")
  
  message("<geographicCoverage>")
  
  dataset@coverage <- set_coverage(begin = temporal.coverage[1],
                                   end = temporal.coverage[2],
                                   geographicDescription = geographic.description,
                                   west = as.numeric(geographic.coordinates[4]),
                                   east = as.numeric(geographic.coordinates[2]),
                                   north = as.numeric(geographic.coordinates[1]),
                                   south = as.numeric(geographic.coordinates[3]))
  
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
                                         sep = "\t",
                                         header = TRUE,
                                         as.is = TRUE,
                                         quote="",
                                         na.strings = "NA",
                                         colClasses = c("numeric","numeric","character"),
                                         comment.char = "#",
                                         fileEncoding = "UTF-8")
    
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

  # Add project and funding
  
  message("<project>")
  
  useI <- which(personinfo$role == "pi")
  
  pi_list <- list()
  pi_list[[1]] <- suppressWarnings(set_person(info_row = useI[1],
                             person_role = "pi"))
  
  if (personinfo$projectTitle[useI[1]] == ""){
    if (personinfo$fundingAgency[useI[1]] == ""){
      project <- new("project",
                     title = "No project title to report",
                     personnel = pi_list,
                     funding = "No funding to report")
    } else {
      project <- new("project",
                     title = "No project title to report",
                     personnel = pi_list,
                     funding = paste(personinfo$fundingAgency[useI[1]],
                                     ": ",
                                     personinfo$fundingNumber[useI[1]],
                                     sep = ""))
    }
  } else if (personinfo$projectTitle[useI[1]] != ""){
    if (personinfo$fundingAgency[useI[1]] == ""){
      project <- new("project",
                     title = personinfo$projectTitle[useI[[1]]],
                     personnel = pi_list,
                     funding = "No funding to report")
    } else {
      project <- new("project",
                     title = personinfo$projectTitle[useI[[1]]],
                     personnel = pi_list,
                     funding = paste(personinfo$fundingAgency[useI[1]],
                                     ": ",
                                     personinfo$fundingNumber[useI[1]],
                                     sep = ""))
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
          relatedProject_list[[i]] <- new("relatedProject",
                                          title = "No project title to report",
                                          personnel = pi_list,
                                          funding = "No funding to report")
        } else {
          relatedProject_list[[i]] <- new("relatedProject",
                                          title = "No project title to report",
                                          personnel = pi_list,
                                          funding = paste(personinfo$fundingAgency[useI[i+1]],
                                                          ": ",
                                                          personinfo$fundingNumber[useI[i+1]],
                                                          sep = ""))
        }
      } else if (personinfo$projectTitle[useI[i+1]] != ""){
        if (personinfo$fundingAgency[useI[i+1]] == ""){
          relatedProject_list[[i]] <- new("relatedProject",
                                          title = personinfo$projectTitle[useI[[i+1]]],
                                          personnel = pi_list,
                                          funding = "No funding to report")
        } else {
          relatedProject_list[[i]] <- new("relatedProject",
                                          title = personinfo$projectTitle[useI[[i+1]]],
                                          personnel = pi_list,
                                          funding = paste(personinfo$fundingAgency[useI[i+1]],
                                                          ": ",
                                                          personinfo$fundingNumber[useI[i+1]],
                                                          sep = ""))
        }
      }
    }
    dataset@project@relatedProject <- as(relatedProject_list, "ListOfrelatedProject")
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

  for (i in 1:length(table_names)){

    message(paste(
      "<dataTable> ...",
      table_names[i]))
    
    attributes <- attributes_in[[1]][[i]]
    
    # Read data table
    
    file_path <- paste(path,
                       "/",
                       table_names[i],
                       sep = "")
    
    nlines <- length(readLines(file_path))
    
    if (os == "mac"){
      delim_guess <- suppressWarnings(get.delim(file_path,
                                                n = 2,
                                                delims = c("\t",
                                                           ",",
                                                           ";",
                                                           "|")))
    } else if (os == "win"){
      delim_guess <- get.delim(file_path,
                               n = nlines/2,
                               delims = c("\t",
                                          ",",
                                          ";",
                                          "|"))
    }
    
    df_table <- read.table(file_path,
                           header = TRUE,
                           sep = delim_guess,
                           quote = "\"",
                           as.is = TRUE,
                           comment.char = "")

    # Read catvars file

    if (!is.na(match(fname_table_catvars[i], list.files(path)))){

      catvars <- read.table(
        paste(
          path,
          "/",
          fname_table_catvars[i], sep = ""),
        header=TRUE,
        sep="\t",
        quote="\"",
        as.is=TRUE,
        comment.char = "")
      
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
    
    
    if (os == "mac"){
      record_delimeter <- "\\n"
    } else if (os == "win"){
      record_delimeter <- "\\r\\n"
    }

    if (!missing("data.files.quote.character")){
      
      physical <- set_physical(table_names[i],
                               numHeaderLines = "1",
                               recordDelimiter = record_delimeter,
                               attributeOrientation = "column",
                               fieldDelimiter = delim_guess,
                               quoteCharacter = data.files.quote.character[i])
      
    } else {
      
      physical <- set_physical(table_names[i],
                               numHeaderLines = "1",
                               recordDelimiter = record_delimeter,
                               attributeOrientation = "column",
                               fieldDelimiter = delim_guess)
      
    }

    
    
    physical@size <- new("size",
                         unit = "byte",
                         as.character(
                           file.size(
                             paste(path,
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
                                path,
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
                                path,
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
      
    }
    
    # Get number of records
    
    number_of_records <- as.character(dim(df_table)[1])

    # Pull together information for the data table

    data_table <- new("dataTable",
                      entityName = table_names[i],
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
      header = TRUE,
      sep = "\t",
      as.is = TRUE,
      na.strings = "")
    
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

  # Build EML
  
  message("Compiling EML.")
  
  
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

  # Write EML

  message("Writing EML.")
  
  write_eml(eml, paste(path, "/", data_package_id, ".xml", sep = ""))
  
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

