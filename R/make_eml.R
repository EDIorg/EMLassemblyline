#' Render templates into EML metadata
#'
#' @description  
#'     Render the content of metadata templates into the EML, then validate the
#'     EML and write to file.
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
#'       data.table.description = NULL, 
#'       data.table.quote.character = NULL, 
#'       other.entity = NULL,
#'       other.entity.description = NULL,
#'       data.url = NULL,
#'       provenance = NULL,
#'       user.id = NULL,
#'       user.domain = NULL,
#'       package.id = NULL,
#'       write.file = TRUE,
#'       return.obj = FALSE,
#'       x = NULL,
#'       affiliation = 'deprecated',
#'       data.files = 'deprecated',
#'       data.files.description = 'deprecated',
#'       data.files.quote.character = 'deprecated',
#'       data.files.url = 'deprecated',
#'       zip.dir = 'deprecated',
#'       zip.dir.description = 'deprecated'
#'     )
#'     
#' @param path 
#'     (character) Path to where the template(s) are stored.
#' @param data.path
#'     (character) Path to where the data files are stored.
#' @param eml.path
#'     (character) Path to where the EML will be written.
#' @param dataset.title
#'     (character) Dataset title. Be descriptive (more than 5 words) and 
#'     consider including the temporal coverage (e.g. "GLEON: Long term lake 
#'     chloride concentrations from North America and Europe: 1940-2016").
#' @param temporal.coverage
#'     (character) Beginning and ending dates of the dataset as a vector of 
#'     character strings in the format `YYYY-MM-DD`.
#' @param geographic.description
#'     (character) Geographic description. Don't use this argument if geographic
#'     coordinates in the `bounding_boxes.txt` template.
#' @param geographic.coordinates
#'     (character) Geographic coordinates delineating the bounding area or 
#'     point of a dataset, in decimal degrees. This argument is not required 
#'     if using `bounding_boxes.txt`. Values must be listed in this order:
#'     North, East, South, West. Longitudes West of the 
#'     prime meridian and latitudes South of the equator are negative. If 
#'     representing a point, repeat the latitude for North and South, and 
#'     repeat the longitude for East and West (e.g. 
#'     `geographic.coordinates = c('28.38', '-119.95', '28.38', '-119.95)`).
#' @param maintenance.description
#'     (character) Indicator of whether data collection is `ongoing` or 
#'     `completed`.
#' @param data.table
#'     (character) Data table name(s). If more than one, then supply as a 
#'     vector of character strings (e.g. 
#'     `data.table = c("concentrations.csv", "characteristics.csv")`).
#' @param data.table.description
#'     (character) Data table description(s). Brief description of `data.table`.
#'     If more than one, then supply as a vector of character strings in the 
#'     same order as listed in `data.table`.
#' @param data.table.quote.character
#'     (character) Quote character(s) used in `data.table`. If more than one, 
#'     then supply as a vector of character strings in the same order as 
#'     listed in `data.table`. This argument is required only if your data 
#'     contain quotations. If the quote character is a quotation, then enter 
#'     `"\\""`. If the quote character is an apostrophe, then enter `"\\'"`.
#' @param other.entity
#'     (character) Name(s) of `other.entity`(s) in this dataset. Use
#'     `other.entity` for all non-`data.table` files. `other.entity`(s) should 
#'     be stored at `data.path`.
#' @param other.entity.description
#'     (character) Description(s) of `other.entity`(s). If more than one, then 
#'     supply as a vector of descriptions in the same order as listed in 
#'     `other.entity`.
#' @param data.url
#'     (character) The URL of where your data can be downloaded by a
#'     data repository. This argument is not required, if the data will be 
#'     manually uploaded to a data repository.
#' @param provenance
#'     (character) EDI Data Repository Data package ID(s) corresponding to 
#'     parent datasets from which this dataset was created (e.g. 
#'     `knb-lter-cap.46.3`).
#' @param user.id
#'     (character) ID(s) of data repository user account(s). If more than one,
#'     supply as a vector of character strings. Contact EDI 
#'     (info@@environmentaldatainitiative.org) to obtain one. This argument is
#'     not required.
#' @param user.domain
#'     (character) Domain of the `user.id`(s). Valid options for EDI are
#'     `LTER` and `EDI`. If more than one, supply as a vector of character 
#'     strings in the same order as corresponding `user.id`(s). This argument
#'     is not required.
#' @param package.id
#'     (character) EDI Data Repository Data package ID for this dataset. A 
#'     missing package ID defaults to `edi.101.1`.
#' @param write.file
#'     (logical) Write file to `eml.path`.
#' @param return.obj
#'     (logical) Return the EML as an R object of class `EML object`.
#' @param x
#'     (named list) Alternative input/output to `EMLassemblyline` functions. 
#'     Use `read_files()` to create `x`.
#' @param affiliation
#'     This argument has been deprecated. Use `user.domain` instead.
#' @param data.files
#'     This argument has been deprecated. Use `data.table` instead.
#' @param data.files.description
#'     This argument has been deprecated. Use `data.table.description` instead.
#' @param data.files.quote.character
#'     This argument has been deprecated. Use `data.table.quote.character` 
#'     instead.
#' @param data.files.url
#'     This argument has been deprecated. Use `data.url` instead.
#' @param zip.dir
#'     This argument has been deprecated. Use `other.entity` instead.
#' @param zip.dir.description
#'     This argument has been deprecated. Use `other.entity.description` 
#'     instead.
#'     
#' @return 
#'     \itemize{
#'         \item{`Status messages` describing the EML creation status}
#'         \item{`Validation result` indicating whether the EML is schema valid}
#'         \item{`EML file` written to `path` and in the form `package.id.xml`}
#'         \item{`EML object` when `return.obj = TRUE`}
#'     }
#'     
#' @details 
#'     If validation fails, open the EML document in a .xml editor to identify 
#'     the source of error. Often the error is small and quickly resolved with 
#'     the aid of an editors schema validator. If the issue can't be resolved
#'     then forward the `EML` to info@@environmentaldatainitiative.org for 
#'     support.
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
  data.table.description = NULL, 
  data.table.quote.character = NULL, 
  other.entity = NULL,
  other.entity.description = NULL,
  data.url = NULL,
  provenance = NULL,
  user.id = NULL,
  user.domain = NULL,
  package.id = NULL,
  write.file = TRUE,
  return.obj = FALSE,
  x = NULL,
  affiliation = 'deprecated',
  data.files = 'deprecated',
  data.files.description = 'deprecated',
  data.files.quote.character = 'deprecated',
  data.files.url = 'deprecated',
  zip.dir = 'deprecated',
  zip.dir.description = 'deprecated') {

  # Validate arguments --------------------------------------------------------
  
  if (missing(path)){
    stop('Input argument "path" is missing.')
  }
  
  validate_arguments(
    fun.name = as.character(match.call()[[1]]),
    fun.args = as.list(environment())
  )
  
  # Read metadata templates and data ------------------------------------------
  
  if (is.null(x)){
    
    table_names <- EDIutils::validate_file_names(
      path = data.path, 
      data.files = data.table
    )
    
    # Read templates and data into x
    
    if (is.null(data.table) & is.null(other.entity)){
      
      x <- read_files(
        path = path,
        data.path = data.path
      )
      
    } else if (!is.null(data.table) & is.null(other.entity)){
      
      x <- read_files(
        path = path,
        data.path = data.path,
        data.table = table_names
      )
      
    } else if (!is.null(data.table) & !is.null(other.entity)){
      
      x <- read_files(
        path = path,
        data.path = data.path,
        data.table = table_names,
        other.entity = other.entity
      )
      
    } else if (is.null(data.table) & !is.null(other.entity)){
      
      x <- read_files(
        path = path,
        data.path = data.path,
        other.entity = other.entity
      )
      
    }
    
    data_read_2_x <- TRUE
    
  }
  
  # Validate x and templates --------------------------------------------------

  # validate_x(
  #   fun.name = as.character(match.call()[[1]]),
  #   fun.args = as.list(environment())
  # )
  # 
  # validate_templates(
  #   fun.name = as.character(match.call()[[1]]),
  #   fun.args = as.list(environment())
  # )

  # Compile attributes --------------------------------------------------------
  
  if (!is.null(data.table)){
    attributes_in <- compile_attributes(x = x)
  }
  
  # Initialize data entity storage (tables)

  data_tables_stored <- list()

  # Load helper function to set personnel roles

  set_person <- function(info_row, person_role){

    if (person_role == "contact"){

      if ((personinfo[info_row, "givenName"] != "") & (personinfo[info_row, "middleInitial"] == "") & (personinfo[info_row, "surName"] == "")){
        
        contact <- methods::new(
          "contact",
          organizationName = trimws(personinfo[info_row,"organizationName"]),
          positionName = stringr::str_to_title(trimws(personinfo[info_row,"givenName"])),
          electronicMailAddress = trimws(personinfo[info_row,"electronicMailAddress"]))
        
        if (nchar(trimws(personinfo[info_row,"userId"])) == 19){
          userId <- methods::new("userId")
          userId@directory <- methods::new("xml_attribute", "https://orcid.org")
          hold <- trimws(personinfo[info_row,"userId"])
          hold <- paste("https://orcid.org/", hold, sep = "")
          userId@.Data <- hold
          contact@userId <- methods::new("ListOfuserId", c(userId))
        }
        
      } else {
        
        individualName <- methods::new(
          "individualName",
          givenName = c(trimws(personinfo[info_row,"givenName"]),
                        trimws(personinfo[info_row,"middleInitial"])),
          surName = trimws(personinfo[info_row,"surName"]))
        
        contact <- methods::new(
          "contact",
          individualName = individualName,
          organizationName = trimws(personinfo[info_row,"organizationName"]),
          electronicMailAddress = trimws(personinfo[info_row,"electronicMailAddress"]))
        
        if (nchar(trimws(personinfo[info_row,"userId"])) == 19){
          userId <- methods::new("userId")
          userId@directory <- methods::new("xml_attribute", "https://orcid.org")
          hold <- trimws(personinfo[info_row,"userId"])
          hold <- paste("https://orcid.org/", hold, sep = "")
          userId@.Data <- hold
          contact@userId <- methods::new("ListOfuserId", c(userId))
        }
        
      }

      contact

    } else if (person_role == "creator"){

      individualName <- methods::new(
        "individualName",
        givenName = c(trimws(personinfo[info_row,"givenName"]),
                      trimws(personinfo[info_row,"middleInitial"])),
        surName = trimws(personinfo[info_row,"surName"]))

      creator <- methods::new(
        "creator",
        individualName = individualName,
        organizationName = trimws(personinfo[info_row,"organizationName"]),
        electronicMailAddress = trimws(personinfo[info_row,"electronicMailAddress"]))

      if (nchar(trimws(personinfo[info_row,"userId"])) == 19){
        userId <- methods::new("userId")
        userId@directory <- methods::new("xml_attribute", "https://orcid.org")
        hold <- trimws(personinfo[info_row,"userId"])
        hold <- paste("https://orcid.org/", hold, sep = "")
        userId@.Data <- hold
        creator@userId <- methods::new("ListOfuserId", c(userId))
      }
      creator

    } else if (person_role == "pi"){

      individualName <- methods::new(
        "individualName",
        givenName = c(trimws(personinfo[info_row,"givenName"]),
                      trimws(personinfo[info_row,"middleInitial"])),
        surName = trimws(personinfo[info_row,"surName"]))

      principal_investigator <- utils::as.person(
        paste(
          trimws(personinfo[info_row, "givenName"]),
          " ",
          trimws(personinfo[info_row, "surName"]),
          " <",
          trimws(personinfo[info_row, "electronicMailAddress"]),
          ">",
          sep = ""))

      rp_personnel <- methods::as(principal_investigator, "personnel")

      if (nchar(trimws(personinfo[info_row,"userId"])) == 19){
        rp_userId <- methods::new("userId")
        rp_userId@directory <- methods::new("xml_attribute", "https://orcid.org")
        hold <- trimws(personinfo[info_row,"userId"])
        hold <- paste("https://orcid.org/", hold, sep = "")
        rp_userId@.Data <- hold
        rp_personnel@userId <- methods::new("ListOfuserId", c(rp_userId))
      }
      role <- methods::new("role", "Principal Investigator")
      rp_personnel@role <- methods::new("ListOfrole", c(role))
      rp_personnel
      
    } else {
      
      if ((personinfo[info_row, "givenName"] == "") & (personinfo[info_row, "middleInitial"] == "") & (personinfo[info_row, "surName"] == "")){
        
        associated_party <- methods::new(
          "associatedParty",
          organizationName = trimws(personinfo[info_row,"organizationName"]),
          electronicMailAddress = trimws(personinfo[info_row,"electronicMailAddress"]))
        
        if (nchar(trimws(personinfo[info_row,"userId"])) == 19){
          userId <- methods::new("userId")
          userId@directory <- methods::new("xml_attribute", "https://orcid.org")
          hold <- trimws(personinfo[info_row,"userId"])
          hold <- paste("https://orcid.org/", hold, sep = "")
          userId@.Data <- hold
          associated_party@userId <- methods::new("ListOfuserId", c(userId))
        }
        role <- methods::new("role", stringr::str_to_title(trimws(personinfo[info_row,"role"])))
        associated_party@role <- methods::new("role", c(role))
        associated_party
        
      } else {
        
        individualName <- methods::new(
          "individualName",
          givenName = c(trimws(personinfo[info_row,"givenName"]),
                        trimws(personinfo[info_row,"middleInitial"])),
          surName = trimws(personinfo[info_row,"surName"]))
        
        associated_party <- methods::new(
          "associatedParty",
          individualName = individualName,
          organizationName = trimws(personinfo[info_row,"organizationName"]),
          electronicMailAddress = trimws(personinfo[info_row,"electronicMailAddress"]))
        
        if (nchar(trimws(personinfo[info_row,"userId"])) == 19){
          userId <- methods::new("userId")
          userId@directory <- methods::new("xml_attribute", "https://orcid.org")
          hold <- trimws(personinfo[info_row,"userId"])
          hold <- paste("https://orcid.org/", hold, sep = "")
          userId@.Data <- hold
          associated_party@userId <- methods::new("ListOfuserId", c(userId))
        }
        role <- methods::new("role", stringr::str_to_title(trimws(personinfo[info_row,"role"])))
        associated_party@role <- methods::new("role", c(role))
        associated_party
        
      }

    }

  }

  # Read personnel file

  personinfo <- x$template$personnel.txt$content
  
  personinfo$role <- tolower(personinfo$role)
  
  personinfo <- validate_personnel(x = personinfo)
  
  # Build modules--------------------------------------------------------------

  message("Building EML ...")
  
  # Create EML
  
  # Build eml-access module ---------------------------------------------------
  
  message("<access>")
  
  # Set default user.id and user.domain, if not defined
  
  if (is.null(user.id)){
    warning('No "user.id" was supplied. The default "someuserid" will be used.')
    user.id <- 'someuserid'
  } else if (is.null(user.domain)){
    warning('No "user.domain" was supplied. The default "user.domain" will be used.')
    user.domain <- 'someuserdomain'
  }
  
  # Build access
  
  list_of_allow_principals <- list()
  list_of_allow_permissions <- list()
  for (i in 1:length(user.id)){
    if (user.domain[i] == 'LTER'){
      list_of_allow_principals[[i]] <- c(
        paste0(
          'uid=',
          user.id[i],
          ',o=',
          user.domain[i],
          ',dc=ecoinformatics,dc=org'
        ),
        "public"
      )
    } else if (user.domain[i] == 'EDI'){
      list_of_allow_principals[[i]] <- c(
        paste0(
          'uid=',
          user.id[i],
          ',o=',
          user.domain[i],
          ',dc=edirepository,dc=org'
        ),
        "public"
      )
    }
    list_of_allow_permissions[[i]] <- c(
      "all",
      "read")
  }
  
  list_of_allow_principals[[(length(list_of_allow_principals)+1)]] <- c("public")
  list_of_allow_permissions[[(length(list_of_allow_permissions)+1)]] <- c("read")
  
  access_order <- "allowFirst"
  
  access_scope <- "document"
  
  access <- methods::new("access",
                scope = access_scope,
                order = access_order,
                authSystem = "https://pasta.edirepository.org/authentication")
  
  allow <- list()
  for (i in 1:length(list_of_allow_principals)){
    allow[[i]] <- methods::new("allow",
                      principal = list_of_allow_principals[[i]][1],
                      permission = list_of_allow_permissions[[i]][1])
  }
  
  access@allow <- methods::new("ListOfallow",
                      c(allow))

  # Build dataset module ------------------------------------------------------
  
  message("<dataset>")

  dataset <- methods::new("dataset",
                 title = dataset.title)

  # Add creators --------------------------------------------------------------
  
  message("<creators>")

  useI <- which(personinfo$role == "creator")

  creator_list <- list()
  for (j in 1:length(useI)){
    creator_list[[j]] <- set_person(info_row = useI[j],
                                    person_role = "creator")
  }

  dataset@creator <- methods::as(creator_list, "ListOfcreator")

  # Add publicaton date -------------------------------------------------------
  
  message("<publicationDate>")

  dataset@pubDate <- methods::as(format(Sys.time(), "%Y-%m-%d"), "pubDate")

  # Add abstract --------------------------------------------------------------
  
  message("<abstract>")
  
  if (!'abstract.txt' %in% names(x$template)){
    stop("abstract.txt doesn't exist!")
  }

  dataset@abstract <- x$template$abstract.txt$content
    
  # Add keywords --------------------------------------------------------------
  
  message("<keywordSet>")
  
  if (!'keywords.txt' %in% names(x$template)){
    stop('keywords.txt does not exist! Run import_templates.txt to regenerate this template.')
  }

  keywords <- x$template$keywords.txt$content
  
  # Edit keywords: Remove blank keyword entries
  
  use_i <- keywords$keyword == ""
  if (sum(use_i) > 0){
    keywords <- keywords[!use_i, ]
  } 
  
  if (sum(keywords$keywordThesaurus == '') > 0){
    # Try resolving keywords without a listed thesaurus to the LTER Controlled 
    # Vocabulary
    
    unresolved_terms <- keywords[keywords$keywordThesaurus == '', 'keyword']
    
    results <- try(
      EDIutils::vocab_resolve_terms(
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
      keywordSet[[k]] <- methods::as(kws[k], "keyword")
    }
    list_keywordSet[[i]] <- methods::new("keywordSet",
                           keywordSet,
                           keywordThesaurus = uni_keywordThesaurus[i])
  }
  dataset@keywordSet <- methods::as(list_keywordSet, "ListOfkeywordSet")

  # Add intellectual rights

  message("<intellectualRights>")
  
  if (!'intellectual_rights.txt' %in% names(x$template)){
    stop("intellectual_rights.txt doesn't exist!")
  }
  
  dataset@intellectualRights <- x$template$intellectual_rights.txt$content

  # Add coverage --------------------------------------------------------------
  
  message("<temporalCoverage>")
  
  dataset@coverage <- EML::set_coverage(
    begin = temporal.coverage[1],
    end = temporal.coverage[2]
  )
  
  message("<geographicCoverage>")
  
  list_of_coverage <- list()
  
  # Add coverage defined in arguments of make_eml
  
  if (!missing(geographic.coordinates) & !missing(geographic.description)){
    geographic_description <- methods::new("geographicDescription", geographic.description)
    bounding_coordinates <- methods::new("boundingCoordinates",
                                westBoundingCoordinate = as.character(geographic.coordinates[4]),
                                eastBoundingCoordinate = as.character(geographic.coordinates[2]),
                                northBoundingCoordinate = as.character(geographic.coordinates[1]),
                                southBoundingCoordinate = as.character(geographic.coordinates[3]))
    geographic_coverage <- methods::new("geographicCoverage",
                               geographicDescription = geographic_description,
                               boundingCoordinates = bounding_coordinates)
    list_of_coverage[[(length(list_of_coverage)+1)]] <- geographic_coverage
  }
  
  # Add coverage defined in bounding_boxes.txt
  
  if ('bounding_boxes.txt' %in% names(x$template)){
    
    bounding_boxes <- x$template$bounding_boxes.txt$content
    
    if (nrow(bounding_boxes) != 0){
      
      bounding_boxes <- lapply(bounding_boxes, as.character)
      
      for (i in 1:length(bounding_boxes$geographicDescription)){
        
        geographic_description <- methods::new("geographicDescription", bounding_boxes$geographicDescription[i])
        bounding_coordinates <- methods::new("boundingCoordinates",
                                    westBoundingCoordinate = bounding_boxes$westBoundingCoordinate[i],
                                    eastBoundingCoordinate = bounding_boxes$eastBoundingCoordinate[i],
                                    northBoundingCoordinate = bounding_boxes$northBoundingCoordinate[i],
                                    southBoundingCoordinate = bounding_boxes$southBoundingCoordinate[i])
        geographic_coverage <- methods::new("geographicCoverage",
                                   geographicDescription = geographic_description,
                                   boundingCoordinates = bounding_coordinates)
        list_of_coverage[[(length(list_of_coverage)+1)]] <- geographic_coverage
        
      }
      
      dataset@coverage@geographicCoverage <- methods::as(list_of_coverage, "ListOfgeographicCoverage")
      
    } 
    
  }
  
  dataset@coverage@geographicCoverage <- methods::as(list_of_coverage, "ListOfgeographicCoverage")
  
  # Add taxonomicCoverage -----------------------------------------------------
  
  if ('taxonomicCoverage.xml' %in% names(x$template)){
    
    if (class(x$template$taxonomicCoverage.xml$content)[1] == 'taxonomicCoverage'){
      
      message("<taxonomicCoverage>")
      
      taxonomic_coverage <- x$template$taxonomicCoverage.xml$content
      
      dataset@coverage@taxonomicCoverage <- methods::as(list(taxonomic_coverage), "ListOftaxonomicCoverage")
      
    }
    
  }

  # Add maintenance -----------------------------------------------------------
  
  message("<maintenance>")
  
  maintenance <- methods::new("maintenance")
  maintenance@description <- methods::as(maintenance.description, "description")
  dataset@maintenance <- maintenance
  
  # Add contacts --------------------------------------------------------------
  
  message("<contacts>")

  useI <- which(personinfo$role == "contact")

  contact_list <- list()
  for (j in 1:length(useI)){
    contact_list[[j]] <- set_person(info_row = useI[j],
                                    person_role = "contact")
  }

  dataset@contact <- methods::as(contact_list, "ListOfcontact")

  # Add methods ---------------------------------------------------------------
  
  message("<methods>")
  
  if (!'methods.txt' %in% names(x$template)){
    stop("methods.txt doesn't exist!")
  }

  dataset@methods <- x$template$methods.txt$content
  
  if ('geographic_coverage.txt' %in% names(x$template)){
    
    df_geographic_coverage <- x$template$geographic_coverage.txt$content
    
    if (nrow(df_geographic_coverage) != 0){
      
      message("<geographicDescription>")
      
      list_of_coverage <- list()
      
      for (i in 1:dim(df_geographic_coverage)[1]){
        
        coverage <- methods::new("coverage")
        
        geographic_description <- methods::new("geographicDescription", df_geographic_coverage$site[i])
        
        bounding_coordinates <- methods::new("boundingCoordinates",
                                             westBoundingCoordinate = df_geographic_coverage$longitude[i],
                                             eastBoundingCoordinate = df_geographic_coverage$longitude[i],
                                             northBoundingCoordinate = df_geographic_coverage$latitude[i],
                                             southBoundingCoordinate = df_geographic_coverage$latitude[i])
        
        geographic_coverage <- methods::new("geographicCoverage",
                                            geographicDescription = geographic_description,
                                            boundingCoordinates = bounding_coordinates)
        
        coverage@geographicCoverage <- methods::as(list(geographic_coverage), "ListOfgeographicCoverage")
        
        list_of_coverage[[i]] <- coverage
        
      }
      
      sampling <- methods::new("sampling")
      
      sampling@studyExtent@coverage <- methods::as(list_of_coverage, "ListOfcoverage")
      
      sampling@samplingDescription <- methods::as("Geographic coordinates of sampling sites", "samplingDescription")
      
      dataset@methods@sampling <- methods::as(list(sampling), "ListOfsampling")

    }

  }

  # Add provenance metadata -----------------------------------------------
  
  if (!is.null(provenance)){
    environment <- 'production'
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
        lib_path <- system.file('/examples/templates/abstract.txt', package = 'EMLassemblyline')
        lib_path <- substr(lib_path, 1, nchar(lib_path) - 32)
        if (!stringr::str_detect(stringr::str_replace_all(eml.path, '\\\\', '/'), lib_path)){
          xml2::write_xml(
            prov_metadata,
            paste0(eml.path,
                   '/provenance_metadata.xml')
          )
        }
        
        # Read provenance file and add to L0 EML
        prov_metadata <- EML::read_eml(paste0(eml.path, '/provenance_metadata.xml'))
        methods_step <- dataset@methods@methodStep
        methods_step[[length(methods_step)+1]] <- prov_metadata
        dataset@methods@methodStep <- methods_step
        
        # Delete provenance_metadata.xml (a temporary file)
        lib_path <- system.file('/examples/templates/abstract.txt', package = 'EMLassemblyline')
        lib_path <- substr(lib_path, 1, nchar(lib_path) - 32)
        if (!stringr::str_detect(stringr::str_replace_all(eml.path, '\\\\', '/'), lib_path)){
          file.remove(
            paste0(eml.path, '/provenance_metadata.xml')
          )
        }
      } else {
        message('Unable to get provenance metadata.')
      }
    }
  }
  
  # Add project and funding ---------------------------------------------------
  
  useI <- which(personinfo$role == "pi")
  
  if (!identical(useI, integer(0))){
    
    message("<project>")
    
    pi_list <- list()
    pi_list[[1]] <- suppressWarnings(set_person(info_row = useI[1],
                                                person_role = "pi"))
    
    if (personinfo$projectTitle[useI[1]] == ""){
      if (personinfo$fundingAgency[useI[1]] == ""){
        if (personinfo$fundingNumber[useI[1]] == ""){
          project <- methods::new("project",
                         title = "No project title to report",
                         personnel = pi_list,
                         funding = "No funding to report")
        } else if (personinfo$fundingNumber[useI[1]] != ""){
          project <- methods::new("project",
                         title = "No project title to report",
                         personnel = pi_list,
                         funding = personinfo$fundingNumber[useI[1]])
        }
      } else if (personinfo$fundingAgency[useI[1]] != ""){
        if (personinfo$fundingNumber[useI[1]] == ""){
          project <- methods::new("project",
                         title = "No project title to report",
                         personnel = pi_list,
                         funding = personinfo$fundingAgency[useI[1]])
        } else if (personinfo$fundingNumber[useI[1]] != ""){
          project <- methods::new("project",
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
          project <- methods::new("project",
                         title = personinfo$projectTitle[useI[1]],
                         personnel = pi_list,
                         funding = "No funding to report")
        } else if (personinfo$fundingNumber[useI[1]] != ""){
          project <- methods::new("project",
                         title = personinfo$projectTitle[useI[1]],
                         personnel = pi_list,
                         funding = personinfo$fundingNumber[useI[1]])
        }
      } else if (personinfo$fundingAgency[useI[1]] != ""){
        if (personinfo$fundingNumber[useI[1]] == ""){
          project <- methods::new("project",
                         title = personinfo$projectTitle[useI[1]],
                         personnel = pi_list,
                         funding = personinfo$fundingAgency[useI[1]])
        } else if (personinfo$fundingNumber[useI[1]] != ""){
          project <- methods::new("project",
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
              relatedProject_list[[i]] <- methods::new("relatedProject",
                                              title = "No project title to report",
                                              personnel = pi_list,
                                              funding = "No funding to report")
            } else if (personinfo$fundingNumber[useI[i+1]] != ""){
              relatedProject_list[[i]] <- methods::new("relatedProject",
                                              title = "No project title to report",
                                              personnel = pi_list,
                                              funding = personinfo$fundingNumber[useI[i+1]])
            }
          } else if (personinfo$fundingAgency[useI[i+1]] != ""){
            if (personinfo$fundingNumber[useI[i+1]] == ""){
              relatedProject_list[[i]] <- methods::new("relatedProject",
                                              title = "No project title to report",
                                              personnel = pi_list,
                                              funding = personinfo$fundingAgency[useI[i+1]])
            } else if (personinfo$fundingNumber[useI[i+1]] != ""){
              relatedProject_list[[i]] <- methods::new("relatedProject",
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
              relatedProject_list[[i]] <- methods::new("relatedProject",
                                              title = personinfo$projectTitle[useI[i+1]],
                                              personnel = pi_list,
                                              funding = "No funding to report")
            } else if (personinfo$fundingNumber[useI[i+1]] != ""){
              relatedProject_list[[i]] <- methods::new("relatedProject",
                                              title = personinfo$projectTitle[useI[i+1]],
                                              personnel = pi_list,
                                              funding = personinfo$fundingNumber[useI[i+1]])
            }
          } else if (personinfo$fundingAgency[useI[i+1]] != ""){
            if (personinfo$fundingNumber[useI[i+1]] == ""){
              relatedProject_list[[i]] <- methods::new("relatedProject",
                                              title = personinfo$projectTitle[useI[i+1]],
                                              personnel = pi_list,
                                              funding = personinfo$fundingAgency[useI[i+1]])
            } else if (personinfo$fundingNumber[useI[i+1]] != ""){
              relatedProject_list[[i]] <- methods::new("relatedProject",
                                              title = personinfo$projectTitle[useI[i+1]],
                                              personnel = pi_list,
                                              funding = paste0(personinfo$fundingAgency[useI[i+1]],
                                                               ": ",
                                                               personinfo$fundingNumber[useI[i+1]]))
            }
          }
        }
      }
      dataset@project@relatedProject <- methods::as(relatedProject_list, "ListOfrelatedProject")
    }
    
  }
  
  # Add associated parties ----------------------------------------------------
  
  useI <- which(personinfo$role != "pi" &
                  personinfo$role != "creator" &
                  personinfo$role != "contact")
  
  if (length(useI) != 0){
    message("<associatedParty>")
    associated_party_list <- list()
    for (j in 1:length(useI)){
      associated_party_list[[j]] <- suppressWarnings(set_person(info_row = useI[j],
                                               person_role = ""))
    }
    dataset@associatedParty <- methods::as(associated_party_list, "ListOfassociatedParty")
  }
  
  # Add additional information ------------------------------------------------
  
  if ('additional_info.txt' %in% names(x$template)){
    
    message("<additionalInfo>")
    
    additional_info <- x$template$additional_info.txt$content
    
    dataset@additionalInfo <- methods::as(list(additional_info), "ListOfadditionalInfo")
    
  }

  # Loop through tables -------------------------------------------------------
  
  if (!is.null(x$data.table)){
    for (i in 1:length(names(x$data.table))){
      
      message(paste(
        "<dataTable> ...",
        names(x$data.table)[i]))
      
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
        
        # Validate catvars: Definitions for each code
        
        use_i <- sum(catvars$attributeName != "") ==  sum(catvars$definition != "")
        
        if (!isTRUE(use_i)){
          hold <- catvars$code[catvars$definition == ""]
          stop(paste(fname_table_catvars, 
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
        
        attributeList <- suppressWarnings(EML::set_attributes(attributes,
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
        
        attributeList <- suppressWarnings(EML::set_attributes(attributes,
                                                         col_classes = col_classes))
        
      }
      
      # Set physical
      
      eol <- EDIutils::get_eol(
        path = x$data.table[[(names(x$data.table)[i])]]$path,
        file.name = names(x$data.table)[i],
        os = EDIutils::detect_os()
      )
      
      if (!is.null(data.table.quote.character)){
        
        physical_temp <- EML::set_physical(
          paste0(
            x$data.table[[(names(x$data.table)[i])]]$path,
            '/',
            names(x$data.table)[i]
          )
        )
        
        physical <- EML::set_physical(names(x$data.table)[i],
                                 numHeaderLines = "1",
                                 recordDelimiter = eol,
                                 attributeOrientation = "column",
                                 fieldDelimiter = unlist(EML::eml_get(physical_temp, 'fieldDelimiter')),
                                 quoteCharacter = data.table.quote.character[i])
        
      }
      
      if (is.null(data.table.quote.character)) {
        
        physical_temp <- EML::set_physical(
          paste0(
            x$data.table[[(names(x$data.table)[i])]]$path,
            '/',
            names(x$data.table)[i]
          )
        )
        
        physical <- EML::set_physical(names(x$data.table)[i],
                                 numHeaderLines = "1",
                                 recordDelimiter = eol,
                                 attributeOrientation = "column",
                                 fieldDelimiter = unlist(EML::eml_get(physical_temp, 'fieldDelimiter')))
        
      }
      
      
      
      physical@size <- methods::new("size",
                           unit = "byte",
                           as.character(
                             file.size(
                               paste(x$data.table[[(names(x$data.table)[i])]]$path,
                                     "/",
                                     names(x$data.table)[i],
                                     sep = ""))))
      
      if (!is.null(data.url)){
        data_table_url <- paste(data.url,
                                "/",
                                names(x$data.table)[i],
                                sep = "")
        distribution <- methods::new("distribution",
                            online = methods::new("online",
                                         url = data_table_url))
        physical@distribution <- methods::new("ListOfdistribution",
                                     c(distribution))
      }
      
      
      if (EDIutils::detect_os() == "mac"){
        
        command_certutil <- paste("md5 ",
                                  "\"",
                                  x$data.table[[(names(x$data.table)[i])]]$path,
                                  "/",
                                  names(x$data.table)[i],
                                  "\"",
                                  sep = "")
        
        certutil_output <- system(command_certutil, intern = T)
        
        checksum_md5 <- gsub(".*= ", "", certutil_output)
        
        authentication <- methods::new("authentication",
                              method = "MD5",
                              checksum_md5)
        
        physical@authentication <- methods::as(list(authentication),
                                      "ListOfauthentication")
        
      } else if (EDIutils::detect_os() == "win"){
        
        command_certutil <- paste("CertUtil -hashfile ",
                                  "\"",
                                  x$data.table[[(names(x$data.table)[i])]]$path,
                                  "\\",
                                  names(x$data.table)[i],
                                  "\"",
                                  " MD5",
                                  sep = "")
        
        certutil_output <- system(command_certutil, intern = T)
        
        checksum_md5 <- gsub(" ", "", certutil_output[2])
        
        authentication <- methods::new("authentication",
                              method = "MD5",
                              checksum_md5)
        
        physical@authentication <- methods::as(list(authentication),
                                      "ListOfauthentication")
        
      } else if (EDIutils::detect_os() == "lin"){
        
        command_certutil <- paste0("md5sum ",
                                   "\"",
                                   x$data.table[[(names(x$data.table)[i])]]$path,
                                   "/",
                                   names(x$data.table)[i],
                                   "\"")
        
        certutil_output <- system(command_certutil, intern = T)
        
        checksum_md5 <- strsplit(certutil_output, split = " ")[[1]][1]
        
        authentication <- methods::new("authentication",
                              method = "MD5",
                              checksum_md5)
        
        physical@authentication <- methods::as(list(authentication),
                                      "ListOfauthentication")

      }
      
      # Get number of records
      
      number_of_records <- as.character(dim(df_table)[1])
      
      # Pull together information for the data table
      
      data_table <- methods::new("dataTable",
                        entityName = data.table.description[i],
                        entityDescription = data.table.description[i],
                        physical = physical,
                        attributeList = attributeList,
                        numberOfRecords = number_of_records)
      
      data_tables_stored[[i]] <- data_table
      
    }
    
    # Compile datatables --------------------------------------------------------
    
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
          
          message("<additionalMetadata>")
          
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
    
    dataset@dataTable <- methods::new("ListOfdataTable",
                             data_tables_stored)
    
  }
  
  # Add other.entity ----------------------------------------------------------
  
  if (!is.null(x$other.entity)){
    
    if (length(names(x$other.entity)) > 0){
      
      list_of_other_entity <- list()
      
      for (i in 1:length(other.entity)){
        
        message(paste0("<otherEntity> ... ", names(x$other.entity)[i]))
        
        # Create new other entity element
        
        other_entity <- methods::new("otherEntity")
        
        # Add code file names
        
        other_entity@entityName <- other.entity.description[i]
        
        # Add description
        
        description <- other.entity.description[i]
        
        other_entity@entityDescription <- description
        
        #  Build physical
        
        physical <- methods::new("physical",
                                 objectName = names(x$other.entity)[i])
        
        format_name <- "unknown"
        entity_type <- "unknown"
        physical@dataFormat@externallyDefinedFormat@formatName <- format_name
        
        physical@size <- methods::new("size", unit = "bytes", methods::as(as.character(file.size(paste(x$other.entity[[i]]$path, "/", names(x$other.entity)[i], sep = ""))), "size"))
        
        if (!is.null(data.url)){
          
          physical@distribution <- methods::new(
            'ListOfdistribution',
            c(
              methods::new(
                "distribution",
                online = methods::new(
                  "online",
                  url = paste0(
                    data.url,
                    '/',
                    names(x$other.entity)[i]
                  )
                )
              )
            )
          )
          
        }

        if (EDIutils::detect_os() == "mac"){
          
          command_certutil <- paste("md5 ",
                                    "\"",
                                    x$other.entity[[i]]$path,
                                    "/",
                                    names(x$other.entity)[i],
                                    "\"",
                                    sep = "")
          
          certutil_output <- system(command_certutil, intern = T)
          
          checksum_md5 <- gsub(".*= ", "", certutil_output)
          
          authentication <- methods::new("authentication",
                                method = "MD5",
                                checksum_md5)
          
          physical@authentication <- methods::as(list(authentication),
                                        "ListOfauthentication")
          
        } else if (EDIutils::detect_os() == "win"){
          
          command_certutil <- paste("CertUtil -hashfile ",
                                    "\"",
                                    x$other.entity[[i]]$path,
                                    "\\",
                                    names(x$other.entity)[i],
                                    "\"",
                                    " MD5",
                                    sep = "")
          
          certutil_output <- system(command_certutil, intern = T)
          
          checksum_md5 <- gsub(" ", "", certutil_output[2])
          
          authentication <- methods::new("authentication",
                                method = "MD5",
                                checksum_md5)
          
          physical@authentication <- methods::as(list(authentication),
                                        "ListOfauthentication")
          
        } else if (EDIutils::detect_os() == "lin"){
          
          command_certutil <- paste0("md5sum ",
                                     "\"",
                                     x$other.entity[[i]]$path,
                                     "/",
                                     names(x$other.entity)[i],
                                     "\"")
          
          certutil_output <- system(command_certutil, intern = T)
          
          checksum_md5 <- strsplit(certutil_output, split = " ")[[1]][1]
          
          authentication <- methods::new("authentication",
                                method = "MD5",
                                checksum_md5)
          
          physical@authentication <- methods::as(list(authentication),
                                        "ListOfauthentication")
          
          
          
        }
        
        other_entity@physical <- methods::as(c(physical), "ListOfphysical")
        
        # Add entity type
        
        other_entity@entityType <- entity_type
        
        # Add other entity to list
        
        list_of_other_entity[[i]] <- other_entity
        
      }
      
      dataset@otherEntity <- methods::new("ListOfotherEntity",
                                          list_of_other_entity)
      
    }
    
    
  }
  
  
  
  if (is.null(package.id)){
    package.id <- 'edi.101.1'
  }
  
  # Build EML -----------------------------------------------------------------
  
  message("Compiling EML.")
  
  if (exists('custom_units')){
    if (custom_units == "yes"){
      eml <- methods::new("eml",
                 schemaLocation = "eml://ecoinformatics.org/eml-2.1.1  http://nis.lternet.edu/schemas/EML/eml-2.1.1/eml.xsd",
                 packageId = package.id,
                 system = "edi",
                 access = access,
                 dataset = dataset,
                 additionalMetadata = methods::as(unitsList, "additionalMetadata"))
    } else {
      eml <- methods::new("eml",
                 schemaLocation = "eml://ecoinformatics.org/eml-2.1.1  http://nis.lternet.edu/schemas/EML/eml-2.1.1/eml.xsd",
                 packageId = package.id,
                 system = "edi",
                 access = access,
                 dataset = dataset)
    }
  } else {
    eml <- methods::new("eml",
               schemaLocation = "eml://ecoinformatics.org/eml-2.1.1  http://nis.lternet.edu/schemas/EML/eml-2.1.1/eml.xsd",
               packageId = package.id,
               system = "edi",
               access = access,
               dataset = dataset)
  }

  # Write EML
  
  if (isTRUE(write.file)){
    message("Writing EML.")
    EML::write_eml(eml, paste(eml.path, "/", package.id, ".xml", sep = ""))
  }
  
  # Validate EML
  
  message("Validating EML.")
  
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
  
  path <- x$template[[1]]$path
  data.path <- x$data.table[[1]]$path
  data.table <- names(x$data.table)
  
  # Get names of data files with associated attribute files
  
  files <- list.files(data.path)
  use_i <- stringr::str_detect(string = files,
                      pattern = "^attributes")
  attribute_files <- files[use_i]
  fname_table_attributes <- attribute_files
  table_names_base <- stringr::str_sub(string = attribute_files,
                              start = 12,
                              end = nchar(attribute_files)-4)
  use_i <- stringr::str_detect(string = files,
                      pattern = stringr::str_c("^", table_names_base, collapse = "|"))
  table_names <- files[use_i]
  
  # Synchronize ordering of data files and attribute files
  
  table_names <- EDIutils::validate_file_names(data.path, data.table)
  
  attribute_files_out <- c()
  for (i in 1:length(table_names)){
    attribute_files_out[i] <- paste0('attributes_',
                                     stringr::str_c(stringr::str_sub(table_names[i], 1, nchar(table_names[i])-4), collapse = "|"),
                                     '.txt')
  }
  fname_table_attributes <- attribute_files_out
  
  
  # Loop through data tables --------------------------------------------------
  
  attributes_stored <- list()
  
  for (i in 1:length(table_names)){
    
    message(paste("Compiling", fname_table_attributes[i]))
    
    
    file_path <- paste(data.path,
                       "/",
                       table_names[i],
                       sep = "")
    
    delim_guess <- EDIutils::detect_delimeter(path = data.path, data.files = table_names[i], os = EDIutils::detect_os())
    
    df_table <- utils::read.table(file_path,
                           header = TRUE,
                           sep = delim_guess,
                           quote = "\"",
                           as.is = TRUE,
                           comment.char = "")
    
    # Read attributes_datatablename
    
    field_count <- utils::count.fields(file = paste0(path,
                                              "/",
                                              fname_table_attributes[i]),
                                sep = "\t")
    
    if (!is.na((sum(field_count) > 0)) & (sum(field_count > 7) > 0)){
      stop(paste0('Some of the information in "',
                  fname_table_attributes[i],
                  '" is not in the correct columns. ',
                  "Please double check the organization of content in this file."))
    }
    
    df_attributes <- utils::read.table(paste(path,
                                      "/",
                                      fname_table_attributes[i],
                                      sep = ""),
                                header = T,
                                sep="\t",
                                quote="\"",
                                as.is=TRUE,
                                comment.char = "",
                                fill = T,
                                colClasses = rep("character", 7))
    df_attributes <- df_attributes[ ,1:7]
    colnames(df_attributes) <- c("attributeName",
                                 "attributeDefinition",
                                 "class",
                                 "unit",
                                 "dateTimeFormatString",
                                 "missingValueCode",
                                 "missingValueCodeExplanation")
    
    
    
    # Convert user inputs to consistent case
    
    df_attributes$class <- tolower(df_attributes$class)
    
    # Validate attributes -----------------------------------------------------
    
    # Validate attributes: Remaining prompts in units field
    
    df_attributes$unit[is.na(df_attributes$unit)] <- ""
    
    use_i <- stringr::str_detect(string = df_attributes$unit,
                        pattern = "^!.*!$")
    
    if (sum(use_i) > 0){
      hold <- df_attributes$attributeName[use_i]
      stop(paste(fname_table_attributes[i], 
                 " contains invalid units. Please edit the units of these attributes: ",
                 paste(hold, collapse = ", ")))
    }
    
    # Validate attributes: Remaining prompts in dateTimeFormatString field
    
    df_attributes$dateTimeFormatString[is.na(df_attributes$dateTimeFormatString)] <- ""
    
    use_i <- stringr::str_detect(string = df_attributes$dateTimeFormatString,
                        pattern = "^!.*!$")
    
    if (sum(use_i) > 0){
      hold <- df_attributes$attributeName[use_i]
      stop(paste(fname_table_attributes[i], 
                 " contains invalid dateTimeFormatString(s). Please edit the dateTimeFormatString(s) of these attributes: ",
                 paste(hold, collapse = ", ")))
    }
    
    # Validate attributes: Each attribute has a definition
    
    use_i <- sum(df_attributes$attributeName != "") ==  sum(df_attributes$attributeDefinition != "")
    
    if (!isTRUE(use_i)){
      hold <- df_attributes$attributeName[df_attributes$attributeDefinition == ""]
      stop(paste(fname_table_attributes[i], 
                 " contains attributes without defintions. Please add definitions to these attributes: ",
                 paste(hold, collapse = ", ")))
    }
    
    # Validate attributes: Each attribute has a class
    
    use_i <- sum(df_attributes$attributeName != "") ==  sum(df_attributes$class != "")
    
    if (!isTRUE(use_i)){
      hold <- df_attributes$attributeName[df_attributes$class == ""]
      stop(paste(fname_table_attributes[i], 
                 " contains attributes without a class. Please add a class to these attributes: ",
                 paste(hold, collapse = ", ")))
    }
    
    # Validate attributes: Each Date class has an entry
    
    use_i <- df_attributes$class == "date"
    use_i2 <- df_attributes$dateTimeFormatString != ""
    use_i3 <- use_i2 == use_i
    
    if (sum(!use_i3) > 0){
      hold <- df_attributes$attributeName[!use_i3]
      stop(paste(fname_table_attributes[i], 
                 " has Date attributes without a dateTimeFormatString. Please add format strings to these attributes: ",
                 paste(hold, collapse = ", ")))
    }
    
    # Validate attributes: class == numeric has non-blank units
    
    use_i <- df_attributes$class == "numeric"
    use_i2 <- df_attributes$unit != ""
    use_i3 <- use_i2 == use_i
    
    if (sum(use_i3) < length(use_i3)){
      hold <- df_attributes$attributeName[!use_i3]
      stop(paste(fname_table_attributes[i], 
                 " has numeric attributes without units. Please add units to these attributes: ",
                 paste(hold, collapse = ", ")))
    }
    
    # Validate attributes: missingValueCodes have missingValueCodeExplanations
    
    use_i <- df_attributes$missingValueCode %in% ""
    use_i2 <- df_attributes$missingValueCodeExplanation == ""
    use_i3 <- use_i2 != use_i
    
    if (sum(is.na(use_i3)) > 0){
      stop(paste(fname_table_attributes[i], 
                 " missingValueCodeExplanation(s) are absent for the missingValueCodes you have entered. Please fix this.",
                 "NA's listed in the missingValueCode column are interpreted as missingValueCodes and are expecting explanation."))
    }
    
    if (sum(use_i3) > 0){
      hold <- df_attributes$attributeName[use_i3]
      stop(paste(fname_table_attributes[i], 
                 " has missingValueCode(s) without missingValueCodeExplanation(s). Please fix this for these attributes: ",
                 paste(hold, collapse = ", ")))
    }
    
    # Validate attributes: missingValueCodeExplanations have non-blank missingValuecodeExplanations
    
    use_i <- df_attributes$missingValueCodeExplanation != ""
    use_i2 <- df_attributes$missingValueCode %in% ""
    use_i3 <- use_i2 == use_i
    
    if (sum(use_i3) > 0){
      hold <- df_attributes$attributeName[use_i3]
      stop(paste(fname_table_attributes[i], 
                 " has blank missingValueCode(s). Blank missing value codes are not allowed. Please fix your data and metadata for these attributes: ",
                 paste(hold, collapse = ", ")))
    }
    
    # Validate attributes: missingValueCodes only have 1 entry per column
    
    vec <- df_attributes$missingValueCode
    vec[is.na(df_attributes$missingValueCode)] <- "NA"
    use_i <- stringr::str_count(vec, '[,]|[\\s]') > 0
    if (sum(use_i) > 0){
      hold <- df_attributes$attributeName[use_i]
      stop(paste0(fname_table_attributes[i], 
                  ' has more than one missingValueCode.', 
                  '\nOnly one missingValueCode per attribute is allowed.', 
                  'Please fix your data and metadata for these attributes:\n',
                  paste(hold, collapse = ", ")))
    }
    
    
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
    
    # Set attribute names
    
    if (length(attributes$attributeName) != length(df_attributes$attributeName)){
      stop(
        paste(
          'The number of attributes listed in ',
          fname_table_attributes[i],
          ' does not match the number of columns listed in the corresponding data table. Please correct this. '
        )
      )
    }
    
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
        
        if ((class(raw) == "character") | (class(raw) == "factor")){
          stop(paste0('Characters strings found in the column "',
                      colnames(df_table)[is_numeric[j]],
                      '" of the file "',
                      table_names[i],
                      '". ',
                      'Please remove these non-numeric characters and try again.'))
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

