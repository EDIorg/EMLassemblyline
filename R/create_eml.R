#' Create EML, then validate and write to file
#'
#' @description  A function to translate user supplied metadata into the EML 
#'     schema, validate the EML, and write to file.
#'
#' @usage create_eml(path)
#'
#' @param path A path to the dataset working directory containing the 
#'     completed metadata templates, \emph{eml_configuration.R}, 
#'     \emph{datasetname_datatablename_attributes.xlsx}, 
#'     \emph{datasetname_datatablename_factors.xlsx} (if factors are present), 
#'     and \emph{geographic_coverage.xlsx} (if reporting detailed geographic 
#'     coverage).
#'
#' @return 
#'     Validation results printed to the \emph{Console}.
#'     
#'     An EML metadata file in the dataset working directory titled 
#'     \emph{packageID.xml}.
#'     
#' @details 
#'     If validation fails, open the EML document in a .xml editor to identify 
#'     the source of error. Often the error is small and quickly resolved with 
#'     the aid of an editors schema congruence functionality.
#'
#' @seealso \code{\link{copy_templates}} to copy metadata templates to the 
#'     dataset working directory.
#' @seealso \code{\link{run_guide}} for guidance on completing the template 
#'     files.
#' @seealso \code{\link{write_attributes}} to compile and write the attributes 
#'     table.
#' @seealso \code{\link{write_factors}} to create the factors table if the 
#'     attributes table contains factors.
#' @seealso \code{\link{extract_geocoverage}} to extract detailed geographic 
#'     coordinates of sampling sites.


create_eml <- function(path) {
  
  # Parameterize function
  
  library("EML")
  library("xlsx")
  #library("rmarkdown")
  library("methods")
  library("stringr")
  library("tools")
  options(java.parameters = "-Xmx4000m")  # Allocate RAM to java
  
  # Get system information
  
  sysinfo <- Sys.info()["sysname"]
  if (sysinfo == "Darwin"){
    os <- "mac"
  } else {
    os <- "win"
  }

  # Parameterize function -----------------------------------------------------

  # Load the datasets configuration file

  source(paste(path, "/eml_configuration.R", sep = ""))
  
  template <- paste(dataset_name,
                    "_template.docx",
                    sep = "")

  # Set file names

  fname_abstract <- paste(path,
                          "/",
                          substr(template, 1, nchar(template) - 14),
                          "_abstract.docx",
                          sep = "")
  
  fname_additional_info <- paste(path,
                            "/",
                            substr(template, 1, nchar(template) - 14),
                            "_additional_info.docx",
                            sep = "")

  fname_personnel <- paste(path,
                           "/",
                          substr(template, 1, nchar(template) - 14),
                          "_personnel.xlsx",
                          sep = "")
  
  fname_intellectual_rights <- paste(path,
                                     "/",
                                     substr(template, 1,
                                            nchar(template) - 14),
                                     "_intellectual_rights.docx", sep = "")

  fname_methods <- paste(path,
                         "/",
                         substr(template, 1, nchar(template) - 14),
                         "_methods.docx",
                         sep = "")
  

  fname_table_attributes <- c()
  for (i in 1:length(table_names)){
    fname_table_attributes[i] <- paste(
      substr(table_names[i], 1, nchar(table_names[i]) - 4),
      "_attributes.xlsx",
      sep = "")
  }
  
  fname_table_factors <- trimws(
    list.files(path,
               pattern = "*_factors.xlsx"))

  # Initialize data entity storage (tables)

  data_tables_stored <- list()

  # Load helper function to set personnel roles

  set_person <- function(info_row, person_role){

    if (person_role == "contact"){

      individualName <- new(
        "individualName",
        givenName = trimws(personinfo[info_row,"givenName"]),
        surName = trimws(personinfo[info_row,"surName"]))

      contact <- new(
        "contact",
        individualName = individualName,
        organizationName = trimws(personinfo[info_row,"organizationName"]),
        electronicMailAddress = trimws(personinfo[info_row,"electronicMailAddress"]))

      contact

    } else if (person_role == "creator"){

      individualName <- new(
        "individualName",
        givenName = trimws(personinfo[info_row,"givenName"]),
        surName = trimws(personinfo[info_row,"surName"]))

      creator <- new(
        "creator",
        individualName = individualName,
        organizationName = trimws(personinfo[info_row,"organizationName"]),
        electronicMailAddress = trimws(personinfo[info_row,"electronicMailAddress"]))

      if (nchar(trimws(personinfo[info_row,"userId"])) == 19){
        userId <- new("userId")
        userId@directory <- new("xml_attribute", "http://orcid.org")
        userId@.Data <- trimws(personinfo[info_row,"userId"])
        creator@userId <- new("ListOfuserId", c(userId))
      }

      creator

    } else if (person_role == "pi"){

      individualName <- new(
        "individualName",
        givenName = trimws(personinfo[info_row,"givenName"]),
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
        rp_userId@directory <- new("xml_attribute", "http://orcid.org")
        rp_userId@.Data <- paste("http://orcid.org/",
                                 trimws(personinfo[info_row, "userId"]),
                                 sep = "")
        rp_personnel@userId <- new("ListOfuserId", c(rp_userId))

      }

      role <- new("role", "Principal Investigator")
      rp_personnel@role <- new("ListOfrole", c(role))

      rp_personnel

    } else {
      
      individualName <- new(
        "individualName",
        givenName = trimws(personinfo[info_row,"givenName"]),
        surName = trimws(personinfo[info_row,"surName"]))
      
      associated_party <- new(
        "associatedParty",
        individualName = individualName,
        organizationName = trimws(personinfo[info_row,"organizationName"]),
        electronicMailAddress = trimws(personinfo[info_row,"electronicMailAddress"]))
      
      if (nchar(trimws(personinfo[info_row,"userId"])) == 19){
        userId <- new("userId")
        userId@directory <- new("xml_attribute", "http://orcid.org")
        userId@.Data <- trimws(personinfo[info_row,"userId"])
        associated_party@userId <- new("ListOfuserId", c(userId))
      }
      
      role <- new("role", trimws(personinfo[info_row,"role"]))
      associated_party@role <- new("role", c(role))
      
      associated_party

    }

  }

  # Read/write personnel .xlsx

  personinfo <- read.xlsx2(
    fname_personnel,
    sheetIndex = 1,
    colClasses = c(rep("character",6)))

  colnames(personinfo) <- c("givenName",
                            "surName",
                            "organizationName",
                            "electronicMailAddress",
                            "userId",
                            "role")

  write.table(personinfo,
              paste(substr(fname_personnel, 1, nchar(fname_personnel) - 5),
                    ".txt",
                    sep = ""),
              sep = "\t",
              row.names = F,
              quote = F,
              fileEncoding = "UTF-8")


  # Build modules--------------------------------------------------------------

  # Build eml-access module

  allow_principals <- c(paste("uid=",
                              user_id,
                              ",o=LTER,dc=ecoinformatics,dc=org",
                              sep = ""),
                        "public")
  
  allow_permissions <- c("all",
                         "read") # order follows allow_principals
  
  access_order <- "allowFirst"
  
  access_scope <- "document"
  
  access <- new("access",
                scope = access_scope,
                order = access_order,
                authSystem = author_system)

  allow <- list()
  for (i in 1:length(allow_principals)){
    allow[[i]] <- new("allow",
                      principal = allow_principals[i],
                      permission = allow_permissions[i])
  }

  access@allow <- new("ListOfallow",
                      c(allow))

  # Build dataset module

  dataset <- new("dataset",
                 title = dataset_title)

  # Add creators

  personinfo <- read.table(
    paste(substr(fname_personnel, 1, nchar(fname_personnel) - 5),
          ".txt",
          sep = ""),
    header = TRUE,
    sep = "\t",
    as.is = TRUE,
    na.strings = "NA",
    colClasses = "character")

  useI <- which(personinfo$role == "creator")

  creator_list <- list()
  for (j in 1:length(useI)){
    creator_list[[j]] <- set_person(info_row = useI[j],
                                    person_role = "creator")
  }

  dataset@creator <- as(creator_list, "ListOfcreator")

  # Add publicaton date

  dataset@pubDate <- as(format(Sys.time(), "%Y-%m-%d"), "pubDate")

  # Add abstract

  dataset@abstract <- as(set_TextType(fname_abstract), "abstract")

  # Add keywords

  dataset@keywordSet <- new("ListOfkeywordSet", c(new("keywordSet", keywords)))

  # Add intellectual rights

  dataset@intellectualRights <- as(
    set_TextType(fname_intellectual_rights),
    "intellectualRights")

  # Add coverage
  
  dataset@coverage <- set_coverage(begin = begin_date,
                                   end = end_date,
                                   geographicDescription = geographic_location,
                                   west = coordinate_west,
                                   east = coordinate_east,
                                   north = coordinate_north,
                                   south = coordinate_south)

  # Add maintenance
  
  maintenance <- new("maintenance")
  maintenance@description <- as(maintenance_description, "description")
  dataset@maintenance <- maintenance
  
  # Add contacts

  personinfo <- read.table(
    paste(substr(fname_personnel, 1, nchar(fname_personnel) - 5),
          ".txt",
          sep = ""),
    header = TRUE,
    sep = "\t",
    as.is = TRUE,
    na.strings = "NA",
    colClasses = "character")

  useI <- which(personinfo$role == "contact")

  contact_list <- list()
  for (j in 1:length(useI)){
    contact_list[[j]] <- set_person(info_row = useI[j],
                                    person_role = "contact")
  }

  dataset@contact <- as(contact_list, "ListOfcontact")

  # Add methods

  dataset@methods <- set_methods(fname_methods)
  
  if (file.exists(paste(path, "/", "geographic_coverage.csv", sep = ""))){
    
    df_geographic_coverage <- read.table(paste(path,
                                               "/",
                                               "geographic_coverage.csv",
                                               sep = ""),
                                         sep = ",",
                                         header = TRUE,
                                         as.is = TRUE,
                                         na.strings = "NA")

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

  personinfo <- read.table(
    paste(substr(fname_personnel, 1, nchar(fname_personnel) - 5),
          ".txt",
          sep = ""),
    header = TRUE,
    sep = "\t",
    as.is = TRUE,
    na.strings = "NA",
    colClasses = "character")

  useI <- which(personinfo$role == "pi")

  pi_list <- list()
  for (j in 1:length(useI)){
    pi_list[[j]] <- set_person(info_row = useI[j],
                                    person_role = "pi")
  }

  project <- new("project",
                 title = funding_title,
                 personnel = pi_list,
                 funding = funding_grants)

  dataset@project <- project
  
  # Add associated parties
  
  personinfo <- read.table(
    paste(substr(fname_personnel, 1, nchar(fname_personnel) - 5),
          ".txt",
          sep = ""),
    header = TRUE,
    sep = "\t",
    as.is = TRUE,
    na.strings = "NA",
    colClasses = "character")
  
  useI <- which(personinfo$role != "pi" &
                  personinfo$role != "creator" &
                  personinfo$role != "contact")
  
  
  if (length(useI) != 0){
    associated_party_list <- list()
    for (j in 1:length(useI)){
      associated_party_list[[j]] <- set_person(info_row = useI[j],
                                               person_role = "")
    }
    dataset@associatedParty <- as(associated_party_list, "ListOfassociatedParty")
  }
  
  # Add additional information
  
  if (file.exists(fname_additional_info)){
    
    additional_info <- as(set_TextType(fname_additional_info), "additionalInfo")
    
    dataset@additionalInfo <- as(list(additional_info), "ListOfadditionalInfo")

  }
  

  # Loop through tables -------------------------------------------------------


  for (i in 1:length(table_names)){

    print(paste(
      "Now building attributes for ... ",
      table_names[i],
      sep = ""))
    
    # Read data table
    
    if (field_delimeter[i] == "comma"){
      
      df_table <- read.table(
        paste(path, "/", table_names[i], sep = ""),
        header=TRUE,
        sep=",",
        quote="\"",
        as.is=TRUE,
        comment.char = "")
      
    } else if (field_delimeter[i] == "tab"){
      
      df_table <- read.table(
        paste(path, "/", table_names[i], sep = ""),
        header=TRUE,
        sep="\t",
        quote="\"",
        as.is=TRUE,
        comment.char = "")
      
    }

    # Read attributes file (encoding necessitates read/write/read)
    
    attributes <- read.xlsx2(paste(
      path,
      "/",
      fname_table_attributes[i], sep = ""),
      sheetIndex = 1,
      colClasses = c(rep("character",7), rep("numeric",2),rep("character",2)),
      stringsAsFactors = F)

    write.table(
      attributes,
      paste(
        path,
        "/",
        substr(fname_table_attributes[i],
               1,
               nchar(fname_table_attributes[i]) - 5),
        ".txt",
        sep=""),
      sep = "\t",
      row.names = F,
      quote = F,
      fileEncoding = "UTF-8")

    attributes <- read.table(
      paste(
        path,
        "/",
        substr(fname_table_attributes[i],
               1,
               nchar(fname_table_attributes[i]) - 5),
        ".txt",
        sep = ""),
      header = TRUE,
      sep = "\t",
      as.is = TRUE,
    na.strings = "")
    for (j in 1:7){
    attributes[ ,j] <- as.character(attributes[ ,j])
    }
    for (j in 8:9){
    attributes[ ,j] <- as.numeric(attributes[ ,j])
    }
    for (j in 10:11){
    attributes[ ,j] <- as.character(attributes[ ,j])
    }

    # Read factors file (encoding necessitates read/write/read)

    if (!is.na(match(fname_table_factors[i], list.files(path)))){

      factors <- read.xlsx2(paste(
        path,
        "/",
        fname_table_factors[i], sep = ""),
        sheetIndex = 1,
        colClasses = c(rep("character",3)))

      if (dim(factors)[1] > 0){

        for (j in 1:dim(factors)[2]){
          factors[ ,j] <- as.character(factors[ ,j])
        }
        
        non_blank_rows <- nrow(factors) - sum(factors$attributeName == "")
        factors <- factors[1:non_blank_rows, 1:3]

        write.table(
          factors,
          paste(
            path,
            "/",
            substr(fname_table_factors[i],
                   1,
                   nchar(fname_table_factors[i]) - 5),
            ".txt",
            sep=""),
          sep = "\t",
          row.names = F,
          quote = F,
          fileEncoding = "UTF-8")

        factors <- read.table(
          paste(
            path,
            "/",
            substr(fname_table_factors[i],
                   1,
                   nchar(fname_table_factors[i]) - 5),
            ".txt",
            sep=""),
          header = TRUE,
          sep = "\t",
          as.is = TRUE,
          na.strings = "NA")

        # Clean extraneous white spaces from factors tables

        if (dim(factors)[1] != 0){
          for (j in 1:ncol(factors)){
            if (class(factors[ ,j]) == "character" ||
                (class(factors[ ,j]) == "factor")){
              factors[ ,j] <- trimws(factors[ ,j])
            }
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

    if (!is.na(match(fname_table_factors, list.files(path)))){

      if (dim(factors)[1] != 0){
        attributeList <- set_attributes(attributes,
                                        factors = factors,
                                        col_classes = col_classes)
      } else {
        attributeList <- set_attributes(attributes,
                                        col_classes = col_classes)
      }
    } else {
      
      attributeList <- set_attributes(attributes,
                                      col_classes = col_classes)
    }


    # Set physical

    physical <- set_physical(table_names[i],
                             numHeaderLines = num_header_lines[i],
                             recordDelimiter = record_delimeter[i],
                             attributeOrientation = attribute_orientation[i],
                             fieldDelimiter = field_delimeter[i])

    physical@size <- new("size",
                         unit = "byte",
                         as.character(
                           file.size(
                             paste(path,
                                   "/",
                                   table_names[i],
                                   sep = ""))))

    distribution <- new("distribution",
                        online = new("online",
                                     url = data_table_urls[i]))

    physical@distribution <- new("ListOfdistribution",
                                 c(distribution))
    

    if (os == "mac"){
      
      # Insert command to retrieve MD5 checksum in mac OS
      
    } else if (os == "win"){
      
      command_certutil <- paste("CertUtil -hashfile ",
                                path,
                                "\\",
                                table_names[i],
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
                      entityDescription = data_table_descriptions[i],
                      physical = physical,
                      attributeList = attributeList,
                      numberOfRecords = number_of_records)

    data_tables_stored[[i]] <- data_table

  }
  

  # Compile datatables, spatial vector folders, and metadata files ------------

  print("Compiling EML ...")

  # Are custom units present in these tables?

  custom_units_df <- read.xlsx2(
    paste(path,
          "/",
          substr(template, 1, nchar(template) - 14),
          "_custom_units.xlsx",
          sep = ""),
    sheetIndex = 1,
    colClasses = c("character","character","character","numeric","character"))

  write.table(custom_units_df,
              paste(path,
                    "/",
                    substr(template, 1, nchar(template) - 14),
                    "_custom_units.txt",
                    sep = ""),
              sep = "\t",
              row.names = F,
              quote = F,
              fileEncoding = "UTF-8")

  unitType_df <- read.table(
    paste(path,
          "/",
          substr(template, 1, nchar(template) - 14),
          "_custom_units.txt",
          sep = ""),
    header = TRUE,
    sep = "\t",
    as.is = TRUE,
    na.strings = "")

  if (nrow(custom_units_df) == 1 & sum(is.na(custom_units_df)) > 1){
    custom_units <- "no"
  } else {
    custom_units <- "yes"
  }

  # Clean white spaces from custom_units and units_types

  if (custom_units == "yes"){

    write.table(custom_units_df,
                paste(path,
                      "/",
                      substr(template, 1, nchar(template) - 14),
                      "_custom_units.txt",
                      sep = ""),
                sep = "\t",
                row.names = F,
                quote = F,
                fileEncoding = "UTF-8")

    custom_units_df <- read.table(
      paste(path,
            "/",
            substr(template, 1, nchar(template) - 14),
            "_custom_units.txt",
            sep = ""),
      header = TRUE,
      sep = "\t",
      as.is = TRUE,
      na.strings = "")

    for (j in 1:ncol(custom_units_df)){
      if (class(custom_units_df[ ,j]) == "character" ||
          (class(custom_units_df[ ,j]) == "factor")){
        custom_units_df[ ,j] <- trimws(custom_units_df[ ,j])
      }
    }

    unitsList <- set_unitList(custom_units_df)
  }

  # Compile data tables

  dataset@dataTable <- new("ListOfdataTable",
                           data_tables_stored)

  # Build EML
  
  if (custom_units == "yes"){
    eml <- new("eml",
               schemaLocation = "eml://ecoinformatics.org/eml-2.1.1  http://nis.lternet.edu/schemas/EML/eml-2.1.1/eml.xsd",
               packageId = data_package_id,
               system = root_system,
               access = access,
               dataset = dataset,
               additionalMetadata = as(unitsList, "additionalMetadata"))
  } else {
    eml <- new("eml",
               schemaLocation = schema_location,
               packageId = data_package_id,
               system = root_system,
               access = access,
               dataset = dataset)
  }

  # Validate EML

  print("Validating EML ...")

  validation_result <- eml_validate(eml)

  if (validation_result == "TRUE"){
    
    print("EML passed validation.")
    
  } else {
    
    print("EML validaton failed. See warnings for details.")
    
  }
  
  # Write EML

  print("Writing EML ...")
  
  write_eml(eml, paste(path, "/", data_package_id, ".xml", sep = ""))
  
}

