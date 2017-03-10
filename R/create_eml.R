#' Create EML and write to file
#'
#' @description  A function for writting metadata in the Ecological Metadata Language
#' standard.
#'
#' @usage create_eml(path)
#'
#' @param path A path to the working directory containing files required by the EMLtools
#' package. Run the function run_guide() for instructions on how to populate this working
#' directory with the requisite files.
#'
#' @return A .xml file in the specified working directory. If the EML fails validation
#' then the file is not written.
#'
#' @seealso \code{\link{run_guide}} for guidance on setting up the working directory
#' @seealso \code{\link{eml_configuration}} for configuring eml
#' @seealso \code{\link{write_attributes}} for writing data attributes
#' @seealso \code{\link{write_factors}} for writing data factors
#'


create_eml <- function(path) {
  
  # Parameterize function
  
  library("EML")
  library("xlsx")
  library("rmarkdown")
  library("methods")
  library("stringr")
  library("tools")
  
  template <- trimws(list.files(path, pattern = "*_template.docx"))

  # Parameterize function -----------------------------------------------------

  # Load the datasets configuration file

  source(paste(path, "eml_configuration.R", sep = ""))

  # Set file names

  fname_abstract <- paste(path,
                          substr(template, 1, nchar(template) - 14),
                          "_abstract.docx",
                          sep = "")

  fname_personnel <- paste(path,
                          substr(template, 1, nchar(template) - 14),
                          "_personnel.xlsx",
                          sep = "")

  fname_intellectual_rights <- paste(path,
                                     substr(template, 1,
                                            nchar(template) - 14),
                                     "_intellectual_rights.md", sep = "")

  fname_methods <- paste(path,
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

  fname_table_factors <- c()
  for (i in 1:length(table_names)){
    fname_table_factors[i] <- paste(
      substr(table_names[i], 1, nchar(table_names[i]) - 4),
      "_factors.xlsx",
      sep = "")
  }

  fname_spatial_vector_attributes <- c()
  for (i in 1:length(spatial_vector_names)){
    fname_spatial_vector_attributes <- paste(
      substr(spatial_vector_names[i], 1, nchar(spatial_vector_names[i]) - 4),
      "_attributes.xlsx",
      sep = "")
  }

  # Initialize data entity storage (tables and spatial vectors)

  data_tables_stored <- list()

  spatial_vectors_stored <- list()

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


  # Loop through tables -------------------------------------------------------


  for (i in 1:length(table_names)){

    print(paste(
      "Now building attributes for ... ",
      table_names[i],
      sep = ""))

    # Read attributes file (encoding necessitates read/write/read)

    attributes <- read.xlsx2(paste(
      path,
      fname_table_attributes[i], sep = ""),
      sheetIndex = 1,
      colClasses = c(rep("character",7), rep("numeric",2),rep("character",2)))

    write.table(
      attributes,
      paste(
        path,
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
        substr(fname_table_attributes[i],
               1,
               nchar(fname_table_attributes[i]) - 5),
        ".txt",
        sep = ""),
      header = TRUE,
      sep = "\t",
      as.is = TRUE,
      na.strings = "NA")

    useI <- attributes$missingValueCodeExplanation == ""

    attributes$missingValueCode[!useI] <- "NA"

    codeExplanations <- attributes$missingValueCodeExplanation

    attributes$missingValueCodeExplanation <- as.logical(
      attributes$missingValueCodeExplanation)

    attributes$missingValueCodeExplanation[!useI] <- codeExplanations[!useI]

    # Read factors file (encoding necessitates read/write/read)

    fname_expected_factors <- paste(
      substr(table_names[i], 1, nchar(table_names[i]) - 4),
      "_factors.xlsx",
      sep = "")

    if (!is.na(match(fname_expected_factors, list.files(path)))){

      factors <- read.xlsx2(paste(
        path,
        fname_table_factors[i], sep = ""),
        sheetIndex = 1,
        colClasses = c(rep("character",3)))

      if (dim(factors)[1] > 0){

        for (j in 1:dim(factors)[2]){
          factors[ ,j] <- as.character(factors[ ,j])
        }

        write.table(
          factors,
          paste(
            path,
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

    if (!is.na(match(fname_expected_factors, list.files(path)))){

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
                         as.character(
                           file.size(
                             paste(path,
                                   table_names[i],
                                   sep = ""))))

    distribution <- new("distribution",
                        online = new("online",
                                     url = data_table_urls[i]))

    physical@distribution <- new("ListOfdistribution",
                                 c(distribution))

    # Pull together information for the data table

    data_table <- new("dataTable",
                      entityName = table_names[i],
                      entityDescription = data_table_descriptions[i],
                      physical = physical,
                      attributeList = attributeList)

    data_tables_stored[[i]] <- data_table

  }


  # Loop through the number of spatial vector data entities -------------------

  if (length(spatial_vector_names) > 0){

    for (i in 1:length(spatial_vector_names)){

      print(paste("Now building attributes for ... ",
                  spatial_vector_names[i], sep = ""))

      # Create and set physical

      physical <- set_physical(
        spatial_vector_names[i],
        numHeaderLines = num_header_lines_sv[i],
        recordDelimiter = record_delimeter_sv[i],
        attributeOrientation = attribute_orientation_sv[i],
        fieldDelimiter = field_delimeter_sv[i])

      physical@size <- new("size",
                           spatial_vector_sizes[i])

      distribution <- new("distribution",
                          online = new("online",
                                       url = spatial_vector_urls[i]))

      physical@distribution <- new("ListOfdistribution", c(distribution))

      # Read attributes file

      attributes <- read.xlsx2(
        paste(
          path,
          fname_spatial_vector_attributes[i],
          sep = ""),
        sheetIndex = 1,
        colClasses = c(rep("character",7),
                       rep("numeric",2),
                       rep("character",2)))

      write.table(
        attributes,
        paste(path,
              substr(fname_spatial_vector_attributes[i],
                     1,
                     nchar(fname_spatial_vector_attributes[i]) - 5),
              ".txt",
              sep=""),
        sep = "\t",
        row.names = F,
        quote = F,
        fileEncoding = "UTF-8")

      attributes <- read.table(
        paste(path,
              substr(fname_spatial_vector_attributes[i],
                     1,
                     nchar(fname_spatial_vector_attributes[i]) - 5),
              ".txt",
              sep=""),
        header = TRUE,
        sep = "\t",
        as.is = TRUE,
        na.strings = "NA")

      useI <- attributes$missingValueCodeExplanation == ""

      if (!is.na(useI)){

        attributes$missingValueCode[!useI] <- "NA"

        codeExplanations <- attributes$missingValueCodeExplanation

        attributes$missingValueCodeExplanation <- as.logical(
          attributes$missingValueCodeExplanation)

        attributes$missingValueCodeExplanation[!useI] <- codeExplanations[
          !useI]

      } else {

        attributes$missingValueCode <- as.character(
          attributes$missingValueCode)

        attributes$missingValueCodeExplanation <- as.character(
          attributes$missingValueCodeExplanation)
      }

      # Clean extraneous white spaces from attributes tables

      for (j in 1:ncol(attributes)){
        if (class(attributes[ ,j]) == "character" ||
            (class(attributes[ ,j]) == "factor")){
          attributes[ ,j] <- trimws(attributes[ ,j])
        }
      }

      # Get the column classes into a vector

      col_classes <- attributes[ ,"columnClasses"]

      # Create the attributeList element

      attributeList <- set_attributes(attributes, col_classes = col_classes)

      # Pull together information for the spatial vector object

      spatial_vector <- new("spatialVector",
                        entityName = spatial_vector_names[i],
                        entityDescription = spatial_vector_description[i],
                        physical = physical,
                        attributeList = attributeList,
                        geometry = spatial_vector_geometry[i])

      spatial_vectors_stored[[i]] <- spatial_vector

    }

  }


  # Compile datatables, spatial vector folders, and metadata files ------------

  print("Compiling EML ...")

  # Are custom units present in these tables?

  unitType_df <- read.xlsx2(
    paste(path,
          substr(template, 1, nchar(template) - 14),
          "_unit_types.xlsx",
          sep = ""),
    sheetIndex = 1,
    colClasses = c("character","character","numeric"))

  write.table(unitType_df,
              paste(path,
                    substr(template, 1, nchar(template) - 14),
                    "_unit_types.txt",
                    sep = ""),
              sep = "\t",
              row.names = F,
              quote = F,
              fileEncoding = "UTF-8")

  unitType_df <- read.table(
    paste(path,
          substr(template, 1, nchar(template) - 14),
          "_unit_types.txt",
          sep = ""),
    header = TRUE,
    sep = "\t",
    as.is = TRUE,
    na.strings = "")

  if (nrow(unitType_df) == 1 & sum(is.na(unitType_df)) > 0){
    custom_units <- "no"
  } else {
    custom_units <- "yes"
  }

  # Clean white spaces from custom_units and units_types

  if (custom_units == "yes"){

    for (j in 1:ncol(unitType_df)){
      if (class(unitType_df[ ,j]) == "character" ||
          (class(unitType_df[ ,j]) == "factor")){
        unitType_df[ ,j] <- trimws(unitType_df[ ,j])
      }
    }

    custom_units_df <- read.xlsx2(
      paste(path,
            substr(template, 1, nchar(template) - 14),
            "_custom_units.xlsx", sep = ""),
      sheetIndex = 1,
      colClasses = c("character",
                     "character",
                     "character",
                     "numeric",
                     "character"))

    write.table(custom_units_df,
                paste(path,
                      substr(template, 1, nchar(template) - 14),
                      "_custom_units.txt",
                      sep = ""),
                sep = "\t",
                row.names = F,
                quote = F,
                fileEncoding = "UTF-8")

    custom_units_df <- read.table(
      paste(path,
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

    unitsList <- set_unitList(custom_units_df,
                              unitType_df)
  }

  # Compile data tables

  dataset@dataTable <- new("ListOfdataTable",
                           data_tables_stored)

  # Compile spatial vectors

  dataset@spatialVector <- new("ListOfspatialVector",
                               spatial_vectors_stored)

  # Build EML

  if (custom_units == "yes"){
    eml <- new("eml",
              packageId = data_package_id,
              system = root_system,
              access = access,
              dataset = dataset,
              additionalMetadata = as(unitsList, "additionalMetadata"))
  } else {
    eml <- new("eml",
               packageId = data_package_id,
               system = root_system,
               access = access,
               dataset = dataset)
  }

  # Validate EML

  print("Validating EML ...")

  validation_result <- eml_validate(eml)

  # Write EML

  if (validation_result == "TRUE"){
    print("Writing EML ...")
    write_eml(eml, paste(path, data_package_id, ".xml", sep = ""))
  } else {
    print("EML validaton failed. EML was not written to file.")
  }

}

