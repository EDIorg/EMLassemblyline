context('Make EML')
library(EMLassemblyline)

# Parameterize ----------------------------------------------------------------

# Parameterize

attr_tmp <- read_template_attributes()
x <- template_arguments(
  path = system.file(
    '/examples/pkg_260/metadata_templates',
    package = 'EMLassemblyline'),
  data.path = system.file(
    '/examples/pkg_260/data_objects',
    package = 'EMLassemblyline'),
  data.table = c("decomp.csv", "nitrogen.csv"),
  other.entity = c("ancillary_data.zip", "processing_and_analysis.R"))
x$x$template$taxonomic_coverage.txt <- NULL

x$data.path <- system.file('/examples/pkg_260/data_objects', package = 'EMLassemblyline')
x$data.table <- c("decomp.csv", "nitrogen.csv")
x$data.table.name <- c("Decomp file name", "Nitrogen file name")
x$data.table.description <- c("Decomp file description", "Nitrogen file description")
x$data.table.quote.character  <- c("\\'", "\\'")
x$data.table.url <- c("https://url/to/decomp.csv", "https://url/to/nitrogen.csv")
x$dataset.title <- 'Sphagnum and Vascular Plant Decomposition under Increasing Nitrogen Additions: 2014-2015'
x$eml.path <- system.file('/examples/pkg_260/eml', package = 'EMLassemblyline')
x$geographic.coordinates <- c('55.895', '112.094','55.895', '112.094')
x$geographic.description <- 'Alberta, Canada, 100 km south of Fort McMurray, Canada'
x$maintenance.description <- 'Completed'
x$other.entity <- c("ancillary_data.zip", "processing_and_analysis.R")
x$other.entity.name <- c("ancillary_data file name", "processing_and_analysis file name")
x$other.entity.description <- c("ancillary_data file description", "processing_and_analysis file description")
x$other.entity.url <- c("https://url/to/ancillary_data.zip", "https://url/to/processing_and_analysis.R")
x$package.id <- "edi.100.1"
x$path <- system.file('/examples/pkg_260/metadata_templates', package = 'EMLassemblyline')
x$provenance <- NULL
x$return.obj <- T
x$temporal.coverage <- c('2014-05-01', '2015-10-31')
x$user.domain <- c("EDI", "LTER")
x$user.id <- c("userid1", "userid2")
x$write.file <- F

# Default test inputs create schema valid EML

x1 <- x
r <- do.call(make_eml, x1[names(x1) %in% names(formals(make_eml))])
expect_true(
  EML::eml_validate(r))

# Expect argument errors ------------------------------------------------------

testthat::test_that('Error out when required arguments are missing', {
  
  # NOTE: These tests verify validation checks implemented within the 
  # make_eml() function. The remainder are handled by validate_arguments().
  
  # path
  
  expect_error(
    make_eml(
      dataset.title = "dataset.title",
      temporal.coverage = "temporal.coverage",
      geographic.description = "geographic.description",
      geographic.coordinates = "geographic.coordinates",
      maintenance.description = "maintenance.description",
      user.id = "user.id",
      user.domain = "user.domain",
      write.file = FALSE))
  
})

# Expect argument values in EML -----------------------------------------------

testthat::test_that('Expect argument values in EML', {

  # data.table.name - defaults to data.table
  
  x1 <- x
  x1$data.table.name <- NULL
  r <- suppressWarnings(
    do.call(make_eml, x1[names(x1) %in% names(formals(make_eml))]))
  for (i in 1:length(r$dataset$dataTable)) {
    expect_true(
      r$dataset$dataTable[[i]]$entityName == r$dataset$dataTable[[i]]$physical$objectName)
  }
  
  # data.table.name
  
  x1 <- x
  r <- do.call(make_eml, x1[names(x1) %in% names(formals(make_eml))])
  for (i in 1:length(r$dataset$dataTable)) {
    expect_true(
      r$dataset$dataTable[[i]]$entityName == x1$data.table.name[i])
  }
  
  # data.table.quote.character
  
  x1 <- x
  r <- do.call(make_eml, x1[names(x1) %in% names(formals(make_eml))])
  for (i in 1:length(r$dataset$dataTable)) {
    expect_true(
      length(r$dataset$dataTable[[i]]$physical$dataFormat$textFormat$simpleDelimited$quoteCharacter) != 0)
  }
  
  # data.table.url
  
  x1 <- x
  r <- do.call(make_eml, x1[names(x1) %in% names(formals(make_eml))])
  for (i in 1:length(r$dataset$dataTable)) {
    expect_equal(
      r$dataset$dataTable[[i]]$physical$distribution$online$url,
      x1$data.table.url[i])
  }
  
  # data.table.url - A URL is not required for each dataTable
  
  x1 <- x
  x1$data.table.url[1] <- ""
  r <- do.call(make_eml, x1[names(x1) %in% names(formals(make_eml))])
  expect_true(
    is.null(r$dataset$dataTable[[1]]$physical$distribution$online$url))
  expect_equal(
    r$dataset$dataTable[[2]]$physical$distribution$online$url,
    x1$data.table.url[2])
  
  x1 <- x
  x1$data.table.url[1] <- NA_character_
  r <- do.call(make_eml, x1[names(x1) %in% names(formals(make_eml))])
  expect_true(
    is.null(r$dataset$dataTable[[1]]$physical$distribution$online$url))
  expect_equal(
    r$dataset$dataTable[[2]]$physical$distribution$online$url,
    x1$data.table.url[2])
  
  # other.entity
  
  x1 <- x
  r <- do.call(make_eml, x1[names(x1) %in% names(formals(make_eml))])
  expect_true(!is.null(r$dataset$otherEntity))
  for (i in 1:length(r$dataset$otherEntity)) {
    expect_true(
      r$dataset$otherEntity[[i]]$physical$objectName == names(x1$x$other.entity[i]))
  }
  
  # other.entity - MIME Type
  
  x1 <- x
  r <- do.call(make_eml, x1[names(x1) %in% names(formals(make_eml))])
  for (i in 1:length(r$dataset$otherEntity)) {
    expect_true(
      r$dataset$otherEntity[[i]]$physical$dataFormat$externallyDefinedFormat$formatName != "Unknown")
  }
  
  # other.entity.name - defaults to other.entity
  
  x1 <- x
  x1$other.entity.name <- NULL
  r <- suppressWarnings(
    do.call(make_eml, x1[names(x1) %in% names(formals(make_eml))]))
  for (i in 1:length(r$dataset$otherEntity)) {
    expect_true(
      r$dataset$otherEntity[[i]]$entityName == r$dataset$otherEntity[[i]]$physical$objectName)
  }
  
  # other.entity.url
  
  x1 <- x
  r <- do.call(make_eml, x1[names(x1) %in% names(formals(make_eml))])
  for (i in 1:length(r$dataset$otherEntity)) {
    expect_equal(
      r$dataset$otherEntity[[i]]$physical$distribution$online$url,
      x1$other.entity.url[i])
  }
  
  # other.entity.url - A URL is not required for each otherEntity
  
  x1 <- x
  x1$other.entity.url[1] <- ""
  r <- do.call(make_eml, x1[names(x1) %in% names(formals(make_eml))])
  expect_true(
    is.null(r$dataset$otherEntity[[1]]$physical$distribution$online$url))
  expect_equal(
    r$dataset$otherEntity[[2]]$physical$distribution$online$url,
    x1$other.entity.url[2])
  
  x1 <- x
  x1$other.entity.url[1] <- NA_character_
  r <- do.call(make_eml, x1[names(x1) %in% names(formals(make_eml))])
  expect_true(
    is.null(r$dataset$otherEntity[[1]]$physical$distribution$online$url))
  expect_equal(
    r$dataset$otherEntity[[2]]$physical$distribution$online$url,
    x1$other.entity.url[2])
  
  # provenance - Get provenance metadata for EDI data packages and place under
  # /eml/dataset/methods/methodStep/dataSource
  
  x1 <- x
  x1$provenance <- "edi.100.1"
  r <- do.call(make_eml, x1[names(x1) %in% names(formals(make_eml))])
  expect_true(EML::eml_validate(r))
  expect_true(
    r$dataset$methods$methodStep[[2]]$description$para[[1]] == 
      "This method step describes provenance-based metadata as specified in the LTER EML Best Practices.")
  expect_true(
    r$dataset$methods$methodStep[[2]]$description$para[[2]] == 
      "This provenance metadata does not contain entity specific information.")
  expect_true(
    all(
      c("title", "contact", "distribution", "creator") %in% 
        names(r$dataset$methods$methodStep[[2]]$dataSource)))
  
})

# Expect template values in EML -----------------------------------------------

testthat::test_that('Expect template values in EML', {

  # custom_units.txt
  
  x1 <- x
  r <- do.call(make_eml, x1[names(x1) %in% names(formals(make_eml))])
  expect_true(
    length(r$additionalMetadata[[1]]$metadata$unitList$unit) == nrow(x$x$template$custom_units.txt$content))
  
  # geographic_coverage.txt
  
  x1 <- x
  x1$geographic.coordinates <- NULL
  x1$geographic.description <- NULL
  r <- do.call(make_eml, x1[names(x1) %in% names(formals(make_eml))])
  expect_true(
    length(r$dataset$coverage$geographicCoverage) == nrow(x1$x$template$geographic_coverage.txt$content))
  
  # personnel.txt - Principal Investigator is absent
  
  x1 <- x
  use_i <- which(x1$x$template$personnel.txt$content$role == "PI")
  x1$x$template$personnel.txt$content <- x1$x$template$personnel.txt$content[-use_i, ]
  r <- suppressWarnings(
    do.call(make_eml, x1[names(x1) %in% names(formals(make_eml))]))
  expect_true(
    is.null(r$dataset$project))
  
  # personnel.txt - only projectTitle is present
  
  x1 <- x
  x1$x$template$personnel.txt$content$fundingAgency <- ""
  x1$x$template$personnel.txt$content$fundingNumber <- ""
  r <- do.call(make_eml, x1[names(x1) %in% names(formals(make_eml))])
  x1$x$template$personnel.txt$content$projectTitle[
    x1$x$template$personnel.txt$content$projectTitle == ""] <- "No project title to report"
  expect_equal(
    r$dataset$project$title,
    x1$x$template$personnel.txt$content$projectTitle[
      x1$x$template$personnel.txt$content$role == "PI"][1])
  for (i in 1:length(r$dataset$project$relatedProject)) {
    expect_equal(
      r$dataset$project$relatedProject[[i]]$title,
      x1$x$template$personnel.txt$content$projectTitle[
        x1$x$template$personnel.txt$content$role == "PI"][
          2:sum(x1$x$template$personnel.txt$content$role == "PI")][[i]])
  }
  
  # personnel.txt - only fundingAgency is present
  
  x1 <- x
  x1$x$template$personnel.txt$content$projectTitle <- ""
  x1$x$template$personnel.txt$content$fundingNumber <- ""
  r <- do.call(make_eml, x1[names(x1) %in% names(formals(make_eml))])
  x1$x$template$personnel.txt$content$fundingAgency[
    x1$x$template$personnel.txt$content$fundingAgency == ""] <- "No funding to report"
  expect_equal(
    r$dataset$project$funding,
    x1$x$template$personnel.txt$content$fundingAgency[
      x1$x$template$personnel.txt$content$role == "PI"][1])
  for (i in 1:length(r$dataset$project$relatedProject)) {
    expect_equal(
      r$dataset$project$relatedProject[[i]]$funding,
      x1$x$template$personnel.txt$content$fundingAgency[
        x1$x$template$personnel.txt$content$role == "PI"][
          2:sum(x1$x$template$personnel.txt$content$role == "PI")][[i]])
  }

  # personnel.txt - only fundingNumber is present
  
  x1 <- x
  x1$x$template$personnel.txt$content$projectTitle <- ""
  x1$x$template$personnel.txt$content$fundingAgency <- ""
  r <- do.call(make_eml, x1[names(x1) %in% names(formals(make_eml))])
  x1$x$template$personnel.txt$content$fundingNumber[
    x1$x$template$personnel.txt$content$fundingNumber == ""] <- "No funding to report"
  expect_equal(
    r$dataset$project$funding,
    x1$x$template$personnel.txt$content$fundingNumber[
      x1$x$template$personnel.txt$content$role == "PI"][1])
  for (i in 1:length(r$dataset$project$relatedProject)) {
    expect_equal(
      r$dataset$project$relatedProject[[i]]$funding,
      x1$x$template$personnel.txt$content$fundingNumber[
        x1$x$template$personnel.txt$content$role == "PI"][
          2:sum(x1$x$template$personnel.txt$content$role == "PI")][[i]])
  }
  
  # personnel.txt - projectTitle, fundingAgency, fundingNumber are all present
  
  x1 <- x
  r <- do.call(make_eml, x1[names(x1) %in% names(formals(make_eml))])
  expect_equal(
    r$dataset$project$title,
    x1$x$template$personnel.txt$content$projectTitle[
      x1$x$template$personnel.txt$content$role == "PI"][1])
  expect_equal(
    r$dataset$project$funding,
    paste(
      x1$x$template$personnel.txt$content$fundingAgency[
        x1$x$template$personnel.txt$content$role == "PI"][1],
      x1$x$template$personnel.txt$content$fundingNumber[
        x1$x$template$personnel.txt$content$role == "PI"][1]))
  expect_equal(
    r$dataset$project$relatedProject[[1]]$title,
    "Decomposition in the real world")
  expect_equal(
    r$dataset$project$relatedProject[[1]]$funding,
    "NSF 111001")
  expect_equal(
    r$dataset$project$relatedProject[[2]]$title,
    "No project title to report")
  expect_equal(
    r$dataset$project$relatedProject[[2]]$funding,
    "NSF 101")
  expect_equal(
    r$dataset$project$relatedProject[[3]]$title,
    "No project title to report")
  expect_equal(
    r$dataset$project$relatedProject[[3]]$funding,
    "111001")
  expect_equal(
    r$dataset$project$relatedProject[[4]]$title,
    "No project title to report")
  expect_equal(
    r$dataset$project$relatedProject[[4]]$funding,
    "No funding to report")
  
  # personnel.txt - publisher
  
  x1 <- x
  use_i <- min(which(x1$x$template$personnel.txt$content$role == "creator"))
  x1$x$template$personnel.txt$content$role[use_i] <- "publisher"
  r <- do.call(make_eml, x1[names(x1) %in% names(formals(make_eml))])
  expect_true(
    !is.null(r$dataset$publisher))
  expect_true(
    length(r$dataset$publisher[[1]]) > 1)
  
  x1 <- x
  use_i <- which(x1$x$template$personnel.txt$content$role == "creator")[1:3]
  x1$x$template$personnel.txt$content$role[use_i] <- "publisher"
  r <- suppressWarnings(
    do.call(make_eml, x1[names(x1) %in% names(formals(make_eml))]))
  expect_true(
    !is.null(r$dataset$publisher))
  expect_true(
    length(r$dataset$publisher[[1]]) > 1)

  # taxonomic_coverage.txt
  
  x1 <- x
  x1$x <- template_arguments(
    path = system.file(
      '/examples/pkg_260/metadata_templates',
      package = 'EMLassemblyline'),
    data.path = system.file(
      '/examples/pkg_260/data_objects',
      package = 'EMLassemblyline'),
    data.table = c("decomp.csv", "nitrogen.csv"),
    other.entity = c("ancillary_data.zip", "processing_and_analysis.R"))$x
  r <- do.call(make_eml, x1[names(x1) %in% names(formals(make_eml))])
  use_i <- try(
    expect_true(
      !is.null(r$dataset$coverage$taxonomicCoverage)), 
    silent = TRUE)
  if (!isTRUE(use_i)) {
    if (!attr(use_i, "class") == "try-error") {
      expect_true(
        !is.null(r$dataset$coverage$taxonomicCoverage))
    }
  }

})

# NA values -------------------------------------------------------------------

testthat::test_that("NA values", {
  
  # Users often add NAs to templates where EMLassemblyline expects "". Such 
  # behavior use to cause make_eml() to crash, now it doesn't.

  x1 <- x
  x1$x$template$attributes_decomp.txt$content$unit[1] <- NA
  x1$x$template$attributes_decomp.txt$content$dateTimeFormatString[1] <- NA
  x1$x$template$custom_units.txt$content$description[1] <- NA
  x1$x$template$keywords.txt$content$keywordThesaurus[1] <- NA
  x1$x$template$personnel.txt$content$middleInitial[1] <- NA
  r <- do.call(make_eml, x1[names(x1) %in% names(formals(make_eml))])
  expect_true(
    EML::eml_validate(r))
  
  # "NA" in the missingValueCode field of attribtues templates should be 
  # should be interpreted as a missing value code by make_eml() and ultimately
  # be listed as such in the EML.
  
  x1 <- x
  x1$x$template$attributes_decomp.txt$content$missingValueCode[1] <- "NA"
  r <- do.call(make_eml, x1[names(x1) %in% names(formals(make_eml))])
  expect_true(
    r$dataset$dataTable[[1]]$attributeList$attribute[[1]]$missingValueCode$code == "NA")
  expect_true(
    r$dataset$dataTable[[1]]$attributeList$attribute[[2]]$missingValueCode$code == "-99999")
  
  # NA in the missingValueCode field of attribtues templates input with the 
  # argument "x" should be interpreted as "".
  
  x1 <- x
  x1$x$template$attributes_decomp.txt$content$missingValueCode[1] <- NA_character_
  x1$x$template$attributes_decomp.txt$content$missingValueCodeExplanation[1] <- NA_character_
  r <- do.call(make_eml, x1[names(x1) %in% names(formals(make_eml))])
  expect_true(
    is.na(r$dataset$dataTable[[1]]$attributeList$attribute[[1]]$missingValueCode$code))
  expect_true(
    is.na(r$dataset$dataTable[[1]]$attributeList$attribute[[1]]$missingValueCode$codeExplanation))
  expect_true(
    r$dataset$dataTable[[1]]$attributeList$attribute[[2]]$missingValueCode$code == "-99999")
  expect_true(
    r$dataset$dataTable[[1]]$attributeList$attribute[[2]]$missingValueCode$codeExplanation == "Missing value")
  
})


# EML header attributes -------------------------------------------------------

testthat::test_that("EML header attributes", {
  
  # package.id - NULL packageId gets assigned a UUID
  
  x1 <- x
  x1$package.id <- NULL
  r <- suppressWarnings(
    do.call(make_eml, x1[names(x1) %in% names(formals(make_eml))]))
  expect_true(
    all(nchar(unlist(stringr::str_split(r$packageId, "-"))) == c(8, 4, 4, 4, 12)))
  
  # package.id - Unknown package IDs are listed 'as is'
  
  x1 <- x
  x1$package.id <- "some-package-id"
  r <- suppressWarnings(
    do.call(make_eml, x1[names(x1) %in% names(formals(make_eml))]))
  expect_true(
    r$packageId == x1$package.id)
  
  # user.domain - If EDI or LTER then system = "edi"
  
  x1 <- x
  x1$user.domain <- "EDI"
  x1$user.id <- "myid"
  r <- do.call(make_eml, x1[names(x1) %in% names(formals(make_eml))])
  expect_true(
    r$system == "edi")
  
  x1 <- x
  x1$user.domain <- "LTER"
  x1$user.id <- "myid"
  r <- do.call(make_eml, x1[names(x1) %in% names(formals(make_eml))])
  expect_true(
    r$system == "edi")
  
  # user.domain - If KNB then system = "knb"
  
  x1 <- x
  x1$user.domain <- "KNB"
  r <- do.call(make_eml, x1[names(x1) %in% names(formals(make_eml))])
  expect_true(
    r$system == "knb")
  
  # user.domain - If ADC then system = "adc"
  
  x1 <- x
  x1$user.domain <- "ADC"
  r <- do.call(make_eml, x1[names(x1) %in% names(formals(make_eml))])
  expect_true(
    r$system == "https://arcticdata.io")
  
  # user.domain - If NULL or unknown then system = "unknown"
  
  x1 <- x
  x1$user.domain <- NULL
  x1$user.id <- NULL
  r <- suppressWarnings(
    do.call(make_eml, x1[names(x1) %in% names(formals(make_eml))]))
  expect_true(
    r$system == "unknown")
  
})


# <access> --------------------------------------------------------------------

testthat::test_that("<access>", {
  
  # user.domain - Not all user.domain (i.e. systems) use the access node. In
  # these cases it's dropped.
  
  x1 <- x
  r <- do.call(make_eml, x1[names(x1) %in% names(formals(make_eml))])
  expect_true(!is.null(r$access))
  
  x1 <- x
  x1$user.domain <- "KNB"
  r <- do.call(make_eml, x1[names(x1) %in% names(formals(make_eml))])
  expect_null(r$access)
  
  x1 <- x
  x1$user.domain <- "ADC"
  r <- do.call(make_eml, x1[names(x1) %in% names(formals(make_eml))])
  expect_null(r$access)

  # user.domain - When EDI/LTER a unique set of access attributes and 
  # principal values are created
  
  x1 <- x
  r <- do.call(make_eml, x1[names(x1) %in% names(formals(make_eml))])
  for (i in 1:length(x1$user.domain)) {
    if (x1$user.domain[i] == "EDI") {
      expect_equal(
        r$access$allow[[i]]$principal, 
        "uid=userid1,o=EDI,dc=edirepository,dc=org")
    } else if (x1$user.domain[i] == "LTER") {
      expect_equal(
        r$access$allow[[i]]$principal, 
        "uid=userid2,o=LTER,dc=ecoinformatics,dc=org")
    }
  }
  expect_true(
    r$access$authSystem == "https://pasta.edirepository.org/authentication")
  
  # user.id - Controls whether the access node is created or not. If not NULL 
  # then the access node is created.
  
  x1 <- x
  x1$user.id <- "some_user_id"
  x1$user.domain <- NULL
  r <- do.call(make_eml, x1[names(x1) %in% names(formals(make_eml))])
  expect_equal(
    r$access$allow[[1]]$principal, "some_user_id")
  
  # user.id - Missing user.id is set to NULL value
  
  x1 <- x
  x1$user.id <- NULL
  x1$user.domain <- "EDI"
  r <- suppressWarnings(
    do.call(make_eml, x1[names(x1) %in% names(formals(make_eml))]))
  expect_null(r$access)
  
  # user.id - Multiple user.id creates multiple principal elements
  
  x1 <- x
  x1$user.id <- c("id1", "id2", "id3")
  x1$user.domain <- c("some_domain", "some_domain", "some_domain")
  r <- do.call(make_eml, x1[names(x1) %in% names(formals(make_eml))])
  expect_equal(
    length(r$access$allow), (length(x1$user.id) + 1))
  
  # user.id & user.domain - Different lengths result in defaults
  
  x1 <- x
  x1$user.id <- c("userid1", "userid2")
  x1$user.domain <- "some_domain"
  r <- suppressWarnings(
    do.call(make_eml, x1[names(x1) %in% names(formals(make_eml))]))
  for (i in 1:length(x1$user.id)) {
    expect_equal(
      r$access$allow[[i]]$principal, x1$user.id[i])
  }
  
  x1 <- x
  x1$user.id <- c("userid1")
  x1$user.domain <- c("some_domain", "some_domain2")
  r <- suppressWarnings(
    do.call(make_eml, x1[names(x1) %in% names(formals(make_eml))]))
  for (i in 1:length(x1$user.id)) {
    expect_equal(
      r$access$allow[[i]]$principal, x1$user.id[i])
  }
  
})



