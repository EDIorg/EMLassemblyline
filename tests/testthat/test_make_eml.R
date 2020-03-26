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

  # data.table.quote.character
  
  x1 <- x
  r <- do.call(make_eml, x1[names(x1) %in% names(formals(make_eml))])
  for (i in 1:length(r$dataset$dataTable)) {
    expect_true(
      length(r$dataset$dataTable[[i]]$physical$dataFormat$textFormat$simpleDelimited$quoteCharacter) != 0)
  }
  
  # other.entity
  
  x1 <- x
  r <- do.call(make_eml, x1[names(x1) %in% names(formals(make_eml))])
  expect_true(!is.null(r$dataset$otherEntity))
  for (i in 1:length(r$dataset$otherEntity)) {
    expect_true(
      r$dataset$otherEntity[[i]]$physical$objectName == names(x1$x$other.entity[i]))
  }

  # .docx templates are supported
  # FIXME: Move to test_template_arguments.R
  
  output <- suppressWarnings(
    make_eml(
      path = system.file('/examples/templates_docx', package = 'EMLassemblyline'),
      data.path = data.path,
      eml.path = eml.path,
      dataset.title = 'Sphagnum and Vascular Plant Decomposition under Increasing Nitrogen Additions: 2014-2015',
      data.table = data.table,
      data.table.name = data.table.name,
      data.table.description = c('Decomposition data', 'Nitrogen data'),
      data.table.quote.character = c("\'", "\'"),
      temporal.coverage = c('2014-05-01', '2015-10-31'),
      maintenance.description = 'completed',
      user.id = user.id,
      user.domain = user.domain,
      package.id = 'edi.141.1',
      return.obj = TRUE,
      write.file = FALSE
    )
  )
  
  expect_true(
    (nchar(output$dataset$abstract$para) > 100)
  )
  
  expect_true(
    (nchar(output$dataset$methods$methodStep$description$para) > 100)
  )
  
  expect_true(
    (nchar(output$dataset$additionalInfo$para) > 50)
  )
  
  # md templates are supported
  # FIXME: Move to test_template_arguments.R
  
  output <- suppressWarnings(
    make_eml(
      path = system.file('/examples/templates_md', package = 'EMLassemblyline'),
      data.path = data.path,
      eml.path = eml.path,
      dataset.title = 'Sphagnum and Vascular Plant Decomposition under Increasing Nitrogen Additions: 2014-2015',
      data.table = data.table,
      data.table.name = data.table.name,
      data.table.description = c('Decomposition data', 'Nitrogen data'),
      data.table.quote.character = c("\'", "\'"),
      temporal.coverage = c('2014-05-01', '2015-10-31'),
      maintenance.description = 'completed',
      user.id = user.id,
      user.domain = user.domain,
      package.id = 'edi.141.1',
      return.obj = TRUE,
      write.file = FALSE
    )
  )
  
  expect_equal(
    class(output) == 'list',
    TRUE
  )
  
  # data.table.name defaults to data.table
  
  output <- expect_warning(
    suppressMessages(
      make_eml(
        path = paste0(tempdir(), '/templates'),
        data.path = data.path,
        eml.path = eml.path,
        dataset.title = 'Sphagnum and Vascular Plant Decomposition under Increasing Nitrogen Additions: 2014-2015',
        data.table = data.table,
        data.table.description = data.table.description,
        temporal.coverage = c('2014-05-01', '2015-10-31'),
        maintenance.description = 'completed',
        user.id = user.id,
        user.domain = user.domain,
        package.id = 'edi.141.1',
        return.obj = TRUE,
        write.file = FALSE
      )
    )
  )
    
  for (i in 1:length(output$dataset$dataTable)) {
    expect_equal(
      output$dataset$dataTable[[i]]$entityName,
      output$dataset$dataTable[[i]]$physical$objectName
    )
  }
  
  # data.table.name
  
  output <- suppressMessages(
    make_eml(
      path = paste0(tempdir(), '/templates'),
      data.path = data.path,
      eml.path = eml.path,
      dataset.title = 'Sphagnum and Vascular Plant Decomposition under Increasing Nitrogen Additions: 2014-2015',
      data.table = data.table,
      data.table.name = data.table.name,
      data.table.description = data.table.description,
      temporal.coverage = c('2014-05-01', '2015-10-31'),
      maintenance.description = 'completed',
      user.id = user.id,
      user.domain = user.domain,
      package.id = 'edi.141.1',
      return.obj = TRUE,
      write.file = FALSE
    )
  )
  
  for (i in 1:length(output$dataset$dataTable)) {
    expect_equal(
      output$dataset$dataTable[[i]]$entityName,
      data.table.name[i]
    )
  }
  
  # other.entity.name defaults to other.entity
  
  output <- expect_warning(
    suppressMessages(
      make_eml(
        path = paste0(tempdir(), '/templates'),
        data.path = data.path,
        eml.path = eml.path,
        dataset.title = 'Sphagnum and Vascular Plant Decomposition under Increasing Nitrogen Additions: 2014-2015',
        other.entity = other.entity,
        other.entity.description = other.entity.description,
        temporal.coverage = c('2014-05-01', '2015-10-31'),
        maintenance.description = 'completed',
        user.id = user.id,
        user.domain = user.domain,
        package.id = 'edi.141.1',
        return.obj = TRUE,
        write.file = FALSE
      )
    )
  )
  
  for (i in 1:length(output$dataset$otherEntity)) {
    expect_equal(
      output$dataset$otherEntity[[i]]$entityName[[1]],
      output$dataset$otherEntity[[i]]$physical$objectName
    )
  }
  
  # other.entity.name
  
  output <- suppressMessages(
    make_eml(
      path = paste0(tempdir(), '/templates'),
      data.path = data.path,
      eml.path = eml.path,
      dataset.title = 'Sphagnum and Vascular Plant Decomposition under Increasing Nitrogen Additions: 2014-2015',
      other.entity = other.entity,
      other.entity.name = other.entity.name,
      other.entity.description = other.entity.description,
      temporal.coverage = c('2014-05-01', '2015-10-31'),
      maintenance.description = 'completed',
      user.id = user.id,
      user.domain = user.domain,
      package.id = 'edi.141.1',
      return.obj = TRUE,
      write.file = FALSE
    )
  )
  
  for (i in 1:length(output$dataset$otherEntity)) {
    expect_equal(
      output$dataset$otherEntity[[i]]$entityName[[1]],
      other.entity.name[i]
    )
  }

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
    paste0(
      x1$x$template$personnel.txt$content$fundingAgency[
        x1$x$template$personnel.txt$content$role == "PI"][1],
      ": ",
      x1$x$template$personnel.txt$content$fundingNumber[
        x1$x$template$personnel.txt$content$role == "PI"][1]))
  expect_equal(
    r$dataset$project$relatedProject[[1]]$title,
    "Decomposition in the real world")
  expect_equal(
    r$dataset$project$relatedProject[[1]]$funding,
    "NSF: 111001")
  expect_equal(
    r$dataset$project$relatedProject[[2]]$title,
    "No project title to report")
  expect_equal(
    r$dataset$project$relatedProject[[2]]$funding,
    "NSF: 101")
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

  
})



# Test usage with x (all templates and 2 data tables) -------------------------

testthat::test_that('Test usage with x (all templates and 2 data tables)', {
  
  # Using deprecated template bounding_boxes.txt results in warning
  
  input <- x_table
  input$template$geographic_coverage.txt <- NULL
  
  expect_warning(
    make_eml(
      data.path = data.path,
      eml.path = eml.path,
      dataset.title = 'Sphagnum and Vascular Plant Decomposition under Increasing Nitrogen Additions: 2014-2015',
      data.table = data.table,
      data.table.name = data.table.name,
      data.table.description = c('Decomposition data', 'Nitrogen data'),
      data.table.quote.character = c("\"","\""),
      data.url = data.url,
      temporal.coverage = c('2014-05-01', '2015-10-31'),
      maintenance.description = 'completed',
      user.id = user.id,
      user.domain = user.domain,
      package.id = 'edi.141.1',
      write.file = FALSE,
      x = input
    )
  )
  
  # Remove deprecated template
  
  x_table$template$bounding_boxes.txt <- NULL

  # Missing path has no effect
  
  output <- make_eml(
    data.path = data.path,
    eml.path = eml.path,
    dataset.title = 'Sphagnum and Vascular Plant Decomposition under Increasing Nitrogen Additions: 2014-2015',
    data.table = data.table,
    data.table.name = data.table.name,
    data.table.description = c('Decomposition data', 'Nitrogen data'),
    data.table.quote.character = c("\"","\""),
    data.url = data.url,
    temporal.coverage = c('2014-05-01', '2015-10-31'),
    maintenance.description = 'completed',
    user.id = user.id,
    user.domain = user.domain,
    package.id = 'edi.141.1',
    return.obj = TRUE,
    write.file = FALSE,
    x = x_table
  )
  
  expect_equal(
    class(output) == 'list',
    TRUE
  )
  
  # Missing path and eml.path has no effect if write.file = FALSE
  
  output <- make_eml(
    data.path = data.path,
    dataset.title = 'Sphagnum and Vascular Plant Decomposition under Increasing Nitrogen Additions: 2014-2015',
    data.table = data.table,
    data.table.name = data.table.name,
    data.table.description = c('Decomposition data', 'Nitrogen data'),
    data.table.quote.character = c("\"","\""),
    data.url = data.url,
    temporal.coverage = c('2014-05-01', '2015-10-31'),
    maintenance.description = 'completed',
    user.id = user.id,
    user.domain = user.domain,
    package.id = 'edi.141.1',
    return.obj = TRUE,
    write.file = FALSE,
    x = x_table
  )
  
  expect_equal(
    class(output) == 'list',
    TRUE
  )
  
  # Missing eml.path and write.file = TRUE results in error
  
  expect_error(
    suppressMessages(
      make_eml(
        data.path = data.path,
        dataset.title = 'Sphagnum and Vascular Plant Decomposition under Increasing Nitrogen Additions: 2014-2015',
        data.table = data.table,
        data.table.name = data.table.name,
        data.table.description = c('Decomposition data', 'Nitrogen data'),
        data.table.quote.character = c("\"","\""),
        data.url = data.url,
        temporal.coverage = c('2014-05-01', '2015-10-31'),
        geographic.description = 'Alberta, Canada, 100 km south of Fort McMurray, Canada',
        geographic.coordinates = c('55.895', '112.094','55.895', '112.094'),
        maintenance.description = 'completed',
        user.id = user.id,
        user.domain = user.domain,
        package.id = 'edi.141.1',
        return.obj = TRUE,
        write.file = TRUE,
        x = x_table
      )
    )
  )
  
  # Missing eml.path and write.file = FALSE has no effect
  
  output <- make_eml(
    data.path = data.path,
    dataset.title = 'Sphagnum and Vascular Plant Decomposition under Increasing Nitrogen Additions: 2014-2015',
    data.table = data.table,
    data.table.name = data.table.name,
    data.table.description = c('Decomposition data', 'Nitrogen data'),
    data.table.quote.character = c("\"","\""),
    data.url = data.url,
    temporal.coverage = c('2014-05-01', '2015-10-31'),
    maintenance.description = 'completed',
    user.id = user.id,
    user.domain = user.domain,
    package.id = 'edi.141.1',
    return.obj = TRUE,
    write.file = FALSE,
    x = x_table
  )
  
  expect_equal(
    class(output) == 'list',
    TRUE
  )

  # Arguments supplied to function in long form
  
  output <- make_eml(
    path = path,
    data.path = data.path,
    eml.path = eml.path,
    dataset.title = 'Sphagnum and Vascular Plant Decomposition under Increasing Nitrogen Additions: 2014-2015',
    data.table = data.table,
    data.table.name = data.table.name,
    data.table.description = c('Decomposition data', 'Nitrogen data'),
    data.table.quote.character = c("\"","\""),
    data.url = data.url,
    temporal.coverage = c('2014-05-01', '2015-10-31'),
    maintenance.description = 'completed',
    user.id = user.id,
    user.domain = user.domain,
    package.id = 'edi.141.1',
    return.obj = TRUE,
    write.file = FALSE,
    x = x_table
  )
  
  expect_equal(
    class(output) == 'list',
    TRUE
  )
  
  # Arguments supplied to function via x
  
  output <- suppressWarnings(
    do.call(
      make_eml, 
      x_table_docall[
        names(x_table_docall) %in% names(formals(make_eml))
      ]
    )
  )
  
  expect_equal(
    class(output) == 'list',
    TRUE
  )
  
  # New geographic_coverage.txt is supported
  
  input <- x_table

  input$template$geographic_coverage.txt <- NULL
  
  input <- suppressMessages(
    template_geographic_coverage(
      data.table = 'nitrogen.csv', 
      site.col = 'site_name', 
      lat.col = 'site_lat',
      lon.col = 'site_lon',
      x = input,
      write.file = FALSE
    ) 
  )
  
  input$template$bounding_boxes.txt <- NULL
    
  output <- make_eml(
    path = path,
    data.path = data.path,
    eml.path = eml.path,
    dataset.title = 'Sphagnum and Vascular Plant Decomposition under Increasing Nitrogen Additions: 2014-2015',
    data.table = data.table,
    data.table.name = data.table.name,
    data.table.description = c('Decomposition data', 'Nitrogen data'),
    data.table.quote.character = c("\"","\""),
    data.url = data.url,
    temporal.coverage = c('2014-05-01', '2015-10-31'),
    maintenance.description = 'completed',
    user.id = user.id,
    user.domain = user.domain,
    package.id = 'edi.141.1',
    return.obj = TRUE,
    write.file = FALSE,
    x = input
  )
  
  expect_equal(
    length(output$dataset$coverage$geographicCoverage) > 5,
    TRUE
  )
  
  
  
})

# Test usage with x (all templates, 2 data tables, and 1 other entity) --------

testthat::test_that('Test usage with x (all templates, 2 data tables, and 1 other entity)', {
  
  # Use of deprecated template bounding_boxes.txt results in warning
  
  input <- x_table_other
  input$template$geographic_coverage.txt <- NULL
  
  expect_warning(
    make_eml(
      path = system.file('/examples/templates_new_geocoverage', package = 'EMLassemblyline'),
      data.path = data.path,
      eml.path = eml.path,
      dataset.title = 'Sphagnum and Vascular Plant Decomposition under Increasing Nitrogen Additions: 2014-2015',
      data.table = data.table,
      data.table.name = data.table.name,
      data.table.description = c('Decomposition data', 'Nitrogen data'),
      data.table.quote.character = c("\"","\""),
      data.url = data.url,
      other.entity = 'ancillary_data.zip',
      other.entity.description = 'Ancillary data',
      temporal.coverage = c('2014-05-01', '2015-10-31'),
      maintenance.description = 'completed',
      user.id = user.id,
      user.domain = user.domain,
      package.id = 'edi.141.1',
      return.obj = FALSE,
      write.file = FALSE,
      x = input
    )
  )
  
  # Remove deprecated template bounding_boxes.txt
  
  x_table_other$template$bounding_boxes.txt <- NULL
  
  # Arguments supplied to function in long form
  
  output <- make_eml(
    path = path,
    data.path = data.path,
    eml.path = eml.path,
    dataset.title = 'Sphagnum and Vascular Plant Decomposition under Increasing Nitrogen Additions: 2014-2015',
    data.table = data.table,
    data.table.name = data.table.name,
    data.table.description = c('Decomposition data', 'Nitrogen data'),
    data.table.quote.character = c("\"","\""),
    data.url = data.url,
    other.entity = 'ancillary_data.zip',
    other.entity.name = other.entity.name,
    other.entity.description = 'Ancillary data',
    temporal.coverage = c('2014-05-01', '2015-10-31'),
    maintenance.description = 'completed',
    user.id = user.id,
    user.domain = user.domain,
    package.id = 'edi.141.1',
    return.obj = TRUE,
    write.file = FALSE,
    x = x_table_other
  )
  
  expect_equal(
    class(output) == 'list',
    TRUE
  )

  output <- make_eml(
    path = path,
    data.path = data.path,
    eml.path = eml.path,
    dataset.title = 'Sphagnum and Vascular Plant Decomposition under Increasing Nitrogen Additions: 2014-2015',
    data.table = data.table,
    data.table.name = data.table.name,
    data.table.description = c('Decomposition data', 'Nitrogen data'),
    data.table.quote.character = c("\"","\""),
    data.url = data.url,
    other.entity = 'ancillary_data.zip',
    other.entity.name = other.entity.name,
    other.entity.description = 'Ancillary data',
    temporal.coverage = c('2014-05-01', '2015-10-31'),
    maintenance.description = 'completed',
    user.id = user.id,
    user.domain = user.domain,
    package.id = 'edi.141.1',
    return.obj = TRUE,
    write.file = FALSE,
    x = x_table_other
  )
  
  expect_equal(
    class(output) == 'list',
    TRUE
  )
  
  # Arguments supplied to function via x
  
  output <- suppressWarnings(
    do.call(
      make_eml, 
      x_table_other_docall[
        names(x_table_other_docall) %in% names(formals(make_eml))
      ]
    )
  )
  
  expect_equal(
    class(output) == 'list',
    TRUE
  )
  
})

# Test usage with x (all templates and 1 other entity) ------------------------

testthat::test_that('Test usage with x (all templates and 1 other entity)', {
  
  # Use of deprecated template bounding_boxes.txt results in warning
  
  input <- x_other
  input$template$geographic_coverage.txt <- NULL
  
  expect_warning(
    make_eml(
      path = path,
      data.path = data.path,
      eml.path = eml.path,
      dataset.title = 'Sphagnum and Vascular Plant Decomposition under Increasing Nitrogen Additions: 2014-2015',
      data.url = data.url,
      other.entity = 'ancillary_data.zip',
      other.entity.description = 'Ancillary data',
      temporal.coverage = c('2014-05-01', '2015-10-31'),
      maintenance.description = 'completed',
      user.id = user.id,
      user.domain = user.domain,
      package.id = 'edi.141.1',
      return.obj = TRUE,
      write.file = FALSE,
      x = input
    )
  )
  
  # Remove deprecated template bounding_boxes.txt
  
  x_other$template$bounding_boxes.txt <- NULL
  
  # Arguments supplied to function in long form
  
  output <- make_eml(
    path = path,
    data.path = data.path,
    eml.path = eml.path,
    dataset.title = 'Sphagnum and Vascular Plant Decomposition under Increasing Nitrogen Additions: 2014-2015',
    data.url = data.url,
    other.entity = 'ancillary_data.zip',
    other.entity.name = other.entity.name,
    other.entity.description = 'Ancillary data',
    temporal.coverage = c('2014-05-01', '2015-10-31'),
    maintenance.description = 'completed',
    user.id = user.id,
    user.domain = user.domain,
    package.id = 'edi.141.1',
    return.obj = TRUE,
    write.file = FALSE,
    x = x_other
  )
  
  expect_equal(
    class(output) == 'list',
    TRUE
  )
  
  # Arguments supplied to function via x
  
  output <- suppressWarnings(
    do.call(
      make_eml, 
      x_other_docall[
        names(x_other_docall) %in% names(formals(make_eml))
      ]
    )
  )
  
  expect_equal(
    class(output) == 'list',
    TRUE
  )
  
})

# Missing value codes should not be mandatory ---------------------------------
# Methods for handling datasets without missing value codes changed in the 
# recent EML v2.0.0 dependency.

testthat::test_that('Missing value codes are not required', {
  
  # Remove missing value codes and explanations in attributes_decomp.csv with 
  # empty fields
  
  x_missing_value_codes_empty <- x_table
  
  x_missing_value_codes_empty$template$attributes_decomp.txt$content$missingValueCode <- ''
  x_missing_value_codes_empty$template$attributes_decomp.txt$content$missingValueCodeExplanation <- ''
  x_missing_value_codes_empty$template$bounding_boxes.txt <- NULL
  
  output <- suppressMessages(
    make_eml(
      data.path = data.path,
      eml.path = eml.path,
      dataset.title = 'Sphagnum and Vascular Plant Decomposition under Increasing Nitrogen Additions: 2014-2015',
      data.table = data.table,
      data.table.name = data.table.name,
      data.table.description = c('Decomposition data', 'Nitrogen data'),
      temporal.coverage = c('2014-05-01', '2015-10-31'),
      maintenance.description = 'completed',
      user.id = user.id,
      user.domain = user.domain,
      package.id = 'edi.141.1',
      write.file = FALSE,
      return.obj = TRUE,
      x = x_missing_value_codes_empty
    )
  )
  
  expect_true(
    all(is.na(output$dataset$dataTable[[1]]$attributeList$attribute[[6]]$missingValueCode))
  )
  
})

# geographicCoverage ----------------------------------------------------------

testthat::test_that('geographicCoverage', {
  
  # Error if more than one source of geographicCoverage
  # 3 sources (geographic.coverage, geographic_coverage.txt, 
  # bounding_boxes.txt)
  
  x_geographicCoverage <- x_table
  x_geographicCoverage$template$bounding_boxes.txt$content <- data.frame(
    geographicDescription = c('site1', 'site2'),
    northBoundingCoordinate = c('45', '43'),
    southBoundingCoordinate = c('30', '33'),
    eastBoundingCoordinate = c('-23', '-43'),
    westBoundingCoordinate = c('-123', '-123'),
    stringsAsFactors = F
  )
  x_geographicCoverage$template$geographic_coverage.txt$content <- data.frame(
    geographicDescription = c('site1', 'site2'),
    northBoundingCoordinate = c('45', '43'),
    southBoundingCoordinate = c('30', '33'),
    eastBoundingCoordinate = c('-23', '-43'),
    westBoundingCoordinate = c('-123', '-123'),
    stringsAsFactors = F
  )
  
  expect_error(
    suppressMessages(
      make_eml(
        data.path = data.path,
        eml.path = eml.path,
        dataset.title = 'Sphagnum and Vascular Plant Decomposition under Increasing Nitrogen Additions: 2014-2015',
        data.table = data.table,
        data.table.name = data.table.name,
        data.table.description = c('Decomposition data', 'Nitrogen data'),
        temporal.coverage = c('2014-05-01', '2015-10-31'),
        geographic.description = 'Alberta, Canada, 100 km south of Fort McMurray, Canada',
        geographic.coordinates = c('55.895', '112.094','55.895', '112.094'),
        maintenance.description = 'completed',
        user.id = user.id,
        user.domain = user.domain,
        package.id = 'edi.141.1',
        write.file = FALSE,
        x = x_geographicCoverage
      )
    )
  )
  
  # Error if more than one source of geographicCoverage
  # 2 sources (geographic.coverage, geographic_coverage.txt)
  
  x_geographicCoverage$template$bounding_boxes.txt$content <- NULL

  expect_error(
    suppressMessages(
      make_eml(
        data.path = data.path,
        eml.path = eml.path,
        dataset.title = 'Sphagnum and Vascular Plant Decomposition under Increasing Nitrogen Additions: 2014-2015',
        data.table = data.table,
        data.table.name = data.table.name,
        data.table.description = c('Decomposition data', 'Nitrogen data'),
        temporal.coverage = c('2014-05-01', '2015-10-31'),
        geographic.description = 'Alberta, Canada, 100 km south of Fort McMurray, Canada',
        geographic.coordinates = c('55.895', '112.094','55.895', '112.094'),
        maintenance.description = 'completed',
        user.id = user.id,
        user.domain = user.domain,
        package.id = 'edi.141.1',
        write.file = FALSE,
        x = x_geographicCoverage
      )
    )
  )
  
  # Create geographic coverage from geographic_coverage.txt

  output <- suppressMessages(
    make_eml(
      data.path = data.path,
      eml.path = eml.path,
      dataset.title = 'Sphagnum and Vascular Plant Decomposition under Increasing Nitrogen Additions: 2014-2015',
      data.table = data.table,
      data.table.name = data.table.name,
      data.table.description = c('Decomposition data', 'Nitrogen data'),
      temporal.coverage = c('2014-05-01', '2015-10-31'),
      maintenance.description = 'completed',
      user.id = user.id,
      user.domain = user.domain,
      package.id = 'edi.141.1',
      write.file = FALSE,
      x = x_geographicCoverage,
      return.obj = TRUE
    )
  )
  
  expect_true(
    length(output$dataset$coverage$geographicCoverage) >= 1
  )
  
  # Create geographic coverage from geographic.coordinates
  
  x_geographicCoverage$template$geographic_coverage.txt$content <- NULL
  
  output <- suppressMessages(
    make_eml(
      data.path = data.path,
      eml.path = eml.path,
      dataset.title = 'Sphagnum and Vascular Plant Decomposition under Increasing Nitrogen Additions: 2014-2015',
      data.table = data.table,
      data.table.name = data.table.name,
      data.table.description = c('Decomposition data', 'Nitrogen data'),
      temporal.coverage = c('2014-05-01', '2015-10-31'),
      geographic.description = 'Alberta, Canada, 100 km south of Fort McMurray, Canada',
      geographic.coordinates = c('55.895', '112.094','55.895', '112.094'),
      maintenance.description = 'completed',
      user.id = user.id,
      user.domain = user.domain,
      package.id = 'edi.141.1',
      write.file = FALSE,
      x = x_geographicCoverage,
      return.obj = TRUE
    )
  )
  
  expect_true(
    length(output$dataset$coverage$geographicCoverage) >= 1
  )
  
  # Create geographic coverage from bounding_boxes.txt
  
  x_geographicCoverage$template$bounding_boxes.txt$content <- data.frame(
    geographicDescription = c('site1', 'site2'),
    northBoundingCoordinate = c('45', '43'),
    southBoundingCoordinate = c('30', '33'),
    eastBoundingCoordinate = c('-23', '-43'),
    westBoundingCoordinate = c('-123', '-123'),
    stringsAsFactors = F
  )
  
  output <- suppressMessages(
    suppressWarnings(
      make_eml(
        data.path = data.path,
        eml.path = eml.path,
        dataset.title = 'Sphagnum and Vascular Plant Decomposition under Increasing Nitrogen Additions: 2014-2015',
        data.table = data.table,
        data.table.name = data.table.name,
        data.table.description = c('Decomposition data', 'Nitrogen data'),
        temporal.coverage = c('2014-05-01', '2015-10-31'),
        maintenance.description = 'completed',
        user.id = user.id,
        user.domain = user.domain,
        package.id = 'edi.141.1',
        write.file = FALSE,
        x = x_geographicCoverage,
        return.obj = TRUE
      )
    )
  )
  
  expect_true(
    length(output$dataset$coverage$geographicCoverage) >= 1
  )

})


# data.table.url & other.entity.url -------------------------------------------

testthat::test_that('data.table.url and other.entity.url', {
  
  file.copy(
    from  = system.file('/examples/pkg_260', package = 'EMLassemblyline'),
    to = tempdir(),
    recursive = TRUE)
  
  expect_error(
    make_eml(
      path = paste0(tempdir(), "/pkg_260/metadata_templates"),
      data.path = paste0(tempdir(), "/pkg_260/data_objects"),
      eml.path = paste0(tempdir(), "/pkg_260/eml"),
      dataset.title = 'Sphagnum and Vascular Plant Decomposition under Increasing Nitrogen Additions: 2014-2015',
      data.table = c("decomp.csv", "nitrogen.csv"),
      data.table.name = c("decomp table name", "nitrogen table name"),
      data.table.description = c('Decomposition data', 'Nitrogen data'),
      data.table.url = c("https://datatableurl_1"),
      other.entity = c("ancillary_data.zip", "processing_and_analysis.R"),
      other.entity.name = c("Ancillary data", "Processing and analysis script"),
      other.entity.description = c("Ancillary data description", "Processing and analysis script description"),
      other.entity.url = c("https://otherentityurl_1", "https://otherentityurl_2"),
      temporal.coverage = c('2014-05-01', '2015-10-31'),
      maintenance.description = 'completed',
      user.id = "userid",
      user.domain = "user.domain",
      package.id = 'edi.141.1',
      write.file = F,
      return.obj = F
    )
  )
  
  expect_error(
    make_eml(
      path = paste0(tempdir(), "/pkg_260/metadata_templates"),
      data.path = paste0(tempdir(), "/pkg_260/data_objects"),
      eml.path = paste0(tempdir(), "/pkg_260/eml"),
      dataset.title = 'Sphagnum and Vascular Plant Decomposition under Increasing Nitrogen Additions: 2014-2015',
      data.table = c("decomp.csv", "nitrogen.csv"),
      data.table.name = c("decomp table name", "nitrogen table name"),
      data.table.description = c('Decomposition data', 'Nitrogen data'),
      data.table.url = c("https://datatableurl_1", "https://datatableurl_2"),
      other.entity = c("ancillary_data.zip", "processing_and_analysis.R"),
      other.entity.name = c("Ancillary data", "Processing and analysis script"),
      other.entity.description = c("Ancillary data description", "Processing and analysis script description"),
      other.entity.url = c("https://otherentityurl_1"),
      temporal.coverage = c('2014-05-01', '2015-10-31'),
      maintenance.description = 'completed',
      user.id = "userid",
      user.domain = "user.domain",
      package.id = 'edi.141.1',
      write.file = F,
      return.obj = F
    )
  )
  
  output <- make_eml(
    path = paste0(tempdir(), "/pkg_260/metadata_templates"),
    data.path = paste0(tempdir(), "/pkg_260/data_objects"),
    eml.path = paste0(tempdir(), "/pkg_260/eml"),
    dataset.title = 'Sphagnum and Vascular Plant Decomposition under Increasing Nitrogen Additions: 2014-2015',
    data.table = c("decomp.csv", "nitrogen.csv"),
    data.table.name = c("decomp table name", "nitrogen table name"),
    data.table.description = c('Decomposition data', 'Nitrogen data'),
    data.table.url = c("https://datatableurl_1", "https://datatableurl_2"),
    other.entity = c("ancillary_data.zip", "processing_and_analysis.R"),
    other.entity.name = c("Ancillary data", "Processing and analysis script"),
    other.entity.description = c("Ancillary data description", "Processing and analysis script description"),
    other.entity.url = c("https://otherentityurl_1", "https://otherentityurl_2"),
    temporal.coverage = c('2014-05-01', '2015-10-31'),
    maintenance.description = 'completed',
    user.id = "userid",
    user.domain = "user.domain",
    package.id = 'edi.141.1',
    write.file = F,
    return.obj = T
  )
  
  expect_true(
    output$dataset$dataTable[[1]]$physical$distribution$online$url ==
      "https://datatableurl_1")
  expect_true(
    output$dataset$dataTable[[2]]$physical$distribution$online$url ==
      "https://datatableurl_2")
  expect_true(
    output$dataset$otherEntity[[1]]$physical$distribution$online$url ==
      "https://otherentityurl_1")
  expect_true(
    output$dataset$otherEntity[[2]]$physical$distribution$online$url ==
      "https://otherentityurl_2")
  
  unlink(paste0(tempdir(), "/pkg_260/metadata_templates"), recursive = T, force = T)
  
})

