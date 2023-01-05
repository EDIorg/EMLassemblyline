
context('Template arguments')
library(EMLassemblyline)

# Parameterize

# Read attributes of EMLassemblyline function arguments and templates

attr_args <- data.table::fread(
  file = system.file(
    '/templates/arguments.txt',
    package = 'EMLassemblyline'),
  fill = TRUE,
  blank.lines.skip = TRUE)

attr_tmp <- read_template_characteristics()

# List files at path and data.path

path_files <- list.files(
  system.file(
    '/examples/templates',
    package = 'EMLassemblyline'))

data_path_files <- list.files(
  system.file(
    '/examples/data',
    package = 'EMLassemblyline'))

# NULL inputs should assign NULL values to arguments names and the template 
# node.

testthat::test_that("Inputs = NULL", {
  
  output <- template_arguments()
  expect_true(class(output) == "list")
  expect_true(all(names(output) %in% attr_args$argument_name))
  expect_true(
    all(names(output$x) %in% c('template', 'data.table', 'other.entity', 'data.objects')))
  expect_true(is.null(output$x$template))
  expect_true(is.null(output$x$data.table))
  expect_true(is.null(output$x$other.entity))

})

# Inputs = missing templates
# A path without templates results in error.

testthat::test_that("Inputs = missing templates", {
  
  dir.create(paste0(tempdir(), "/empty"))
  file.create(paste0(tempdir(), "/empty/my.csv"))
  expect_error(template_arguments(path = paste0(tempdir(), "/empty")))
  unlink(paste0(tempdir(), "/empty/my.csv"))
  expect_error(template_arguments(path = paste0(tempdir(), "/empty")))
  unlink(paste0(tempdir(), "/empty"), force = T, recursive = T)

})

# Inputs = duplicate templates
# Duplicate templates result in error.

testthat::test_that("Inputs = duplicate templates", {
  
  file.copy(
    from  = system.file('/templates', package = 'EMLassemblyline'),
    to = tempdir(),
    recursive = T)
  expect_error(template_arguments(path = paste0(tempdir(), "/templates")))
  unlink(paste0(tempdir(), "/templates"), force = T, recursive = T)
  
  file.copy(
    from  = system.file('/templates', package = 'EMLassemblyline'),
    to = tempdir(),
    recursive = T)
  file.remove(paste0(tempdir(), "/templates/abstract.docx"))
  file.remove(paste0(tempdir(), "/templates/abstract.md"))
  file.remove(paste0(tempdir(), "/templates/additional_info.docx"))
  file.remove(paste0(tempdir(), "/templates/additional_info.md"))
  file.remove(paste0(tempdir(), "/templates/methods.docx"))
  file.remove(paste0(tempdir(), "/templates/methods.md"))
  file.copy(
    from  = system.file('/examples/pkg_260/metadata_templates/attributes_decomp.txt', package = 'EMLassemblyline'),
    to = paste0(tempdir(), "/templates"))
  file.copy(
    from  = system.file('/examples/pkg_260/metadata_templates/attributes_nitrogen.txt', package = 'EMLassemblyline'),
    to = paste0(tempdir(), "/templates"))
  file.copy(
    from  = system.file('/examples/pkg_260/metadata_templates/catvars_nitrogen.txt', package = 'EMLassemblyline'),
    to = paste0(tempdir(), "/templates"))
  file.copy(
    from  = system.file('/examples/pkg_260/metadata_templates/catvars_decomp.txt', package = 'EMLassemblyline'),
    to = paste0(tempdir(), "/templates"))
  output <- template_arguments(path = paste0(tempdir(), "/templates"))
  unlink(paste0(tempdir(), "/templates"), force = T, recursive = T)
  
})

# Inputs = empty templates
# Empty templates at path should be read into x

testthat::test_that("Inputs = empty templates", {
  
  file.copy(
    from  = system.file('/templates', package = 'EMLassemblyline'),
    to = tempdir(),
    recursive = T)
  file.remove(paste0(tempdir(), "/templates/abstract.docx"))
  file.remove(paste0(tempdir(), "/templates/abstract.md"))
  file.remove(paste0(tempdir(), "/templates/additional_info.docx"))
  file.remove(paste0(tempdir(), "/templates/additional_info.md"))
  file.remove(paste0(tempdir(), "/templates/methods.docx"))
  file.remove(paste0(tempdir(), "/templates/methods.md"))
  output <- template_arguments(path = paste0(tempdir(), "/templates"))
  
  expect_true(class(output) == "list")
  expect_true(all(names(output) %in% attr_args$argument_name))
  expect_true(
    all(names(output$x) %in% c('template', 'data.table', 'other.entity', 'data.objects')))
  
  tnames <- names(output$x$template)
  for (i in 1:length(tnames)) {
    
    is_text <- FALSE
    is_dataframe <- FALSE
    is_taxonomicCoverage <- FALSE
    is_text <- stringr::str_detect(
      tnames[i], 
      paste(
        attr_tmp$regexpr[
          (attr_tmp$type == "text")],
        collapse = "|"))
    is_dataframe <- stringr::str_detect(
      tnames[i], 
      paste(
        attr_tmp$regexpr[
          (attr_tmp$type == "table")],
        collapse = "|"))
    is_taxonomicCoverage <- stringr::str_detect(
      tnames[i], 
      paste(
        attr_tmp$regexpr[
          (attr_tmp$type == "xml")],
        collapse = "|"))
    
    if (is_text) {
      expect_true(is.list(output$x$template[[i]]$content))
    } else if (isTRUE(is_dataframe)) {
      expect_true(is.data.frame(output$x$template[[i]]$content))
    } else if (isTRUE(is_taxonomicCoverage)) {
      expect_true(is.data.frame(output$x$template[[i]]$content))
    }
    
  }
  
  unlink(paste0(tempdir(), "/templates"), force = T, recursive = T)

})

# Inputs = the 'empty' argument
# All templates should be added to the list object with empty values when the 
# argument empty = TRUE.

testthat::test_that("Inputs = the 'empty' argument", {
  
  output <- template_arguments(empty = T)
  
  expect_true(class(output) == "list")
  expect_true(all(names(output) %in% attr_args$argument_name))
  expect_true(
    all(names(output$x) %in% c('template', 'data.table', 'other.entity', 'data.objects')))
  
  expected_templates <- c(
    "abstract.txt", "additional_info.txt", "custom_units.txt", 
    "geographic_coverage.txt", "intellectual_rights.txt", "keywords.txt", 
    "methods.txt", "personnel.txt", "taxonomic_coverage.txt", 
    "bounding_boxes.txt")
  for (i in 1:length(names(output$x$template))) {
    is_text <- FALSE
    is_dataframe <- FALSE
    is_text <- stringr::str_detect(
      names(output$x$template)[i], 
      paste(
        attr_tmp$regexpr[
          (attr_tmp$type == "text")],
        collapse = "|"))
    is_dataframe <- stringr::str_detect(
      names(output$x$template)[i], 
      paste(
        attr_tmp$regexpr[
          (attr_tmp$type == "table")],
        collapse = "|"))
    if (is_text) {
      expect_true(is.list(output$x$template[[i]]$content))
    } else if (isTRUE(is_dataframe)) {
      expect_true(is.data.frame(output$x$template[[i]]$content))
    }
  }
  
})

# Inputs = completed templates
# Completed templates should be read into x

testthat::test_that("Inputs = empty templates", {
  
  # .txt abstract, methods, and additional_info
  
  file.copy(
    from  = system.file('/examples/pkg_260', package = 'EMLassemblyline'),
    to = tempdir(),
    recursive = T)
  output <- template_arguments(
    path = paste0(tempdir(), "/pkg_260/metadata_templates"))
  
  expect_true(class(output) == "list")
  expect_true(all(names(output) %in% attr_args$argument_name))
  expect_true(
    all(names(output$x) %in% c('template', 'data.table', 'other.entity', 'data.objects')))
  
  tnames <- names(output$x$template)
  for (i in 1:length(tnames)) {
    
    is_text <- FALSE
    is_dataframe <- FALSE
    is_taxonomicCoverage <- FALSE
    is_text <- stringr::str_detect(
      tnames[i], 
      paste(
        attr_tmp$regexpr[
          (attr_tmp$type == "text")],
        collapse = "|"))
    is_dataframe <- stringr::str_detect(
      tnames[i], 
      paste(
        attr_tmp$regexpr[
          (attr_tmp$type == "table")],
        collapse = "|"))
    is_taxonomicCoverage <- stringr::str_detect(
      tnames[i], 
      paste(
        attr_tmp$regexpr[
          (attr_tmp$type == "xml")],
        collapse = "|"))
    
    if (isTRUE(is_text)) {
      expect_true(is.list(output$x$template[[i]]$content))
    } else if (isTRUE(is_dataframe)) {
      expect_true(is.data.frame(output$x$template[[i]]$content))
    } else if (isTRUE(is_taxonomicCoverage)) {
      expect_true(is.data.frame(output$x$template[[i]]$content))
    }
    
  }
  
  unlink(paste0(tempdir(), "/pkg_260/metadata_templates"), force = T, recursive = T)
  
  # abstract.docx, methods.docx, and additional_info.docx
  
  file.copy(
    from  = system.file('/examples/templates_docx', package = 'EMLassemblyline'),
    to = tempdir(),
    recursive = T)
  output <- template_arguments(
    path = paste0(tempdir(), "/templates_docx"))
  
  expect_true(class(output) == "list")
  expect_true(all(names(output) %in% attr_args$argument_name))
  expect_true(
    all(names(output$x) %in% c('template', 'data.table', 'other.entity', 'data.objects')))
  
  tnames <- names(output$x$template)
  for (i in 1:length(tnames)) {
    
    is_text <- FALSE
    is_dataframe <- FALSE
    is_taxonomicCoverage <- FALSE
    is_text <- stringr::str_detect(
      tnames[i], 
      paste(
        attr_tmp$regexpr[
          (attr_tmp$type == "text")],
        collapse = "|"))
    is_dataframe <- stringr::str_detect(
      tnames[i], 
      paste(
        attr_tmp$regexpr[
          (attr_tmp$type == "table")],
        collapse = "|"))
    is_taxonomicCoverage <- stringr::str_detect(
      tnames[i], 
      paste(
        attr_tmp$regexpr[
          (attr_tmp$type == "xml")],
        collapse = "|"))
    
    if (isTRUE(is_text)) {
      expect_true(is.list(output$x$template[[i]]$content))
    } else if (isTRUE(is_dataframe)) {
      expect_true(is.data.frame(output$x$template[[i]]$content))
    } else if (isTRUE(is_taxonomicCoverage)) {
      expect_true(is.data.frame(output$x$template[[i]]$content))
    }
    
  }
  
  unlink(paste0(tempdir(), "/templates_docx"), force = T, recursive = T)
  
  # abstract.md, methods.md, and additional_info.md
  
  file.copy(
    from  = system.file('/examples/templates_md', package = 'EMLassemblyline'),
    to = tempdir(),
    recursive = T)
  output <- template_arguments(
    path = paste0(tempdir(), "/templates_md"))
  
  expect_true(class(output) == "list")
  expect_true(all(names(output) %in% attr_args$argument_name))
  expect_true(
    all(names(output$x) %in% c('template', 'data.table', 'other.entity', 'data.objects')))
  
  tnames <- names(output$x$template)
  for (i in 1:length(tnames)) {
    
    is_text <- FALSE
    is_dataframe <- FALSE
    is_taxonomicCoverage <- FALSE
    is_text <- stringr::str_detect(
      tnames[i], 
      paste(
        attr_tmp$regexpr[
          (attr_tmp$type == "text")],
        collapse = "|"))
    is_dataframe <- stringr::str_detect(
      tnames[i], 
      paste(
        attr_tmp$regexpr[
          (attr_tmp$type == "table")],
        collapse = "|"))
    is_taxonomicCoverage <- stringr::str_detect(
      tnames[i], 
      paste(
        attr_tmp$regexpr[
          (attr_tmp$type == "xml")],
        collapse = "|"))
    
    if (isTRUE(is_text)) {
      expect_true(is.list(output$x$template[[i]]$content))
    } else if (isTRUE(is_dataframe)) {
      expect_true(is.data.frame(output$x$template[[i]]$content))
    } else if (isTRUE(is_taxonomicCoverage)) {
      expect_true(is.data.frame(output$x$template[[i]]$content))
    }
    
  }
  
  unlink(paste0(tempdir(), "/templates_md"), force = T, recursive = T)
  
})

# Inputs = data tables

testthat::test_that("Inputs = data tables", {
  file.copy(
    from  = system.file('/examples/pkg_260', package = 'EMLassemblyline'),
    to = tempdir(),
    recursive = T)
  output <- template_arguments(
    data.path = paste0(tempdir(), "/pkg_260/data_objects"), 
    data.table = c("nitrogen.csv", "decomp.csv"))
  expect_true(class(output) == "list")
  expect_true(all(names(output) %in% attr_args$argument_name))
  expect_true(
    all(names(output$x) %in% c('template', 'data.table', 'other.entity', 'data.objects')))
  for (i in 1:length(names(output$x$data.table))) {
    expect_true(is.data.frame(output$x$data.table[[i]]$content))
    expect_true(ncol(output$x$data.table[[i]]$content) > 1)
    expect_true(nrow(output$x$data.table[[i]]$content) > 1)
  }
  unlink(paste0(tempdir(), "/pkg_260/data_objects"), force = T, recursive = T)
})

# Inputs = other entities

testthat::test_that("Inputs = other entities", {
  file.copy(
    from  = system.file('/examples/pkg_260', package = 'EMLassemblyline'),
    to = tempdir(),
    recursive = T)
  output <- template_arguments(
    data.path = paste0(tempdir(), "/pkg_260/data_objects"), 
    other.entity = c("ancillary_data.zip", "processing_and_analysis.R"))
  expect_true(class(output) == "list")
  expect_true(all(names(output) %in% attr_args$argument_name))
  expect_true(
    all(names(output$x) %in% c('template', 'data.table', 'other.entity', 'data.objects')))
  for (i in 1:length(names(output$x$other.entity))) {
    expect_true(is.na(output$x$other.entity[[i]]$content))
  }
  unlink(paste0(tempdir(), "/pkg_260/data_objects"), force = T, recursive = T)
})


testthat::test_that("data.object facilitates general data object reads.", {
  testdir <- paste0(tempdir(), "/pkg")
  files <- copy_test_package(testdir)
  data_objects <- c(
    "nitrogen.csv", 
    "decomp.csv",
    "ancillary_data.zip", 
    "processing_and_analysis.R"
  )
  res <- template_arguments(data.path = testdir, data.objects = data_objects)$x
  expected_names <- c("template", "data.table", "other.entity", "data.objects")
  expect_type(res, "list")
  expect_true(setequal(names(res), expected_names))
  expect_type(res$data.table, "NULL")
  expect_type(res$other.entity, "NULL")
  expect_type(res$data.objects, "list")
  expect_true(setequal(names(res$data.objects), data_objects))
  for (i in seq_along(res$data.objects)) {
    expect_true(!is.null(res$data.objects[[i]]$content))
    expect_true(!is.null(res$data.objects[[i]]$mime_type))
    expect_true(!is.null(res$data.objects[[i]]$file_path))
  }
  unlink(testdir, recursive = TRUE, force = TRUE)
})

