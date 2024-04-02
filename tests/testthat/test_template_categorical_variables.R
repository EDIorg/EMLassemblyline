
context("Create categorical variables template")
library(EMLassemblyline)


testthat::test_that("Test usage with file inputs", {
  
  # write.file = TRUE writes files to path
  
  file.copy(
    from = system.file(
      '/examples/templates',
      package = 'EMLassemblyline'
    ),
    to = tempdir(),
    recursive = TRUE
  )
  
  file.remove(
    paste0(
      tempdir(),
      '/templates/catvars_decomp.txt'
    )
  )
  
  expect_message(
    suppressWarnings(
      template_categorical_variables(
        path = paste0(
          tempdir(),
          "/templates"
        ), 
        data.path = system.file(
          "/examples/data",
          package = "EMLassemblyline"
        ),
        write.file = TRUE
      ) 
    )
  )
  
  # Correct argument use results in messages
  
  expect_message(
    suppressWarnings(
      template_categorical_variables(
        path = system.file(
          "/examples/templates",
          package = "EMLassemblyline"
        ), 
        data.path = system.file(
          "/examples/data",
          package = "EMLassemblyline"
        ),
        write.file = FALSE
      )
    )
  )
  
  # Missing path results in error
  
  expect_error(
    suppressMessages(
      template_categorical_variables(
        data.path = system.file(
          "/examples/data",
          package = "EMLassemblyline"
        ),
        write.file = FALSE
      )
    )
  )
  
  # Missing attribute files from path
  
  expect_error(
    suppressMessages(
      template_categorical_variables(
        path = system.file(
          "/examples/data",
          package = "EMLassemblyline"
        ),
        data.path = system.file(
          "/examples/data",
          package = "EMLassemblyline"
        ),
        write.file = FALSE
      )
    )
  )
  
})


testthat::test_that("Missing value codes should not be listed as catvars.", {
  
  x <- template_arguments(
    path = system.file(
      "/examples/templates", 
      package = "EMLassemblyline"),
    data.path = system.file(
      "/examples/data",
      package = "EMLassemblyline"),
    data.table = c(
      "decomp.csv",
      "nitrogen.csv"))$x
  
  # Helper function for testing this feature - Change missing value code in 
  # attributes.txt, add missing value code to data, write to file, run 
  # template_categorical variables(), check missing value codes are not 
  # included in categorical_variables.txt
  
  test_missing_value_code <- function(data, 
                                      attribute.template, 
                                      missing.value.code) {
    
    if (class(missing.value.code) == "numeric") {
      
      attribute.template$missingValueCode[
        attribute.template$class == "categorical"] <- as.character(
          missing.value.code
        )
      data[1:5, colnames(data)[
        attribute.template$class == "categorical"]] <- missing.value.code
      dir.create(paste0(tempdir(), "/catvars_test"))
      
      data.table::fwrite(
        x = attribute.template,
        file = paste0(tempdir(), "/catvars_test/attributes_decomp.txt"),
        sep = "\t",
        quote = FALSE)
      
      write.csv(
        data,
        paste0(tempdir(), "/catvars_test/decomp.csv"),
        row.names = FALSE)
      template_categorical_variables(
        path = paste0(tempdir(), "/catvars_test"))
      
      t <- as.data.frame(
        data.table::fread(
          file = paste0(tempdir(), "/catvars_test/catvars_decomp.txt"),
          fill = TRUE,
          blank.lines.skip = TRUE,
          sep = "\t",
          quote = "",
          colClasses = list(
            character = 1:utils::count.fields(
              paste0(
                tempdir(), 
                "/catvars_test/catvars_decomp.txt"
              ), 
              sep = "\t")[1]
          )
        )
      )
      
      expect_true(!any(missing.value.code %in% t$code))
      
    } else if (class(missing.value.code) == "character") {
      
      attribute.template$missingValueCode[
        attribute.template$class == "categorical"] <- missing.value.code
      data[1:5, colnames(data)[
        attribute.template$class == "categorical"]] <- missing.value.code
      dir.create(paste0(tempdir(), "/catvars_test"))
      
      data.table::fwrite(
        x = attribute.template,
        file = paste0(tempdir(), "/catvars_test/attributes_decomp.txt"),
        sep = "\t",
        quote = FALSE)
      
      write.csv(
        data,
        paste0(tempdir(), "/catvars_test/decomp.csv"),
        row.names = FALSE)
      template_categorical_variables(
        path = paste0(tempdir(), "/catvars_test"))
      
      t <- as.data.frame(
        data.table::fread(
          file = paste0(tempdir(), "/catvars_test/catvars_decomp.txt"),
          fill = TRUE,
          blank.lines.skip = TRUE,
          sep = "\t",
          quote = "",
          colClasses = list(
            character = 1:utils::count.fields(
              paste0(
                tempdir(), 
                "/catvars_test/catvars_decomp.txt"
              ), 
              sep = "\t")[1]
          )
        )
      )
      
      expect_true(!any(missing.value.code %in% t$code))
      
    } else if (missing.value.code == "NA") {
      
      attribute.template$missingValueCode[
        attribute.template$class == "categorical"] <- missing.value.code
      data[1:5, colnames(data)[
        attribute.template$class == "categorical"]] <- NA
      dir.create(paste0(tempdir(), "/catvars_test"))
      
      data.table::fwrite(
        x = attribute.template,
        file = paste0(tempdir(), "/catvars_test/attributes_decomp.txt"),
        sep = "\t",
        quote = FALSE)
      
      write.csv(
        data,
        paste0(tempdir(), "/catvars_test/decomp.csv"),
        row.names = FALSE)
      template_categorical_variables(
        path = paste0(tempdir(), "/catvars_test"))
      
      t <- as.data.frame(
        data.table::fread(
          file = paste0(tempdir(), "/catvars_test/catvars_decomp.txt"),
          fill = TRUE,
          blank.lines.skip = TRUE,
          sep = "\t",
          quote = "",
          colClasses = list(
            character = 1:utils::count.fields(
              paste0(
                tempdir(), 
                "/catvars_test/catvars_decomp.txt"
              ), 
              sep = "\t")[1]
          )
        )
      )
      
      expect_true(!any(missing.value.code %in% t$code))
      
    }
    unlink(paste0(tempdir(), "/catvars_test"), recursive = TRUE, force = TRUE)
  }
  
  # Missing value code = -999 (interpreted as numeric within the data and 
  # character in attributes.txt)
  
  test_missing_value_code(
    data = x$data.table$decomp.csv$content,
    attribute.template = x$template$attributes_decomp.txt$content,
    missing.value.code = -999)
  
  # Missing value code = "No data"
  
  test_missing_value_code(
    data = x$data.table$decomp.csv$content,
    attribute.template = x$template$attributes_decomp.txt$content,
    missing.value.code = "No data")
  
  # Missing value code = "NA" (interpreted as NA in the data)
  
  test_missing_value_code(
    data = x$data.table$decomp.csv$content,
    attribute.template = x$template$attributes_decomp.txt$content,
    missing.value.code = "NA")

})


testthat::test_that("Templates can be returned as a list of data frames.", {
  # Setup
  testdir <- paste0(tempdir(), "/pkg")
  pkg_files <- copy_test_package(testdir)
  on.exit(unlink(testdir, recursive = TRUE, force = TRUE))
  catvars_files <- dir(testdir, pattern = "catvars", full.names = TRUE)
  file.remove(catvars_files)
  # Test
  res <- template_categorical_variables(path = testdir, write.file = FALSE)
  expect_equal(typeof(res), "list")
  expect_true(setequal(x = names(res), y = basename(catvars_files)))
  for (r in res) {
    expect_equal(class(r[1]), "data.frame")
  }
})


testthat::test_that("Templates can be returned as files.", {
  # Setup
  testdir <- paste0(tempdir(), "/pkg")
  pkg_files <- copy_test_package(testdir)
  on.exit(unlink(testdir, force = TRUE))
  catvars_files <- dir(testdir, pattern = "catvars", full.names = TRUE)
  file.remove(catvars_files)
  expected <- basename(catvars_files)
  # Test
  res <- template_categorical_variables(path = testdir, write.file = TRUE)
  expect_true(all(is.element(expected, dir(testdir))))
})


testthat::test_that("Return empty templates if instructed.", {
  # Setup
  testdir <- paste0(tempdir(), "/pkg")
  pkg_files <- copy_test_package(testdir)
  on.exit(unlink(testdir, recursive = TRUE, force = TRUE))
  catvars_files <- dir(testdir, pattern = "catvars", full.names = TRUE)
  file.remove(catvars_files)
  # Test
  res <- template_categorical_variables(
    path = testdir, 
    empty = TRUE,
    write.file = FALSE
  )
  expect_equal(typeof(res), "list")
  expect_true(setequal(x = names(res), y = basename(catvars_files)))
  for (r in res) {
    expect_equal(class(r[1]), "data.frame")
    expect_equal(nrow(r[1]), 0)
  }
})
