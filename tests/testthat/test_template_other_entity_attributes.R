context("template_other_entity_attributes()")
library(EMLassemblyline)


testthat::test_that("Templates can be returned as a list of data frames.", {
  files <- c("file1.pdf", "file2.R")
  res <- template_other_entity_attributes(
    other.entity = files,
    write.file = FALSE
  )
  expect_equal(typeof(res), "list")
  expect_true(
    setequal(
      x = names(res),
      y = name_attribute_templates(files)
    )
  )
  for (r in res) {
    expect_equal(class(r[1]), "data.frame")
  }
})


testthat::test_that("Templates can be returned as files.", {
  
  # dirpath <- paste0(tempdir(), "/templates")
  # dir.create(dirpath, showWarnings = FALSE)
  files <- c("file1.pdf", "file2.R")
  expected <- name_attribute_templates(files)
  on.exit(unlink(paste0(tempdir(), '/', expected), force = TRUE))
  
  res <- template_other_entity_attributes(
    path = tempdir(),
    other.entity = files,
    write.file = TRUE
  )
  expect_true(all(is.element(expected, dir(tempdir()))))
})
