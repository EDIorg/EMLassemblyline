context('validate_arguments()')
library(EMLassemblyline)

# template_annotations() ------------------------------------------------------

testthat::test_that('template_annotations()', {
  
  # Missing path
  
  expect_error(
    validate_arguments(
      fun.name = 'template_annotations',
      fun.args = list(path = NULL)
    )
  )
  
  # Invalid path
  
  expect_error(
    validate_arguments(
      fun.name = 'template_annotations',
      fun.args = list(path = "/an/invalid/path")
    )
  )
  
  # default.annotations
  
  df <- data.table::fread(
    system.file(
      "/templates/annotation_defaults.txt", 
      package = "EMLassemblyline"
    )
  )
  
  expect_null(
    validate_arguments(
      fun.name = 'template_annotations',
      fun.args = list(
        path = tempdir(),
        default.annotations = df
      )
    )
  )
  
  expect_error(
    validate_arguments(
      fun.name = 'template_annotations',
      fun.args = list(
        path = tempdir(),
        default.annotations = as.list(df)
      )
    )
  )
  
  expect_null(
    validate_arguments(
      fun.name = 'template_annotations',
      fun.args = list(
        path = tempdir(),
        default.annotations = df
      )
    )
  )
  
  df2 <- df
  colnames(df2) <- c("el", "predicate_label", "predicate_uri", "object_label", 
                    "object_uri")
  
  expect_error(
    validate_arguments(
      fun.name = 'template_annotations',
      fun.args = list(
        path = tempdir(),
        default.annotations = df2
      )
    )
  )
  
  rm(df2)
  rm(df)

})