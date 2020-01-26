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
  
  # eml (legacy eml)
  
  expect_error(
    validate_arguments(
      fun.name = 'template_annotations',
      fun.args = list(
        path = tempdir(),
        eml = "This is not the expected emld list object."
      )
    )
  )

})

testthat::test_that('annotate_eml()', {
  
  # annotations
  
  expect_error(
    validate_arguments(
      fun.name = 'annotate_eml',
      fun.args = list(
        annotations = NULL
      )
    )
  )
  
  expect_error(
    validate_arguments(
      fun.name = 'annotate_eml',
      fun.args = list(
        annotations = "/a/non/existant/path"
      )
    )
  )
  
  expect_error(
    validate_arguments(
      fun.name = 'annotate_eml',
      fun.args = list(
        annotations = list(
          data.table::fread(
            system.file(
              "/examples/pkg_260/metadata_templates/annotations.txt", 
              package = "EMLassemblyline"
            )
          )
        )
      )
    )
  )
  
  expect_error(
    validate_arguments(
      fun.name = 'annotate_eml',
      fun.args = list(
        annotations = system.file(
          "/examples/pkg_260/metadata_templates/annotations.txt", 
          package = "EMLassemblyline"
        )
      )
    )
  )
  
  # eml.in
  
  expect_error(
    validate_arguments(
      fun.name = 'annotate_eml',
      fun.args = list(
        annotations = system.file(
          "/examples/pkg_260/metadata_templates/annotations.txt", 
          package = "EMLassemblyline"
        ),
        eml.in = "/an/invalid/eml/file"
      )
    )
  )
  
  expect_error(
    validate_arguments(
      fun.name = 'annotate_eml',
      fun.args = list(
        annotations = system.file(
          "/examples/pkg_260/metadata_templates/annotations.txt", 
          package = "EMLassemblyline"
        ),
        eml.in = system.file(
          "/examples/eml/edi.260.3.xml", 
          package = "EMLassemblyline"
        )
      )
    )
  )
  
  # eml.out
  
  expect_error(
    validate_arguments(
      fun.name = 'annotate_eml',
      fun.args = list(
        annotations = system.file(
          "/examples/pkg_260/metadata_templates/annotations.txt", 
          package = "EMLassemblyline"
        ),
        eml.in = system.file(
          "/examples/eml/edi.260.3.xml", 
          package = "EMLassemblyline"
        ),
        eml.out = "/an/invalid/path/for/new/eml"
      )
    )
  )
  
})