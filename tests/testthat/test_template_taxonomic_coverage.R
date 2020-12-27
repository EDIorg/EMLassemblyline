context('Create taxonomic coverage template')
library(EMLassemblyline)

# File inputs -----------------------------------------------------------------

testthat::test_that('File inputs', {
  
  # Standard usage results in messages, taxonomic_coverage.txt written to path,
  # and expected table attributes.
  
  # TODO: Mock up this test so slow API calls don't hang up Travis-CI
  # expect_message(
  #   template_taxonomic_coverage(
  #     path = tempdir(),
  #     data.path = system.file(
  #       '/examples/data',
  #       package = 'EMLassemblyline'
  #     ),
  #     taxa.table = 'decomp.csv',
  #     taxa.col = 'taxa',
  #     taxa.name.type = 'both',
  #     taxa.authority = c(3, 11),
  #     write.file = TRUE
  #   ) 
  # )
  # 
  # input <- utils::read.table(
  #   paste0(
  #     tempdir(), 
  #     '/taxonomic_coverage.txt'
  #   ),
  #   header = T,
  #   sep="\t",
  #   quote="\"",
  #   as.is=TRUE,
  #   comment.char = "",
  #   fill = T,
  #   na.strings = "NA",
  #   fileEncoding = "UTF-8"
  # )
  # 
  # expect_equal(
  #   class(input),
  #   'data.frame'
  # )
  # 
  # expect_true(
  #   all(
  #     colnames(input) %in% 
  #       c('name', 
  #         'name_type', 
  #         'name_resolved',
  #         'authority_system',
  #         'authority_id')
  #   )
  # )
  # 
  # unlink(
  #   paste0(
  #     tempdir(),
  #     '/taxonomic_coverage.txt'
  #   ),
  #   force = TRUE
  # )
  
  # Missing path results in error
  
  expect_error(
    suppressMessages(
      template_taxonomic_coverage(
        data.path = system.file(
          '/examples/data',
          package = 'EMLassemblyline'
        ),
        taxa.table = 'decomp.csv',
        taxa.col = 'taxa',
        taxa.name.type = 'scientific',
        taxa.authority = c(3, 11),
        write.file = FALSE)))

  # Missing data.path results in error
  
  expect_error(
    suppressMessages(
      template_taxonomic_coverage(
        path = tempdir(),
        taxa.table = 'decomp.csv',
        taxa.col = 'taxa',
        taxa.name.type = 'scientific',
        taxa.authority = c(3, 11),
        write.file = FALSE)))
  
  # Missing taxa.table results in error
  
  expect_error(
    suppressMessages(
      template_taxonomic_coverage(
        path = tempdir(),
        data.path = system.file(
          '/examples/data',
          package = 'EMLassemblyline'),
        taxa.col = 'taxa',
        taxa.name.type = 'scientific',
        taxa.authority = c(3, 11),
        write.file = FALSE)))
  
  # Incorrectly spelled taxa.table results in error
  
  expect_error(
    suppressMessages(
      template_taxonomic_coverage(
        path = tempdir(),
        data.path = system.file(
          '/examples/data',
          package = 'EMLassemblyline'
        ),
        taxa.table = 'decomppp.csv',
        taxa.col = 'taxa',
        taxa.name.type = 'scientific',
        taxa.authority = c(3, 11),
        write.file = FALSE)))
  
  # Missing taxa.col results in error
  
  expect_error(
    suppressMessages(
      template_taxonomic_coverage(
        path = tempdir(),
        data.path = system.file(
          '/examples/data',
          package = 'EMLassemblyline'
        ),
        taxa.table = 'decomp.csv',
        taxa.name.type = 'scientific',
        taxa.authority = c(3, 11),
        write.file = FALSE)))
  
  # Incorrectly spelled taxa.col results in error
  
  expect_error(
    suppressMessages(
      template_taxonomic_coverage(
        path = tempdir(),
        data.path = system.file(
          '/examples/data',
          package = 'EMLassemblyline'),
        taxa.table = 'decomp.csv',
        taxa.col = 'taxaaa',
        taxa.name.type = 'scientific',
        taxa.authority = c(3, 11),
        write.file = FALSE)))
  
  # Missing taxa.name.type results in error
  
  expect_error(
    suppressMessages(
      template_taxonomic_coverage(
        path = tempdir(),
        data.path = system.file(
          '/examples/data',
          package = 'EMLassemblyline'),
        taxa.table = 'decomp.csv',
        taxa.col = 'taxa',
        taxa.authority = c(3, 11),
        write.file = FALSE)))
  
  # Missing invalid taxa.name.type results in error
  
  expect_error(
    suppressMessages(
      template_taxonomic_coverage(
        path = tempdir(),
        data.path = system.file(
          '/examples/data',
          package = 'EMLassemblyline'
        ),
        taxa.table = 'decomp.csv',
        taxa.col = 'taxa',
        taxa.name.type = 'commontific',
        taxa.authority = c(3, 11),
        write.file = FALSE)))
  
  # Invalid taxa.authority results in error
  
  expect_error(
    suppressMessages(
      template_taxonomic_coverage(
        path = tempdir(),
        data.path = system.file(
          '/examples/data',
          package = 'EMLassemblyline'),
        taxa.table = 'decomp.csv',
        taxa.col = 'taxa',
        taxa.name.type = 'scientific',
        taxa.authority = 2112,
        write.file = FALSE)))
  
})




# Empty template --------------------------------------------------------------

testthat::test_that('Empty template', {
  
  unlink(
    paste0(tempdir(), "/taxonomic_coverage.txt"), recursive = T, force = T)
  
  r <- suppressMessages(
    template_taxonomic_coverage(
      path = tempdir(),
      empty = T))
  
  expect_true(is.data.frame(r))
  expect_true(nrow(r) == 0)
  expect_true(file.exists(paste0(tempdir(), "/taxonomic_coverage.txt")))
  
  unlink(
    paste0(tempdir(), "/taxonomic_coverage.txt"), recursive = T, force = T)
  
})


