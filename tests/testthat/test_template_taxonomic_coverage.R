context('Create taxonomic coverage template')
library(EMLassemblyline)

# Test usage with file inputs -------------------------------------------------

testthat::test_that('Test usage with file inputs', {
  
  # Standard usage results in messages, taxonomic_coverage.txt written to path,
  # and expected table attributes.
  
  output <- template_taxonomic_coverage(
    path = tempdir(),
    data.path = system.file(
      '/examples/data',
      package = 'EMLassemblyline'
    ),
    taxa.table = 'decomp.csv',
    taxa.col = 'taxa',
    taxa.name.type = 'scientific',
    taxa.authority = c(3, 11),
    write.file = TRUE
  )
  
})

