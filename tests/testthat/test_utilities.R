
context('utilities.R')
library(EMLassemblyline)

# set_methods_md() ------------------------------------------------------------

testthat::test_that('set_methods_md()', {
  # Parameterize
  myfile <- paste0(tempdir(), "/test.md")
  writeLines(text = "File content", con = myfile)
  node <- set_methods_md(myfile)
  # TEST: Text is wrapped in <markdown> and node has expected nesting
  expect_equal(names(node), "methodStep")
  expect_equal(names(node[[1]]), "description")
  expect_equal(names(node[[1]][[1]]), "markdown")
  # Clean up
  unlink(myfile)
})
