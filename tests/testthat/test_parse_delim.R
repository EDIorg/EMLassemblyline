context('Detect record delimiters.')

library(EMLassemblyline)

# Build test data -------------------------------------------------------------

crlf <- c(
  'These character strings\r\n',
  'conclude in a carriage return plus line feed\r\n',
  ', which is used in the MD-DOS and Windows OS\r\n'
  )

cr <- c(
  'These character strings\r',
  'end in a carriage return\r',
  ', which is used in the Macintosh OS\r'
  )

lf <- c(
  'These character strings\n',
  'end in a carriage return\n',
  ', which is used in the UNIX OS\n'
  )


# Test ------------------------------------------------------------------------

testthat::test_that('Output should be carriage return + line feed.', {
  expect_equal(
    parse_delim(crlf),
    '\\r\\n'
  )
})


testthat::test_that('Output should be carriage return.', {
  expect_equal(
    parse_delim(cr),
    '\\r'
  )
})

testthat::test_that('Output should be line feed.', {
  expect_equal(
    parse_delim(lf),
    '\\n'
  )
})
