context('Test data package evaluation')
library(EDIutils)

testthat::test_that('Invalid request results in error', {
  
  path <- system.file('edi.151.4.xml', package = 'EDIutils')
  path <- substr(path, 1, nchar(path)-14)
  
  expect_error(
    pkg_evaluate(path = path, package.id = 'edi.151.4', environment = 'staging',
               user.id = 'myuserid', user.pass = 'mypassword', 
               affiliation = 'LTER')
  )
  
})
