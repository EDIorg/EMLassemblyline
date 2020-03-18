context('Validate x')
library(EMLassemblyline)

# # Parameterize ----------------------------------------------------------------
# 
# # Get template file attributes
# 
# attr.templates <- read_template_attributes()
# 
# # Test usage with file inputs -------------------------------------------------
# 
# testthat::test_that('Test usage with file inputs', {
#   
#   # Correct usage results in messages (templates don't yet exist)
#   
#   expect_message(
#     import_templates(
#       path = system.file(
#         '/examples',
#         package = 'EMLassemblyline'
#       ),
#       data.path = system.file(
#         '/examples/data',
#         package = 'EMLassemblyline'
#       ),
#       license = 'CC0',
#       data.table = c(
#         'nitrogen.csv',
#         'decomp.csv'
#       ),
#       write.file = FALSE
#     )
#   )
#   
#   # Correct usage results in messages (templates already exist)
#   
#   expect_message(
#     import_templates(
#       path = system.file(
#         '/examples/templates',
#         package = 'EMLassemblyline'
#       ),
#       data.path = system.file(
#         '/examples/data',
#         package = 'EMLassemblyline'
#       ),
#       license = 'CC0',
#       data.table = c(
#         'nitrogen.csv',
#         'decomp.csv'
#       ),
#       write.file = FALSE
#     )
#   )
#   
#   # Missing path results in error
#   
#   expect_error(
#     suppressMessages(
#       import_templates(
#         data.path = system.file(
#           '/examples/data',
#           package = 'EMLassemblyline'
#         ),
#         license = 'CC0',
#         data.table = c(
#           'nitrogen.csv',
#           'decomp.csv'
#         ),
#         write.file = FALSE
#       )
#     )
#   )
#   
#   # Missing data.path with data.table results in error
#   
#   expect_error(
#     suppressMessages(
#       import_templates(
#         path = system.file(
#           '/examples',
#           package = 'EMLassemblyline'
#         ),
#         license = 'CC0',
#         data.table = c(
#           'nitrogen.csv',
#           'decomp.csv'
#         ),
#         write.file = FALSE
#       )
#     )
#   )
#   
#   # Missing data.path and data.table result in message
#   
#   expect_message(
#     import_templates(
#       path = system.file(
#         '/examples',
#         package = 'EMLassemblyline'
#       ),
#       license = 'CC0',
#       write.file = FALSE
#     )
#   )
#   
#   # Missing license results in error
#   
#   expect_error(
#     suppressMessages(
#       import_templates(
#         path = system.file(
#           '/examples',
#           package = 'EMLassemblyline'
#         ),
#         data.path = system.file(
#           '/examples/data',
#           package = 'EMLassemblyline'
#         ),
#         data.table = c(
#           'nitrogen.csv',
#           'decomp.csv'
#         ),
#         write.file = FALSE
#       )
#     )
#   )
#   
# })
# 
# # Test usage with x inputs (empty x) ------------------------------------------
# 
# testthat::test_that('Test usage with x inputs (empty x)', {
#   
#   # Create x with no templates or data
#   
#   x_empty <- read_files(
#     path = system.file(
#       '/examples', 
#       package = 'EMLassemblyline'
#     )
#   )
#   
#   # First import results in messages
#   
#   expect_message(
#     import_templates(
#       license = 'CC0',
#       x = x_empty,
#       write.file = FALSE
#     )
#   )
#   
#   # All arguments are supported:
#   # - /x/template/* is populated with template
#   # - content and paths are present
#   # - data.table and other.entity are NULL
#   
#   x_empty <- suppressMessages(
#     import_templates(
#       license = 'CC0',
#       x = x_empty,
#       write.file = FALSE
#     )
#   )
#   
#   for (i in 1:length(x_empty$template)){
#     
#     expect_equal(
#       any(names(x_empty$template[i]) %in% attr.templates$regexpr),
#       TRUE
#     )
#     
#     if (!names(x_empty$template[i]) %in% 
#         c('taxonomicCoverage.xml',
#           'geographic_coverage.txt')){
#       
#       expect_equal(
#         all(!is.na(x_empty$template[[i]]$content)),
#         TRUE
#       )
#       
#     }
#     
#     expect_equal(
#       is.na(x_empty$template[[i]]$path),
#       TRUE
#     )
#     
#   }
#   
#   # Second import notifies of existing content
#   
#   expect_message(
#     import_templates(
#       license = 'CC0',
#       x = x_empty,
#       write.file = FALSE
#     )
#   )
#   
# })
# 
# # Test usage with x inputs (half empty x, no templates but 2 data.table) ------
# 
# testthat::test_that('Test usage with x inputs (half empty x, no templates but 2 data.table)', {
#   
#   # Create x with no template but 2 data.table
#   
#   x_half_empty <- read_files(
#     path = system.file(
#       '/examples', 
#       package = 'EMLassemblyline'
#     ),
#     data.path = system.file(
#       '/examples/data',
#       package = 'EMLassemblyline'
#     ),
#     data.table = c(
#       'decomp.csv',
#       'nitrogen.csv'
#     )
#   )
#   
#   # First import results in messages
#   
#   expect_message(
#     import_templates(
#       license = 'CC0',
#       x = x_half_empty,
#       write.file = FALSE
#     )
#   )
#   
#   x_half_empty <- suppressMessages(
#     import_templates(
#       path = system.file(
#         '/examples',
#         package = 'EMLassemblyline'
#       ),
#       license = 'CC0',
#       x = x_half_empty,
#       write.file = FALSE
#     )
#   )
#   
#   # All arguments are supported:
#   # - /x/template/* is populated with template
#   # - content and paths are present
#   # - data.table is present with content and path completed
#   
#   # For each template ...
#   
#   for (i in 1:length(x_half_empty$template)){
#     
#     use_i <- c(
#       stringr::str_detect(
#         string = names(x_half_empty$template[i]), 
#         pattern = 'attributes_.*.txt'
#       ),
#       stringr::str_detect(
#         string = names(x_half_empty$template[i]), 
#         pattern = 'catvars_.*.txt'
#       ),
#       (names(x_half_empty$template[i]) %in% 
#          c('taxonomicCoverage.xml',
#            'geographic_coverage.txt'))
#     )
#     
#     # If template is not attribute, catvars, taxonomicCoverage, or 
#     # geographic_coverage ...
#     
#     if (!any(use_i)){
#       
#       expect_equal(
#         any(names(x_half_empty$template[i]) %in% attr.templates$regexpr),
#         TRUE
#       )
#       
#       expect_equal(
#         all(!is.na(x_half_empty$template[[i]]$content)),
#         TRUE
#       )
#       
#       expect_equal(
#         any(!is.na(x_half_empty$template[[i]]$path)),
#         TRUE
#       )
#       
#       # If template is attributes ...  
#       
#     } else if (stringr::str_detect(
#       string = names(x_half_empty$template[i]), 
#       pattern = 'attributes_.*.txt'
#     )){
#       
#       fregexpr <- stringr::str_extract(
#         string = names(x_half_empty$template[i]),
#         pattern = '(?<=^attributes_)[:graph:]*(?=.txt$)'
#       )
#       
#       expect_equal(
#         any(
#           stringr::str_detect(
#             string = names(x_half_empty$data.table),
#             pattern = fregexpr
#           )
#         ),
#         TRUE
#       )
#       
#       expect_equal(
#         all(!is.na(x_half_empty$template[[i]]$content)),
#         TRUE
#       )
#       
#       expect_equal(
#         any(!is.na(x_half_empty$template[[i]]$path)),
#         TRUE
#       )
#       
#       # If template is taxonomicCoverage or geographic coverage ...
#       
#     } else {
#       
#       expect_equal(
#         is.na(x_half_empty$template[[i]]$content),
#         TRUE
#       )
#       
#       expect_equal(
#         is.na(x_half_empty$template[[i]]$path),
#         TRUE
#       )
#       
#     }
#     
#     
#   }
#   
#   # Second import notifies of existing content
#   
#   expect_message(
#     import_templates(
#       license = 'CC0',
#       x = x_half_empty,
#       write.file = FALSE
#     )
#   )
#   
# })
# 
# # Test usage with x inputs (full x, all templates and 2 data.table) -----------
# 
# testthat::test_that('Test usage with x inputs (full x, all templates and 2 data.table)', {
#   
#   # Create x with full templates and 2 data.table
#   
#   x_full <- read_files(
#     path = system.file(
#       '/examples/templates', 
#       package = 'EMLassemblyline'
#     ),
#     data.path = system.file(
#       '/examples/data',
#       package = 'EMLassemblyline'
#     ),
#     data.table = c(
#       'decomp.csv',
#       'nitrogen.csv'
#     )
#   )
#   
#   # First import results in messages
#   
#   expect_message(
#     import_templates(
#       license = 'CC0',
#       x = x_full,
#       write.file = FALSE
#     )
#   )
#   
#   x_full <- suppressMessages(
#     import_templates(
#       path = system.file(
#         '/examples/templates',
#         package = 'EMLassemblyline'
#       ),
#       license = 'CC0',
#       x = x_full,
#       write.file = FALSE
#     )
#   )
#   
#   # All arguments are supported:
#   # - /x/template/* is populated with template
#   # - content and paths are present
#   # - data.table is present with content and path completed
#   
#   # For each template ...
#   
#   for (i in 1:length(x_full$template)){
#     
#     # use_i <- c(
#     #   stringr::str_detect(
#     #     string = names(x_full$template[i]), 
#     #     pattern = 'attributes_.*.txt'
#     #   ),
#     #   stringr::str_detect(
#     #     string = names(x_full$template[i]), 
#     #     pattern = 'catvars_.*.txt'
#     #   )
#     # )
#     
#     # Content is not NA
#     
#     expect_equal(
#       all(!is.na(x_full$template[[i]]$content)),
#       TRUE
#     )
#     
#     # Path is not NA
#     
#     expect_equal(
#       any(!is.na(x_full$template[[i]]$path)),
#       TRUE
#     )
#     
#     # If template is attributes_*.txt ...
#     
#     if (stringr::str_detect(
#       string = names(x_full$template[i]), 
#       pattern = 'attributes_.*.txt'
#     )){
#       
#       # Template name should have a matching data.table
#       
#       fregexpr <- stringr::str_extract(
#         string = names(x_full$template[i]),
#         pattern = '(?<=^attributes_)[:graph:]*(?=.txt$)'
#       )
#       
#       expect_equal(
#         any(
#           stringr::str_detect(
#             string = names(x_full$data.table),
#             pattern = fregexpr
#           )
#         ),
#         TRUE
#       )
#       
#       # If template is catvars_*.txt
#       
#     } else if (stringr::str_detect(
#       string = names(x_full$template[i]), 
#       pattern = 'catvars_.*.txt'
#     )){
#       
#       # Template name should have a matching data.table
#       
#       fregexpr <- stringr::str_extract(
#         string = names(x_full$template[i]),
#         pattern = '(?<=^catvars_)[:graph:]*(?=.txt$)'
#       )
#       
#       expect_equal(
#         any(
#           stringr::str_detect(
#             string = names(x_full$data.table),
#             pattern = fregexpr
#           )
#         ),
#         TRUE
#       )
#       
#     }
#     
#     
#   }
#   
#   # Second import notifies of existing content
#   
#   expect_message(
#     import_templates(
#       license = 'CC0',
#       x = x_full,
#       write.file = FALSE
#     )
#   )
#   
# })
