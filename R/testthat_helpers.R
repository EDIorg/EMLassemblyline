#' Copy test package files to directory path
#'
#' @param path (character) Directory path where test files will be copied.
#'
#' @return (character) Full paths of files copied to \code{path}.
#' 
#' @keywords internal
#' 
#' @examples 
#' \dontrun{
#' testdir <- paste0(tempdir(), "/pkg")
#' pkg_files <- copy_test_package(testdir)
#' pkg_files
#' unlink(testdir, recursive = TRUE, force = TRUE)
#' }
#' 
copy_test_package <- function(path) {
  unlink(path, recursive = TRUE, force = TRUE)
  dir.create(path)
  invisible(
    file.copy(
      from = list.files(
        path = system.file(
          '/examples/pkg_260/data_objects',
          package = 'EMLassemblyline'
        ),
        full.names = TRUE
      ), 
      to = path
    )
  )
  invisible(
    file.copy(
      from = list.files(
        path = system.file(
          '/examples/pkg_260/metadata_templates',
          package = 'EMLassemblyline'
        ),
        full.names = TRUE
      ), 
      to = path
    )
  )
  dir_files <- dir(path, full.names = TRUE)
  return(dir_files)
}
